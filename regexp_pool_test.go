package regexp2

import (
	"fmt"
	"runtime"
	"strings"
	"sync"
	"sync/atomic"
	"testing"
)

func skipIfAllocsUnreliable(t *testing.T) {
	t.Helper()
	if raceDetectorEnabled {
		t.Skip("AllocsPerRun is inflated by the race detector")
	}
}

func TestFindRunesMatchNoMatchAllocs(t *testing.T) {
	skipIfAllocsUnreliable(t)
	input := []rune(strings.Repeat("x := someIdentifier + 42 // comment\n", 100))
	re := MustCompile(`^(?:func\b)`)
	for _, startingAt := range []bool{false, true} {
		name := "FindRunesMatch"
		if startingAt {
			name = "FindRunesMatchStartingAt"
		}
		t.Run(name, func(t *testing.T) {
			allocs := testing.AllocsPerRun(200, func() {
				var m *Match
				var err error
				if startingAt {
					m, err = re.FindRunesMatchStartingAt(input, 0)
				} else {
					m, err = re.FindRunesMatch(input)
				}
				if err != nil || m != nil {
					t.Fatalf("match = %v, %v; want nil, nil", m, err)
				}
			})
			if allocs != 0 {
				t.Fatalf("allocs/op = %v, want 0", allocs)
			}
		})
	}
}

func TestStringOperationsNoMatchAllocs(t *testing.T) {
	skipIfAllocsUnreliable(t)
	for _, tc := range []struct {
		name, pattern, input string
		options              RegexOptions
	}{
		{"anchored", `^func\b`, strings.Repeat("x := someIdentifier + 42 // comment\n", 100), None},
		{"rejected_candidate", `func\b`, "funcx" + strings.Repeat("x", 3400), None},
		{"right_to_left", `func\b`, strings.Repeat("x", 3400), RightToLeft},
	} {
		t.Run(tc.name, func(t *testing.T) {
			re := MustCompile(tc.pattern, tc.options)
			for _, api := range []struct {
				name string
				call func(*testing.T)
				want float64
			}{
				{"FindStringMatch", func(t *testing.T) {
					if m, err := re.FindStringMatch(tc.input); m != nil || err != nil {
						t.Fatalf("match = %v, %v; want nil, nil", m, err)
					}
				}, 0},
				{"FindStringMatchStartingAt", func(t *testing.T) {
					startAt := 0
					if re.RightToLeft() {
						startAt = len(tc.input)
					}
					if m, err := re.FindStringMatchStartingAt(tc.input, startAt); m != nil || err != nil {
						t.Fatalf("match = %v, %v; want nil, nil", m, err)
					}
				}, 0},
				{"Replace", func(t *testing.T) {
					if got, err := re.Replace(tc.input, "replacement", -1, -1); got != tc.input || err != nil {
						t.Fatalf("Replace = %q, %v; want original input, nil", got, err)
					}
				}, 0},
				{"ReplaceFunc", func(t *testing.T) {
					got, err := re.ReplaceFunc(tc.input, func(Match) string {
						t.Fatal("unexpected match")
						return ""
					}, -1, -1)
					if got != tc.input || err != nil {
						t.Fatalf("ReplaceFunc = %q, %v; want original input, nil", got, err)
					}
				}, 0},
				{"Split", func(t *testing.T) {
					if got, err := re.Split(tc.input, -1); err != nil || len(got) != 1 || got[0] != tc.input {
						t.Fatalf("Split = %q, %v; want original input, nil", got, err)
					}
				}, 1}, // The returned []string still needs storage.
				{"FindAllStringIndex", func(t *testing.T) {
					if got, err := re.FindAllStringIndex(tc.input, 100); len(got) != 0 || err != nil {
						t.Fatalf("indexes = %v, %v; want no matches, nil", got, err)
					}
				}, 0},
			} {
				t.Run(api.name, func(t *testing.T) {
					if allocs := testing.AllocsPerRun(200, func() { api.call(t) }); allocs != api.want {
						t.Fatalf("allocs/op = %v, want %v", allocs, api.want)
					}
				})
			}
		})
	}
}

func TestFindAllRunesIndexNoMatchAllocs(t *testing.T) {
	skipIfAllocsUnreliable(t)
	re := MustCompile(`^func\b`)
	input := []rune("x := someIdentifier + 42")
	allocs := testing.AllocsPerRun(200, func() {
		if got, err := re.FindAllRunesIndex(input, 100); len(got) != 0 || err != nil {
			t.Fatalf("indexes = %v, %v; want no matches, nil", got, err)
		}
	})
	if allocs != 0 {
		t.Fatalf("allocs/op = %v, want 0", allocs)
	}
}

func TestFindStringMatchPoolingDoesNotIncreaseAllocs(t *testing.T) {
	skipIfAllocsUnreliable(t)
	input := "needle" + strings.Repeat("x", 64<<10)
	measure := func(re *Regexp) float64 {
		return testing.AllocsPerRun(200, func() {
			m, err := re.FindStringMatch(input)
			if err != nil || m == nil || m.String() != "needle" || string(m.Runes()) != "needle" {
				t.Fatalf("match = %v, %v; want needle, nil", m, err)
			}
		})
	}
	pooled := measure(MustCompile(`needle`))
	unpooled := measure(MustCompile(`needle`, OptionMaxCachedRuneBufferLength(0)))
	if pooled > unpooled {
		t.Fatalf("pooled allocs/op = %v, unpooled = %v; pooling should not add allocations for matches", pooled, unpooled)
	}
}

func TestReturnedMatchSurvivesPooledReuse(t *testing.T) {
	for _, pattern := range []string{`needle\d+`, `(?<n>needle)\d+`} {
		t.Run(pattern, func(t *testing.T) {
			re := MustCompile(pattern)
			heldInput := strings.Repeat("漢", 80) + "needle42"
			held, err := re.FindStringMatch(heldInput)
			if err != nil || held == nil {
				t.Fatalf("held match = %v, %v", held, err)
			}
			wantString := held.String()
			wantRunes := string(held.Runes())
			wantRuneIndex := held.RuneIndex
			wantRuneLength := held.RuneLength
			wantByteIndex, wantByteLength := held.ByteRange()
			group := held.GroupByNumber(held.GroupCount() - 1)
			wantGroup := group.String()

			// These paths return pooled rune/replace buffers. If FindStringMatch had
			// borrowed one, later reuse would overwrite the held match.
			for i := 0; i < 200; i++ {
				other := strings.Repeat("z", 1500) + fmt.Sprintf("needle%d", i)
				// Alternate misses and hits with the same buffer size as the held input.
				miss := strings.Repeat("z", len(heldInput)-8) + "needle!!"
				if m, err := re.FindStringMatch(miss); m != nil || err != nil {
					t.Fatalf("miss = %v, %v; want nil, nil", m, err)
				}
				if _, err := re.FindStringMatch(strings.Repeat("z", len(heldInput)-8) + "needle99"); err != nil {
					t.Fatal(err)
				}
				if ok, err := re.MatchString(other); !ok || err != nil {
					t.Fatalf("MatchString: %v %v", ok, err)
				}
				if _, err := re.FindAllStringIndex(other, -1); err != nil {
					t.Fatal(err)
				}
				if _, err := re.Replace(other, "[$1]", -1, -1); err != nil {
					t.Fatal(err)
				}
			}

			if got := held.String(); got != wantString {
				t.Fatalf("String mutated to %q, want %q", got, wantString)
			}
			if got := string(held.Runes()); got != wantRunes {
				t.Fatalf("Runes mutated to %q, want %q", got, wantRunes)
			}
			if held.RuneIndex != wantRuneIndex || held.RuneLength != wantRuneLength {
				t.Fatalf("rune span mutated to (%d,%d)", held.RuneIndex, held.RuneLength)
			}
			gotByteIndex, gotByteLength := held.ByteRange()
			if gotByteIndex != wantByteIndex || gotByteLength != wantByteLength {
				t.Fatalf("ByteRange mutated to (%d,%d)", gotByteIndex, gotByteLength)
			}
			if got := held.GroupByNumber(held.GroupCount() - 1).String(); got != wantGroup {
				t.Fatalf("group mutated to %q, want %q", got, wantGroup)
			}
			if next, err := re.FindNextMatch(held); next != nil || err != nil {
				t.Fatalf("FindNextMatch = %v, %v; want nil, nil", next, err)
			}
		})
	}
}

func TestCaptureStringNoAlloc(t *testing.T) {
	skipIfAllocsUnreliable(t)
	re := MustCompile(`needle`)
	input := strings.Repeat("x", 80) + "needle"
	m, err := re.FindStringMatch(input)
	if err != nil || m == nil {
		t.Fatalf("match = %v, %v", m, err)
	}
	if got := m.String(); got != "needle" {
		t.Fatal(got)
	}

	allocs := testing.AllocsPerRun(200, func() {
		if m.String() != "needle" {
			panic(m.String())
		}
		if _, n := m.ByteRange(); n != 6 {
			panic(n)
		}
	})
	if allocs != 0 {
		t.Fatalf("String/ByteRange allocs/op = %v, want 0", allocs)
	}
}

func TestFindNextMatchPreservesEarlierCaptures(t *testing.T) {
	re := MustCompile(`a|b`)
	first, err := re.FindStringMatch("a b")
	if err != nil || first == nil || first.String() != "a" {
		t.Fatalf("first = %v, %v; want a, nil", first, err)
	}
	captures := first.Captures
	second, err := re.FindNextMatch(first)
	if err != nil || second == nil || second.String() != "b" {
		t.Fatalf("second = %v, %v; want b, nil", second, err)
	}
	if first.String() != "a" || len(captures) != 1 || captures[0].String() != "a" {
		t.Fatalf("earlier match or capture changed after FindNextMatch: %v, %v", first, captures)
	}
}

func TestMatchStringPoolingReducesAllocs(t *testing.T) {
	skipIfAllocsUnreliable(t)
	// Match at the start so the whole haystack is decoded and eligible for the rune pool.
	input := "needle" + strings.Repeat("z", 3000)
	pooled := MustCompile(`needle`)
	unpooled := MustCompile(`needle`, OptionMaxCachedRuneBufferLength(0))

	for i := 0; i < 20; i++ {
		if ok, err := pooled.MatchString(input); !ok || err != nil {
			t.Fatal(ok, err)
		}
		if ok, err := unpooled.MatchString(input); !ok || err != nil {
			t.Fatal(ok, err)
		}
	}

	p := testing.AllocsPerRun(100, func() {
		if ok, err := pooled.MatchString(input); !ok || err != nil {
			panic(err)
		}
	})
	u := testing.AllocsPerRun(100, func() {
		if ok, err := unpooled.MatchString(input); !ok || err != nil {
			panic(err)
		}
	})
	if p >= u {
		t.Fatalf("pooled MatchString allocs/op = %.2f, unpooled = %.2f; pooling should allocate less", p, u)
	}
}

func TestFindAllAndReplacePoolingReducesAllocs(t *testing.T) {
	skipIfAllocsUnreliable(t)
	// One early match keeps FindAll/Replace on a large decode + replace buffer.
	input := "needle" + strings.Repeat("z", 4000)
	pooled := MustCompile(`needle`)
	unpooled := MustCompile(`needle`, OptionMaxCachedRuneBufferLength(0), OptionMaxCachedReplaceBufferLength(0))

	for i := 0; i < 10; i++ {
		if _, err := pooled.FindAllStringIndex(input, -1); err != nil {
			t.Fatal(err)
		}
		if _, err := unpooled.FindAllStringIndex(input, -1); err != nil {
			t.Fatal(err)
		}
		if _, err := pooled.Replace(input, "X", -1, -1); err != nil {
			t.Fatal(err)
		}
		if _, err := unpooled.Replace(input, "X", -1, -1); err != nil {
			t.Fatal(err)
		}
	}

	pIdx := testing.AllocsPerRun(50, func() {
		if _, err := pooled.FindAllStringIndex(input, -1); err != nil {
			panic(err)
		}
	})
	uIdx := testing.AllocsPerRun(50, func() {
		if _, err := unpooled.FindAllStringIndex(input, -1); err != nil {
			panic(err)
		}
	})
	if pIdx >= uIdx {
		t.Fatalf("pooled FindAllStringIndex allocs/op = %.2f, unpooled = %.2f", pIdx, uIdx)
	}

	pRep := testing.AllocsPerRun(50, func() {
		if _, err := pooled.Replace(input, "X", -1, -1); err != nil {
			panic(err)
		}
	})
	uRep := testing.AllocsPerRun(50, func() {
		if _, err := unpooled.Replace(input, "X", -1, -1); err != nil {
			panic(err)
		}
	})
	if pRep >= uRep {
		t.Fatalf("pooled Replace allocs/op = %.2f, unpooled = %.2f", pRep, uRep)
	}
}

func TestMatchStringDoesNotLeak(t *testing.T) {
	re := MustCompile(`needle`)
	input := strings.Repeat("z", 8<<10) + "needle"
	for i := 0; i < 50; i++ {
		if ok, err := re.MatchString(input); !ok || err != nil {
			t.Fatal(ok, err)
		}
	}

	before := heapAlloc()
	const n = 2000
	for i := 0; i < n; i++ {
		if ok, err := re.MatchString(input); !ok || err != nil {
			t.Fatal(ok, err)
		}
	}
	after := heapAlloc()
	// A retained decode buffer per call would be tens of MB. Allow heap noise.
	const slack = 8 << 20
	if after > before+slack {
		t.Fatalf("heap grew from %d to %d after %d MatchString calls (delta %d)", before, after, n, after-before)
	}
}

func TestReplaceCacheDoesNotLeakEntries(t *testing.T) {
	re := MustCompile(`needle`, OptionMaxCachedReplacerDataEntries(2), OptionMaxCachedReplacerDataBytes(-1))
	input := strings.Repeat("x", 200) + "needle" + strings.Repeat("y", 200)
	for i := 0; i < 20; i++ {
		if _, err := re.Replace(input, fmt.Sprintf("<%d>", i), -1, -1); err != nil {
			t.Fatal(err)
		}
	}

	before := heapAlloc()
	const n = 400
	for i := 0; i < n; i++ {
		if _, err := re.Replace(input, fmt.Sprintf("[%d]", i), -1, -1); err != nil {
			t.Fatal(err)
		}
	}
	after := heapAlloc()
	const slack = 8 << 20
	if after > before+slack {
		t.Fatalf("heap grew from %d to %d after %d unique Replace patterns (delta %d)", before, after, n, after-before)
	}
}

func TestPoolOptionsDoNotChangeResults(t *testing.T) {
	pattern := `(?<w>\w+)-(\d+)`
	input := strings.Repeat("漢", 8) + "token-42 and more token-7"
	replacement := "[$1=$2]"

	baseline := MustCompile(pattern, OptionMaxCachedRuneBufferLength(0), OptionMaxCachedReplaceBufferLength(0), OptionMaxCachedReplacerDataEntries(0))
	wantMatch, err := baseline.FindStringMatch(input)
	if err != nil || wantMatch == nil {
		t.Fatalf("baseline match = %v, %v", wantMatch, err)
	}
	wantIndexes, err := baseline.FindAllStringIndex(input, -1)
	if err != nil {
		t.Fatal(err)
	}
	wantRepl, err := baseline.Replace(input, replacement, -1, -1)
	if err != nil {
		t.Fatal(err)
	}
	wantSplit, err := baseline.Split(input, -1)
	if err != nil {
		t.Fatal(err)
	}

	configs := []struct {
		name string
		opts []CompileOption
	}{
		{name: "defaults"},
		{name: "no rune pool", opts: []CompileOption{OptionMaxCachedRuneBufferLength(0)}},
		{name: "unbounded rune pool", opts: []CompileOption{OptionMaxCachedRuneBufferLength(-1)}},
		{name: "no replace buffer pool", opts: []CompileOption{OptionMaxCachedReplaceBufferLength(0)}},
		{name: "tiny replacer cache", opts: []CompileOption{OptionMaxCachedReplacerDataEntries(1), OptionMaxCachedReplacerDataBytes(-1)}},
		{name: "no replacer cache", opts: []CompileOption{OptionMaxCachedReplacerDataEntries(0)}},
		{name: "all caches off", opts: []CompileOption{
			OptionMaxCachedRuneBufferLength(0),
			OptionMaxCachedReplaceBufferLength(0),
			OptionMaxCachedReplacerDataEntries(0),
		}},
	}

	for _, cfg := range configs {
		t.Run(cfg.name, func(t *testing.T) {
			re := MustCompile(pattern, cfg.opts...)
			m, err := re.FindStringMatch(input)
			if err != nil || m == nil {
				t.Fatalf("FindStringMatch = %v, %v", m, err)
			}
			if m.String() != wantMatch.String() || m.RuneIndex != wantMatch.RuneIndex || m.RuneLength != wantMatch.RuneLength {
				t.Fatalf("match = %q @%d, want %q @%d", m.String(), m.RuneIndex, wantMatch.String(), wantMatch.RuneIndex)
			}
			gotIdx, gotLen := m.ByteRange()
			wantIdx, wantLen := wantMatch.ByteRange()
			if gotIdx != wantIdx || gotLen != wantLen {
				t.Fatalf("ByteRange = (%d,%d), want (%d,%d)", gotIdx, gotLen, wantIdx, wantLen)
			}

			gotIndexes, err := re.FindAllStringIndex(input, -1)
			if err != nil {
				t.Fatal(err)
			}
			if !sameStringIndexes(gotIndexes, wantIndexes) {
				t.Fatalf("FindAllStringIndex = %v, want %v", gotIndexes, wantIndexes)
			}

			gotRepl, err := re.Replace(input, replacement, -1, -1)
			if err != nil {
				t.Fatal(err)
			}
			if gotRepl != wantRepl {
				t.Fatalf("Replace = %q, want %q", gotRepl, wantRepl)
			}

			gotSplit, err := re.Split(input, -1)
			if err != nil {
				t.Fatal(err)
			}
			if len(gotSplit) != len(wantSplit) {
				t.Fatalf("Split = %#v, want %#v", gotSplit, wantSplit)
			}
			for i := range wantSplit {
				if gotSplit[i] != wantSplit[i] {
					t.Fatalf("Split = %#v, want %#v", gotSplit, wantSplit)
				}
			}
		})
	}
}

func TestCapture_ConcurrentReads(t *testing.T) {
	re := MustCompile(`(?<a>nee)(?<b>dle)`)
	input := strings.Repeat("é", 40) + "needle"
	m, err := re.FindStringMatch(input)
	if err != nil {
		t.Fatalf("Unexpected match error: %v", err)
	}
	if m == nil {
		t.Fatal("Should have matched")
	}
	// Groups() is populated once so later concurrent reads only hit initialized data.
	_ = m.Groups()

	var wg sync.WaitGroup
	for i := 0; i < 8; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for j := 0; j < 500; j++ {
				if got := m.String(); got != "needle" {
					t.Errorf("String = %q", got)
					return
				}
				if got := string(m.Runes()); got != "needle" {
					t.Errorf("Runes = %q", got)
					return
				}
				idx, ln := m.ByteRange()
				if input[idx:idx+ln] != "needle" {
					t.Errorf("ByteRange = %q", input[idx:idx+ln])
					return
				}
				if g := m.GroupByName("a"); g == nil || g.String() != "nee" {
					t.Errorf("group a = %v", g)
					return
				}
				if g := m.GroupByName("b"); g == nil || g.String() != "dle" {
					t.Errorf("group b = %v", g)
					return
				}
			}
		}()
	}
	wg.Wait()
}

func TestRegexpConcurrentPoolUse(t *testing.T) {
	type namedRE struct {
		name string
		re   *Regexp
	}
	regexps := []namedRE{
		{name: "literal", re: MustCompile(`needle`)},
		{name: "groups", re: MustCompile(`(?<w>\w+)-(\d+)`)},
		{name: "boundary", re: MustCompile(`\bcat\b`)},
		{name: "lookbehind", re: MustCompile(`(?<=x)foo`)},
		{name: "alt", re: MustCompile(`apple|tiger`)},
		{name: "rtl", re: MustCompile(`ab`, RightToLeft)},
		{name: "tinyCache", re: MustCompile(`a+`, OptionMaxCachedReplacerDataEntries(3), OptionMaxCachedReplacerDataBytes(-1))},
	}

	inputs := []string{
		strings.Repeat("z", 200) + "needle42 cat xfoo apple ab aa",
		strings.Repeat("漢", 30) + "token-7 tiger needle",
		"xxfoo xxcat xxapple xxab xxaaa",
		"needle",
		"",
	}
	replacements := []string{"X", "[$1]", "$`", "$'", "$_", "$&"}

	const goroutines = 12
	const iters = 80
	var wg sync.WaitGroup
	var failures atomic.Int64

	for g := 0; g < goroutines; g++ {
		wg.Add(1)
		go func(id int) {
			defer wg.Done()
			for i := 0; i < iters; i++ {
				nr := regexps[(id+i)%len(regexps)]
				input := inputs[(id*3+i)%len(inputs)]
				re := nr.re

				ok, err := re.MatchString(input)
				if err != nil {
					t.Errorf("%s MatchString: %v", nr.name, err)
					failures.Add(1)
					return
				}

				m, err := re.FindStringMatch(input)
				if err != nil {
					t.Errorf("%s FindStringMatch: %v", nr.name, err)
					failures.Add(1)
					return
				}
				if (m != nil) != ok {
					t.Errorf("%s MatchString=%v but FindStringMatch=%v", nr.name, ok, m != nil)
					failures.Add(1)
					return
				}
				if m != nil {
					s := m.String()
					if string(m.Runes()) != s {
						t.Errorf("%s Runes %q != String %q", nr.name, m.Runes(), s)
						failures.Add(1)
						return
					}
					idx, ln := m.ByteRange()
					if idx < 0 || idx+ln > len(input) || input[idx:idx+ln] != s {
						t.Errorf("%s ByteRange (%d,%d) = %q, String=%q", nr.name, idx, ln, safeSlice(input, idx, ln), s)
						failures.Add(1)
						return
					}
					if _, err := re.FindNextMatch(m); err != nil {
						t.Errorf("%s FindNextMatch: %v", nr.name, err)
						failures.Add(1)
						return
					}
					_ = m.Groups()
				}

				if _, err := re.FindAllStringIndex(input, -1); err != nil {
					t.Errorf("%s FindAllStringIndex: %v", nr.name, err)
					failures.Add(1)
					return
				}
				if _, err := re.MatchRunes([]rune(input)); err != nil {
					t.Errorf("%s MatchRunes: %v", nr.name, err)
					failures.Add(1)
					return
				}

				repl := replacements[(id+i)%len(replacements)]
				if _, err := re.Replace(input, repl, -1, -1); err != nil {
					// Some replacement patterns are invalid for patterns without that group.
					if !strings.Contains(err.Error(), "group") && !strings.Contains(err.Error(), "unrecognized") {
						t.Errorf("%s Replace(%q): %v", nr.name, repl, err)
						failures.Add(1)
						return
					}
				}
				if _, err := re.ReplaceFunc(input, func(m Match) string { return m.String() }, -1, -1); err != nil {
					t.Errorf("%s ReplaceFunc: %v", nr.name, err)
					failures.Add(1)
					return
				}
				if _, err := re.Split(input, -1); err != nil {
					t.Errorf("%s Split: %v", nr.name, err)
					failures.Add(1)
					return
				}
			}
		}(g)
	}
	wg.Wait()
	if failures.Load() > 0 {
		t.Fatalf("%d concurrent workers reported failures", failures.Load())
	}
}

func TestConcurrentReplaceCache(t *testing.T) {
	cached := MustCompile(`(\w+)`, OptionMaxCachedReplacerDataEntries(4), OptionMaxCachedReplacerDataBytes(-1))
	uncached := MustCompile(`(\w+)`, OptionMaxCachedReplacerDataEntries(0))
	input := strings.Repeat("漢", 5) + " one two three four five"
	replacements := []string{"[$1]", "<$1>", "$1!", "*$1*", "x", "yy", "$`", "$&"}

	oracles := make([]string, len(replacements))
	for i, repl := range replacements {
		got, err := uncached.Replace(input, repl, -1, -1)
		if err != nil {
			t.Fatal(err)
		}
		oracles[i] = got
	}

	var wg sync.WaitGroup
	for g := 0; g < 10; g++ {
		wg.Add(1)
		go func(id int) {
			defer wg.Done()
			for i := 0; i < 100; i++ {
				idx := (id + i) % len(replacements)
				got, err := cached.Replace(input, replacements[idx], -1, -1)
				if err != nil {
					t.Errorf("Replace(%q): %v", replacements[idx], err)
					return
				}
				if got != oracles[idx] {
					t.Errorf("Replace(%q) = %q, want %q", replacements[idx], got, oracles[idx])
					return
				}
			}
		}(g)
	}
	wg.Wait()
}

func TestConcurrentHeapStableAfterBurst(t *testing.T) {
	re := MustCompile(`needle|tiger`)
	input := strings.Repeat("z", 4096) + "needle"
	var wg sync.WaitGroup
	for i := 0; i < 8; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for j := 0; j < 50; j++ {
				_, _ = re.MatchString(input)
			}
		}()
	}
	wg.Wait()

	before := heapAlloc()
	for i := 0; i < 8; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for j := 0; j < 200; j++ {
				if ok, err := re.MatchString(input); !ok || err != nil {
					t.Errorf("MatchString = %v, %v", ok, err)
					return
				}
			}
		}()
	}
	wg.Wait()
	after := heapAlloc()
	const slack = 8 << 20
	if after > before+slack {
		t.Fatalf("heap grew from %d to %d after concurrent MatchString burst (delta %d)", before, after, after-before)
	}
}

func heapAlloc() uint64 {
	runtime.GC()
	runtime.GC()
	var ms runtime.MemStats
	runtime.ReadMemStats(&ms)
	return ms.HeapAlloc
}

func safeSlice(s string, idx, ln int) string {
	if idx < 0 || ln < 0 || idx+ln > len(s) {
		return fmt.Sprintf("<invalid %d,%d>", idx, ln)
	}
	return s[idx : idx+ln]
}
