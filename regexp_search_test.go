package regexp2

import (
	"math/rand"
	"strconv"
	"strings"
	"sync"
	"testing"

	"github.com/dlclark/regexp2/v2/syntax"
)

func TestAlternationWithNegatedStartingCharacter(t *testing.T) {
	// Issue #119: combining a literal start with a negated start must not
	// exclude the literal from the possible match positions.
	for _, tc := range []struct {
		pattern, input, want string
		index                int // -1 means no match.
	}{
		{`a.|.b`, "ab", "ab", 0},
		{`a.|.b`, "ac", "ac", 0},
		{`a.|.b`, "cb", "cb", 0},
		{`a.|.b`, "界aé", "aé", 1},
		{`a.|.b`, "a\n", "", -1},
		{`a.|.b`, "\nb", "", -1},
		{`.b|a.`, "ac", "ac", 0},
		{`a[^x]|[^x]b`, "ab", "ab", 0},
		{`a[^x]|[^x]b`, "xb", "", -1},
		{`x[^x]|[^x]b`, "xb", "xb", 0},
		{`\d.|.b`, "1c", "1c", 0},
		{`a[^\x00]|[^\x00]b`, "ab", "ab", 0},
		{`a[^\x{10fffe}]|[^\x{10fffe}]b`, "a\U0010ffff", "a\U0010ffff", 0},
		{`a[^\x{10ffff}]|[^\x{10ffff}]b`, "ab", "ab", 0},
		{`a[^\x{10ffff}]|[^\x{10ffff}]b`, "\U0010ffffb", "", -1},
		{`a.|.+b`, "ac", "ac", 0},
		{`a.|.+?b`, "ac", "ac", 0},
		{`a.|(?>.+)b`, "ac", "ac", 0},
		{`.b`, "cb", "cb", 0},
		{`[^x]b`, "ab", "ab", 0},
	} {
		t.Run(tc.pattern+"/"+tc.input, func(t *testing.T) {
			re := MustCompile(tc.pattern)
			wantMatched := tc.index >= 0
			if got, err := re.MatchString(tc.input); err != nil || got != wantMatched {
				t.Errorf("MatchString(%q) = %v, %v; want %v, nil", tc.input, got, err, wantMatched)
			}
			if got, err := re.MatchRunes([]rune(tc.input)); err != nil || got != wantMatched {
				t.Errorf("MatchRunes(%q) = %v, %v; want %v, nil", tc.input, got, err, wantMatched)
			}
			for _, find := range []struct {
				name string
				find func() (*Match, error)
			}{
				{"FindStringMatch", func() (*Match, error) { return re.FindStringMatch(tc.input) }},
				{"FindRunesMatch", func() (*Match, error) { return re.FindRunesMatch([]rune(tc.input)) }},
			} {
				m, err := find.find()
				if err != nil {
					t.Errorf("%s(%q): %v", find.name, tc.input, err)
				} else if (m != nil) != wantMatched {
					t.Errorf("%s(%q) = %v; want matched=%v", find.name, tc.input, m, wantMatched)
				} else if m != nil && (m.String() != tc.want || m.RuneIndex != tc.index) {
					t.Errorf("%s(%q) = %q at %d; want %q at %d", find.name, tc.input, m.String(), m.RuneIndex, tc.want, tc.index)
				}
			}
		})
	}
}

// Retain the original interpreter finder as an independent reference for
// optimizations that skip possible starting positions. Disable it on both
// programs because index and boolean APIs use the capture-elided program.
func compileWithoutSearchOptimizations(pattern string, options ...CompileOption) *Regexp {
	re := MustCompile(pattern, options...)
	re.stringPrefixFilter = nil
	re.prefixSearch = nil
	re.code.FindOptimizations = nil
	if re.quickCode != nil {
		re.quickCode.FindOptimizations = nil
	}
	return re
}

func TestMultiplePrefixSearchEquivalent(t *testing.T) {
	for _, pattern := range []string{
		`(aaba|aaca|bada)([!?])`,
		`(aaba)!|(?<pick>aaca)\?`,
		`(?=(aaba|aaca|bada))....`,
		`(?:aaba|aaca|bada)(?<=a)`,
		`apple|tiger`, `apple|apply|tiger`,
		`(?i:aaba|aaca|bada)`, `aaba|界界`, `aaba|bad`,
	} {
		for _, option := range []RegexOptions{None, RightToLeft} {
			re := MustCompile(pattern, option)
			reference := compileWithoutSearchOptimizations(pattern, option)
			for _, input := range []string{
				"", "aaca?", "bada!", "apple tiger", "界界aaba",
				strings.Repeat("a", 128),
				strings.Repeat("a", 128) + "aaca?aaba!bada!",
				strings.Repeat("x", 128) + "aaca?aaba!bada!",
				strings.Repeat("a", 64) + "界\xffaaca?aaba!bada!",
			} {
				got, err := re.FindAllStringIndex(input, -1)
				if err != nil {
					t.Fatal(err)
				}
				want, err := reference.FindAllStringIndex(input, -1)
				if err != nil || !sameStringIndexes(got, want) {
					t.Fatalf("%q on %q, options %v: indexes = %v, want %v, err %v", pattern, input, option, got, want, err)
				}
				m, err := re.FindStringMatch(input)
				if err != nil {
					t.Fatal(err)
				}
				w, err := reference.FindStringMatch(input)
				if err != nil {
					t.Fatal(err)
				}
				for m != nil && w != nil {
					if !corpusIntSlicesEqual(corpusMatchSubmatchIndex(m), corpusMatchSubmatchIndex(w)) {
						t.Fatalf("%q on %q, options %v: captures differ", pattern, input, option)
					}
					m, err = re.FindNextMatch(m)
					if err != nil {
						t.Fatal(err)
					}
					w, err = reference.FindNextMatch(w)
					if err != nil {
						t.Fatal(err)
					}
				}
				if (m == nil) != (w == nil) {
					t.Fatalf("%q on %q, options %v: match counts differ", pattern, input, option)
				}
			}
		}
	}
}

func TestFixedLengthEndAnchorMatches(t *testing.T) {
	for _, tc := range []struct {
		pattern, input string
		start, end     int
	}{
		{`ab$`, "xab\n", 1, 3},
		{`a\n$`, "xa\n", 1, 3},
		{`ab\n$`, "xab\n", 1, 4},    // first prefix fails, second candidate matches
		{`aa[ \n]$`, "aaa\n", 1, 4}, // first prefix passes, first execution fails
		{`a(?=\n)\n$`, "aa\n", 1, 3},
		{`(?i)ab$`, "xAB\n", 1, 3},
		{`ab$`, "xxy\n", -1, -1},
		{`(?s)..$`, "abc\n", 1, 3},
		{`[\s\S]{2}$`, "abc\n", 1, 3},
		{`ab\z`, "xab", 1, 3},
		{`ab\z`, "xab\n", -1, -1},
		{`ab$`, "ab\n\n", -1, -1},
		{`界b$`, "a界b\n", 1, 5},
	} {
		t.Run(tc.pattern+"/"+tc.input, func(t *testing.T) {
			re := MustCompile(tc.pattern)
			m, err := re.FindStringMatch(tc.input)
			if err != nil {
				t.Fatal(err)
			}
			if tc.start < 0 {
				if m != nil {
					t.Fatalf("unexpected match %q", m.String())
				}
				return
			}
			if m == nil {
				t.Fatal("missing match")
			}
			start, length := m.ByteRange()
			if start != tc.start || start+length != tc.end {
				t.Fatalf("match = [%d,%d), want [%d,%d)", start, start+length, tc.start, tc.end)
			}
		})
	}
}

func TestEndAnchorSearchEquivalent(t *testing.T) {
	inputs := []string{"", "a", "b", "\n", "a\n", "\n\n", "ab", "ab\n", "ab\n\n", "abc\n", "界ab\n", "\xffab\n", "xab\n", "aaa\n", "aa\n", "xAB\n", "xxy\n"}
	for _, pattern := range []string{
		`ab$`, `(a)(b)$`, `[ab][ab]$`, `[\s\S]{2}$`, `(?:ab|a\n)$`,
		`(?s)..$`, `ab\z`, `ab\Z`, `a?$`, `$`, `\n$`,
		`^ab$`, `\Aab$`, `(?m)ab$`, `(?<=界)ab$`, `(?=ab)ab$`, `\Gab$`,
		`ab\n$`, `aa[ \n]$`, `a(?=\n)\n$`, `(?i)ab$`,
	} {
		for _, option := range []RegexOptions{None, RE2, ECMAScript, RightToLeft} {
			re := MustCompile(pattern, option)
			reference := compileWithoutSearchOptimizations(pattern, option)
			for _, input := range inputs {
				got, err := re.FindAllStringIndex(input, -1)
				if err != nil {
					t.Fatal(err)
				}
				want, err := reference.FindAllStringIndex(input, -1)
				if err != nil || !sameStringIndexes(got, want) {
					t.Fatalf("%q on %q, options %v: indexes = %v, want %v, err %v", pattern, input, option, got, want, err)
				}
				// In particular, starting after the pre-newline candidate must
				// still allow a match that consumes the newline itself.
				for start := range input {
					m, err := re.FindStringMatchStartingAt(input, start)
					if err != nil {
						t.Fatal(err)
					}
					w, err := reference.FindStringMatchStartingAt(input, start)
					if err != nil || (m == nil) != (w == nil) {
						t.Fatalf("%q on %q at %d, options %v: match = %v, want %v, err %v", pattern, input, start, option, m, w, err)
					}
					if m != nil && !corpusIntSlicesEqual(corpusMatchSubmatchIndex(m), corpusMatchSubmatchIndex(w)) {
						t.Fatalf("%q on %q at %d, options %v: captures differ", pattern, input, start, option)
					}
				}
			}
		}
	}
}

func TestLargeFixedDistanceSetSearchEquivalent(t *testing.T) {
	input := []rune(strings.Repeat("AbCdEfGhIj12345678_\nαγεηικαγδεζηθ ", 4))
	input = append(input, -1, 0x110000, 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')
	for _, pattern := range []string{
		`[A-Za-z]{8,13}`, `[A-Za-z0-9]{8,13}`, `[^A-Za-z]{8,13}`,
		`[ACEGIK]{2,8}`, `[^ACEGIK]{2,8}`, `[αγεηικ]{2,8}`, `[^αγεηικ]{2,8}`,
	} {
		for _, bitmap := range []bool{true, false} {
			var options []CompileOption
			if !bitmap {
				options = append(options, OptionDisableCharClassASCIIBitmap())
			}
			re := MustCompile(pattern, options...)
			reference := compileWithoutSearchOptimizations(pattern, options...)
			got, err := re.FindAllRunesIndex(input, -1)
			if err != nil {
				t.Fatal(err)
			}
			want, err := reference.FindAllRunesIndex(input, -1)
			if err != nil || !sameStringIndexes(got, want) {
				t.Fatalf("%q, bitmap %v: indexes = %v, want %v, err %v", pattern, bitmap, got, want, err)
			}
		}
	}
}

func TestRequiredLandmarkChainSearch(t *testing.T) {
	for _, tc := range []struct {
		pattern, input, want string
	}{
		// The repetition must stop before the next landmark.
		{`\S*([ac]{1,3})[a-c]\1`, "aaa", "aaa"},
		// The repetition must stop before the whitespace that \s+ needs.
		{`\S*([a ]{1,3}\s+|x)a()`, " a a", " a a"},
		// A later alternative can end before the first one found.
		{`\S*(abc|b)c()`, "abc", "abc"},
		{`\S*(ab|[ac])b()`, "ab", "ab"},
		// Another alternative's leading whitespace can start the match.
		{`\S*(\t|\s*b)c()`, " \tbc", " \tbc"},
		{`\S*(\t|\s*b)c()`, " \t \tbc", " \t \tbc"},
	} {
		t.Run(tc.pattern, func(t *testing.T) {
			m, err := MustCompile(tc.pattern).FindStringMatch(tc.input)
			if err != nil || m == nil || m.String() != tc.want || m.RuneIndex != 0 {
				t.Fatalf("FindStringMatch(%q) = %s, %v; want %q at 0", tc.input, matchSummary(m), err, tc.want)
			}
		})
	}
}

func TestRequiredLandmarkSearchWindows(t *testing.T) {
	for _, tc := range []struct {
		pattern, input string
		want           requiredLandmarkMatch
		found          bool
	}{
		// A literal core can straddle the end of a window.
		{`[yz]*(ab|q)c()`, "zzabc", requiredLandmarkMatch{2, 4}, true},
		// Another alternative's core can follow a start that an earlier
		// alternative cannot decide yet.
		{`[yz]*(b|aba)c()`, "zzzabacbc", requiredLandmarkMatch{3, 5}, true},
		// A set core that ends at the end of a window needs the rune after it.
		{`\S*([ab]{1,3}\s+|x)c()`, "aabc", requiredLandmarkMatch{}, false},
		{`\S*([ab]{1,2}\s+|aa)c()`, "aa c", requiredLandmarkMatch{0, 2}, true},
	} {
		t.Run(tc.pattern, func(t *testing.T) {
			landmark := MustCompile(tc.pattern).code.FindOptimizations.LandmarkChain.Landmarks[0]
			s := requiredLandmarkSearch{alternatives: make([]requiredLandmarkCursor, len(landmark.Alternatives))}
			input := []rune(tc.input)
			if got, ok := s.next(input, 0, len(input), landmark); got != tc.want || ok != tc.found {
				t.Fatalf("next(%q) = %v, %v; want %v, %v", tc.input, got, ok, tc.want, tc.found)
			}
		})
	}
}

func TestRequiredLandmarkCursorDropsEarlierPrefix(t *testing.T) {
	// After the cursor reads "ab" at 0 and moves past 0, only the match
	// at 2 that overlaps it can still be a core.
	input := []rune("ababa")
	alt := syntax.RequiredLandmarkAlternative{Literal: []rune("aba")}
	var c requiredLandmarkCursor
	if _, ok := c.find(input, 0, 5, 2, 2, &alt); ok {
		t.Fatal("find from 0 found a core it has not read")
	}
	if m, ok := c.find(input, 1, 5, 3, 3, &alt); ok || c.undecided(5) != 2 {
		t.Fatalf("find from 1 = %v, %v, undecided %d; want none, undecided 2", m, ok, c.undecided(5))
	}
	if m, ok := c.find(input, 2, 5, 3, 5, &alt); !ok || m != (requiredLandmarkMatch{2, 5}) {
		t.Fatalf("find from 2 = %v, %v; want {2 5}", m, ok)
	}
}

func TestRequiredLandmarkCursorStopsWhenNoCoreFits(t *testing.T) {
	set := MustCompile(`\S*(\s+[ab]{3}|x)c()`).code.FindOptimizations.LandmarkChain.Landmarks[0].Alternatives[0]
	for _, tc := range []struct {
		name             string
		alt              syntax.RequiredLandmarkAlternative
		input            string
		startAt, readEnd int
	}{
		// Neither start where the literal fits matches.
		{"literal", syntax.RequiredLandmarkAlternative{Literal: []rune("abcd")}, "xxaba", 0, 2},
		{"long literal", syntax.RequiredLandmarkAlternative{Literal: []rune("aba")}, "ab", 0, 0},
		// No later start follows whitespace, but none fits either.
		{"set", set, "zzzzz", 3, 4},
	} {
		t.Run(tc.name, func(t *testing.T) {
			input := []rune(tc.input)
			var c requiredLandmarkCursor
			if _, ok := c.find(input, tc.startAt, len(input), tc.readEnd, tc.readEnd, &tc.alt); ok || !c.exhausted {
				t.Fatalf("find = %v, exhausted %v; want false, true", ok, c.exhausted)
			}
		})
	}
}

func TestRequiredLandmarkChainSearchReusesWork(t *testing.T) {
	for _, tc := range []struct {
		pattern, input string
	}{
		// One alternative's search says nothing about another's.
		{`\S*(\t|\s*b|\s*c)a()`, "aa ca"},
		// Whitespace that stops at another rune says nothing about cores
		// after that rune.
		{`\S*(\t|\s*b)c()`, " \ta bc"},
		// After the VM fails, the next search reuses the candidate.
		{`\S*\B(\t|\s*b)c()`, "a  bc"},
	} {
		re := MustCompile(tc.pattern)
		reference := compileWithoutSearchOptimizations(tc.pattern)
		got, err := re.FindAllRunesIndex([]rune(tc.input), -1)
		if err != nil {
			t.Fatal(err)
		}
		want, err := reference.FindAllRunesIndex([]rune(tc.input), -1)
		if err != nil || !sameStringIndexes(got, want) {
			t.Fatalf("%q on %q: indexes = %v, want %v, err %v", tc.pattern, tc.input, got, want, err)
		}
	}

	// A scan of other input must not reuse what an earlier scan found.
	re := MustCompile(`\S*(\t|\s*b)c()`)
	runner := &Runner{re: re, code: re.code}
	for _, input := range []string{"    \tc", "aaa\tbc", " \t c", "a \tbc"} {
		m, err := runner.scan([]rune(input), nil, 0, 0, -1, true, re.MatchTimeout)
		if err != nil {
			t.Fatal(err)
		}
		w, err := compileWithoutSearchOptimizations(re.String()).FindRunesMatch([]rune(input))
		if err != nil || (m == nil) != (w == nil) || m != nil && (m.runeSliceIndex() != w.RuneIndex || m.RuneLength != w.RuneLength) {
			t.Fatalf("scan(%q) = %s, want %s, err %v", input, matchSummary(m), matchSummary(w), err)
		}
	}

	// Nor must a call on a slice that the caller changed after an earlier call.
	// Reuse one runner even if a GC empties the pool.
	pooled := &Runner{re: re, code: re.code}
	re.runnerPool.New = func() any { return pooled }
	input := make([]rune, 6)
	for _, text := range []string{"xxxxxx", "\tc  bc"} {
		copy(input, []rune(text))
		got, err := re.FindAllRunesIndex(input, -1)
		if err != nil {
			t.Fatal(err)
		}
		want, err := compileWithoutSearchOptimizations(re.String()).FindAllRunesIndex(input, -1)
		if err != nil || !sameStringIndexes(got, want) {
			t.Fatalf("FindAllRunesIndex(%q) = %v, want %v, err %v", text, got, want, err)
		}
	}
}

func TestRequiredLandmarkChainSearchFindNextMatch(t *testing.T) {
	re := MustCompile(`\S*(\t|\s*b)c()`)
	other := MustCompile(`\S*(b|\t)c(\t|c)()`)
	for _, re := range []*Regexp{re, other} {
		if re.code.FindOptimizations == nil || re.code.FindOptimizations.FindMode != syntax.RequiredLandmarkChain_LeftToRight {
			t.Fatalf("%q does not use a required landmark chain", re.String())
		}
	}
	input := []rune(" \tc \tc  bc\tc \tc")
	next := func(re *Regexp, m *Match) *Match {
		n, err := re.FindNextMatch(m)
		if err != nil {
			t.Error(err)
		}
		return n
	}
	iterate := func(re *Regexp, m *Match) (matches []*Match, summaries []string) {
		for ; m != nil; m = next(re, m) {
			matches, summaries = append(matches, m), append(summaries, matchSummary(m))
		}
		return matches, summaries
	}
	first, err := re.FindRunesMatch(input)
	if err != nil {
		t.Fatal(err)
	}
	matches, got := iterate(re, first)
	_, want := iterate(compileWithoutSearchOptimizations(re.String()), first)
	if strings.Join(got, "; ") != strings.Join(want, "; ") {
		t.Fatalf("matches = %q, want %q", got, want)
	}

	// The searches for a match have passed the matches after it, so a call
	// on an earlier match must start new ones.
	for i := len(matches) - 2; i >= 0; i-- {
		if got := matchSummary(next(re, matches[i])); got != want[i+1] {
			t.Fatalf("FindNextMatch(match %d) = %s, want %s", i, got, want[i+1])
		}
	}

	// A pattern with other landmarks cannot use them.
	m := next(re, matches[0])
	if got, want := matchSummary(next(other, m)), matchSummary(next(compileWithoutSearchOptimizations(other.String()), m)); got != want {
		t.Fatalf("other pattern: FindNextMatch = %s, want %s", got, want)
	}

	// Only one of several calls on the same match can continue them.
	m = next(re, matches[0])
	var wg sync.WaitGroup
	for range 4 {
		wg.Go(func() {
			if _, got := iterate(re, next(re, m)); strings.Join(got, "; ") != strings.Join(want[2:], "; ") {
				t.Errorf("matches = %q, want %q", got, want[2:])
			}
		})
	}
	wg.Wait()
}

func TestRequiredLandmarkChainSearchDifferential(t *testing.T) {
	rng := rand.New(rand.NewSource(1))
	pick := func(choices ...string) string { return choices[rng.Intn(len(choices))] }
	alternative := func() string {
		var b strings.Builder
		if rng.Intn(2) == 0 {
			b.WriteString(pick(`\s*`, `\s+`, `\s{2,}`))
		}
		if rng.Intn(2) == 0 {
			b.WriteString(pick("a", "ab", "aab", "aba", "\t", " a", "a b"))
		} else {
			b.WriteString(pick(`[ab]`, `[a ]`, `[ \t]`, `[b\t]`) + pick("", "{1,2}", "{2}", "{1,3}", "{2147483646}", "{2,2147483646}"))
		}
		if rng.Intn(3) == 0 {
			b.WriteString(pick(`\s*`, `\s+`))
		}
		return b.String()
	}
	var inputs [][]rune
	for _, alphabet := range []string{"aab \t \t\nc", "ab "} {
		for i := 0; i < 75; i++ {
			input := make([]rune, rng.Intn(30))
			for j := range input {
				input[j] = rune(alphabet[rng.Intn(len(alphabet))])
			}
			inputs = append(inputs, input)
		}
	}
	for attempts, chains := 0, 0; chains < 150; attempts++ {
		if attempts == 10000 {
			t.Fatalf("found only %d landmark chains in %d patterns", chains, attempts)
		}
		var b strings.Builder
		b.WriteString(pick(`\S*`, `\S*?`, `[a-c]*`, `\w*`, `[^z]*`, `.*`))
		b.WriteString(pick("", "", `\b`, `\B`, `^`, `(?m:^)`, `$`, `\A`, `\G`, `\Z`))
		for i := 2 + rng.Intn(2); i > 0; i-- {
			alternatives := make([]string, 1+rng.Intn(3))
			for j := range alternatives {
				alternatives[j] = alternative()
			}
			b.WriteString("(" + strings.Join(alternatives, "|") + ")" + pick("", "", ".", `\s*`, "a?"))
		}
		b.WriteString(pick("()", "$", `\s*()`))
		pattern := b.String()
		option := []RegexOptions{None, ECMAScript, RE2, Multiline, IgnoreCase}[rng.Intn(5)]
		re := MustCompile(pattern, option)
		if re.code.FindOptimizations == nil || re.code.FindOptimizations.FindMode != syntax.RequiredLandmarkChain_LeftToRight {
			continue
		}
		chains++
		reference := compileWithoutSearchOptimizations(pattern, option)
		for _, input := range inputs {
			got, err := re.FindAllRunesIndex(input, -1)
			if err != nil {
				t.Fatal(err)
			}
			want, err := reference.FindAllRunesIndex(input, -1)
			if err != nil || !sameStringIndexes(got, want) {
				t.Fatalf("%q on %q, options %v: indexes = %v, want %v, err %v", pattern, string(input), option, got, want, err)
			}
			for _, start := range []int{0, len(input) / 2} {
				m, err := re.FindRunesMatchStartingAt(input, start)
				if err != nil {
					t.Fatal(err)
				}
				w, err := reference.FindRunesMatchStartingAt(input, start)
				if err != nil {
					t.Fatal(err)
				}
				for m != nil && w != nil {
					if !corpusIntSlicesEqual(corpusMatchSubmatchIndex(m), corpusMatchSubmatchIndex(w)) {
						t.Fatalf("%q on %q at %d, options %v: captures differ", pattern, string(input), start, option)
					}
					if m, err = re.FindNextMatch(m); err != nil {
						t.Fatal(err)
					}
					if w, err = reference.FindNextMatch(w); err != nil {
						t.Fatal(err)
					}
				}
				if (m == nil) != (w == nil) {
					t.Fatalf("%q on %q at %d, options %v: match counts differ", pattern, string(input), start, option)
				}
			}
		}
	}
}

func TestRequiredLandmarkLargeRepeatCount(t *testing.T) {
	// The short alternative must match even when another alternative has a
	// huge minimum length. On 32-bit platforms, adding that length to the
	// core's starting index (2) must not overflow and panic in the prefilter.
	re := MustCompile(`\S*(ab|[cd]{2147483646})z()`)
	m, err := re.FindStringMatch("__abz")
	if err != nil {
		t.Fatal(err)
	}
	if m == nil {
		t.Fatal("expected a match")
	}
	if m.RuneIndex != 0 || m.String() != "__abz" {
		t.Fatalf("got %q at %d; want %q at 0", m.String(), m.RuneIndex, "__abz")
	}
}

func BenchmarkRequiredLandmarkWhitespaceAlternatives(b *testing.B) {
	for _, n := range []int{1024, 4096, 16384} {
		b.Run(strconv.Itoa(n), func(b *testing.B) {
			re := MustCompile(`\S*(\t|\s*b)c()`)
			// There is no b, so only the tab alternative can match. Rewinding
			// over the spaces for the other alternative causes repeated scans.
			input := []rune(strings.Repeat(" ", n) + "\tc")
			b.ReportAllocs()
			for b.Loop() {
				m, err := re.FindRunesMatch(input)
				if err != nil {
					b.Fatal(err)
				}
				if m == nil {
					b.Fatal("expected a match")
				}
				if m.RuneIndex != n || m.RuneLength != 2 {
					b.Fatalf("got index %d, length %d; want index %d, length 2", m.RuneIndex, m.RuneLength, n)
				}
			}
		})
	}
}

func BenchmarkRequiredLandmarkFindNextMatch(b *testing.B) {
	for _, n := range []int{1024, 4096, 16384} {
		b.Run(strconv.Itoa(n), func(b *testing.B) {
			re := MustCompile(`\S*(\t|\s*b)[ \t]()`)
			// Searching for the second alternative reads all the whitespace
			// after each match, so each call must continue the last search.
			input := []rune(strings.Repeat(" \t\t", n/3))
			b.ReportAllocs()
			for b.Loop() {
				count := 0
				m, err := re.FindRunesMatch(input)
				for ; m != nil && err == nil; m, err = re.FindNextMatch(m) {
					count++
				}
				if err != nil {
					b.Fatal(err)
				}
				if count != n/3 {
					b.Fatalf("got %d matches; want %d", count, n/3)
				}
			}
		})
	}
}

func BenchmarkRequiredLandmarkLongWhitespaceCore(b *testing.B) {
	for _, n := range []int{1024, 4096, 16384} {
		b.Run(strconv.Itoa(n), func(b *testing.B) {
			re := MustCompile(`\S*(\t|\s*[ \t]{32768})\t*c()`)
			// The first alternative matches. The second cannot consume
			// 32768 whitespace runes because it encounters 'c'.
			// Padding ensures that remaining input length alone cannot
			// rule out the second alternative.
			input := []rune(" " + strings.Repeat("\t", n) + "c" + strings.Repeat("x", 32768))
			b.ReportAllocs()
			for b.Loop() {
				m, err := re.FindRunesMatch(input)
				if err != nil {
					b.Fatal(err)
				}
				if m == nil {
					b.Fatal("expected a match")
				}
				if m.RuneIndex != 1 || m.RuneLength != n+1 {
					b.Fatalf("got index %d, length %d; want index 1, length %d", m.RuneIndex, m.RuneLength, n+1)
				}
			}
		})
	}
}
