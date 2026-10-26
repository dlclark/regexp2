package regexp2

import (
	"errors"
	"fmt"
	"reflect"
	"strings"
	"sync"
	"testing"
)

func TestFindRunesMatchesInvalidStart(t *testing.T) {
	defer func() {
		if p := recover(); p != nil {
			t.Fatalf("invalid start must return an error, not panic: %v", p)
		}
	}()
	re := MustCompile(`.`, RightToLeft)
	err := re.FindRunesMatches([]rune("a"), 2, func(m MatchView) bool {
		t.Fatal("callback called for an invalid starting position")
		return false
	})
	if err == nil {
		t.Fatal("expected an error for startAt beyond the input")
	}
}

func TestFindRunesMatchesBalancedGroupRange(t *testing.T) {
	for _, tc := range []struct {
		name, pattern, input string
		wantIndex            int
		wantOK               bool
	}{
		{"fully_balanced", `(?<x>a)+(?<-x>b)`, "ab", 0, false},
		{"capture_remaining", `(?<x>a)+(?<-x>b)`, "aab", 0, true},
		{"repeated_balancing", `(?<x>a)+(?<-x>b)+`, "aaabb", 0, true},
		{"recapture", `(?<x>a)+(?<-x>b)+(?<x>c)`, "aabbc", 4, true},
		{"balance_recapture", `(?<x>a)(?<-x>b)(?<x>c)(?<-x>d)`, "abcd", 0, false},
	} {
		t.Run(tc.name, func(t *testing.T) {
			re := MustCompile(tc.pattern)
			group := re.GroupNumberFromName("x")
			calls := 0
			err := re.FindRunesMatches([]rune(tc.input), -1, func(m MatchView) bool {
				calls++
				index, length, ok := m.GroupRange(group)
				if ok != tc.wantOK {
					t.Errorf("GroupRange(x) = (%d, %d, %v), want ok = %v", index, length, ok, tc.wantOK)
				}
				// Index and length are only meaningful when ok is true.
				if tc.wantOK && ok && (index != tc.wantIndex || length != 1) {
					t.Errorf("GroupRange(x) = (%d, %d, true), want (%d, 1, true)", index, length, tc.wantIndex)
				}
				return true
			})
			if err != nil || calls != 1 {
				t.Fatalf("iteration: calls = %d, err = %v; want 1, nil", calls, err)
			}
		})
	}
}

func TestFindRunesMatchesAllocationScaling(t *testing.T) {
	skipIfAllocsUnreliable(t)
	for _, opt := range []RegexOptions{None, ECMAScript} {
		for _, registered := range []bool{false, true} {
			t.Run(fmt.Sprintf("mode=%d/registered=%v", opt, registered), func(t *testing.T) {
				pattern := `(a)`
				if registered {
					pattern = `(?<matchview_allocation>a)`
					// A generated-engine test double: full execution captures group 1;
					// boolean-only execution elides it. Correct iteration must preserve the
					// capture while also reusing match storage.
					RegisterEngine(pattern, RuntimeEngineData{
						CapSize: 2,
						FindFirstChar: func(r *Runner) bool {
							for r.Runtextpos < r.Runtextend && r.Runtext[r.Runtextpos] != 'a' {
								r.Runtextpos++
							}
							return r.Runtextpos < r.Runtextend
						},
						Execute: func(r *Runner) error {
							start := r.Runtextpos
							r.Runtextpos++
							r.Capture(1, start, r.Runtextpos)
							r.Capture(0, start, r.Runtextpos)
							return nil
						},
						ExecuteQuick: func(r *Runner) error {
							start := r.Runtextpos
							r.Runtextpos++
							r.Capture(0, start, r.Runtextpos)
							return nil
						},
					}, opt)
				}
				re := MustCompile(pattern, opt)
				measure := func(matchCount int) float64 {
					input := []rune(strings.Repeat("a", matchCount))
					calls := 0
					callback := func(m MatchView) bool {
						index, length, ok := m.GroupRange(1)
						if !ok || index != calls || length != 1 {
							t.Fatalf("match %d: group range = (%d, %d, %v), want (%d, 1, true)", calls, index, length, ok, calls)
						}
						calls++
						return true
					}
					// AllocsPerRun warms the runner before measuring. Inputs and the
					// callback are prepared outside the measured operation.
					return testing.AllocsPerRun(100, func() {
						calls = 0
						err := re.FindRunesMatches(input, -1, callback)
						if err != nil || calls != matchCount {
							t.Fatalf("iteration: calls = %d, err = %v; want %d, nil", calls, err, matchCount)
						}
					})
				}
				one := measure(1)
				many := measure(100)
				// Permit fixed per-call overhead without pinning an exact allocation
				// count; an additional 99 matches must not allocate per-match objects.
				if many > one+1 {
					t.Fatalf("allocations grow with match count: 1 match = %.1f, 100 matches = %.1f", one, many)
				}
			})
		}
	}
}

func TestMatchViewZeroValue(t *testing.T) {
	var view MatchView
	if got := view.GroupCount(); got != 0 {
		t.Fatalf("GroupCount = %d, want 0", got)
	}
	for _, num := range []int{-1, 0, 1} {
		if i, n, ok := view.GroupRange(num); i != 0 || n != 0 || ok {
			t.Errorf("GroupRange(%d) = (%d,%d,%v), want (0,0,false)", num, i, n, ok)
		}
	}
}

func TestFindRunesMatchesParity(t *testing.T) {
	cases := []struct {
		name, pattern, input string
		options              []RegexOptions
	}{
		{"optional", `(a)?b`, "éabbab", nil},
		{"capture_reset", `(a|(b))+`, "ba ab ba", nil},
		{"empty", `a*`, "aéa", nil},
		{"empty_input", `()`, "", nil},
		{"empty_lookahead", `(?=(a))`, "aa", nil},
		{"lookbehind", `(?<=(a))b`, "ab ab", nil},
		{"backreference", `(a)?\1b`, "b aab", nil},
		{"duplicate_names", `(?<x>a)|(?<x>b)`, "ab ba", nil},
		{"named_backreference", `(?<x>a)\k<x>`, "aa aaaa", nil},
		{"search_origin", `\G(?:a|)`, "aa ba", []RegexOptions{None, RightToLeft}},
		{"sparse", `(?<5>a)(b)?`, "a ab a", []RegexOptions{None, RightToLeft}},
		{"balanced", `(?<x>a)+(?<-x>b)+`, "ab aab aaabb aabbb ab", []RegexOptions{None, RightToLeft}},
		{"boundary", `\b|\B`, "é aK", []RegexOptions{None, ECMAScript, ECMAScript | Unicode | IgnoreCase}},
		{"line_endings", `^|$`, "a\rb\nc\u2028d", []RegexOptions{Multiline, ECMAScript | Multiline}},
	}
	type captureRange struct {
		index, length int
		ok            bool
	}
	for _, tc := range cases {
		options := tc.options
		if options == nil {
			options = []RegexOptions{None, ECMAScript, ECMAScript | Unicode, RightToLeft}
		}
		for _, opt := range options {
			t.Run(fmt.Sprintf("%s/%d", tc.name, opt), func(t *testing.T) {
				re := MustCompile(tc.pattern, opt)
				input := []rune(tc.input)
				numbers := re.GetGroupNumbers()
				for start := -1; start <= len(input); start++ {
					var want, got [][]captureRange
					m, err := re.FindRunesMatchStartingAt(input, start)
					for m != nil && err == nil {
						row := make([]captureRange, len(numbers))
						for i, num := range numbers {
							g := m.GroupByNumber(num)
							row[i] = captureRange{g.RuneIndex, g.RuneLength, len(g.Captures) > 0}
						}
						want = append(want, row)
						m, err = re.FindNextMatch(m)
					}
					if err != nil {
						t.Fatal(err)
					}
					err = re.FindRunesMatches(input, start, func(v MatchView) bool {
						if v.GroupCount() != len(numbers) {
							t.Errorf("GroupCount = %d, want %d", v.GroupCount(), len(numbers))
						}
						row := make([]captureRange, len(numbers))
						for i, num := range numbers {
							index, length, ok := v.GroupRange(num)
							row[i] = captureRange{index, length, ok}
						}
						got = append(got, row)
						if len(got) > len(input)+1 {
							t.Fatal("iteration did not advance")
						}
						for _, num := range []int{-1, 1000} {
							if i, n, ok := v.GroupRange(num); i != 0 || n != 0 || ok {
								t.Errorf("invalid group %d = (%d,%d,%v)", num, i, n, ok)
							}
						}
						return true
					})
					if err != nil {
						t.Fatal(err)
					}
					if !reflect.DeepEqual(got, want) {
						t.Fatalf("start=%d: got %v, want %v", start, got, want)
					}
				}
			})
		}
	}
}

func TestMatchViewECMAScriptCaptures(t *testing.T) {
	for _, opt := range []RegexOptions{None, ECMAScript, ECMAScript | Unicode} {
		t.Run(fmt.Sprint(opt), func(t *testing.T) {
			re := MustCompile(`(a|(b))+`, opt)
			calls := 0
			err := re.FindRunesMatches([]rune("ba ba"), -1, func(v MatchView) bool {
				index, length, ok := v.GroupRange(2)
				if opt&ECMAScript != 0 {
					if ok {
						t.Error("group 2 should be cleared by the final repetition")
					}
				} else if !ok || index != calls*3 || length != 1 {
					t.Errorf("group 2 = (%d,%d,%v), want (%d,1,true)", index, length, ok, calls*3)
				}
				calls++
				return true
			})
			if err != nil || calls != 2 {
				t.Fatalf("calls=%d err=%v", calls, err)
			}
			re = MustCompile(`(?<x>a)|(?<x>b)`, opt)
			calls = 0
			err = re.FindRunesMatches([]rune("ab"), -1, func(v MatchView) bool {
				group := 1
				wantCount := 2
				if opt&ECMAScript != 0 {
					group += calls
					wantCount = 3
					_, _, otherOK := v.GroupRange(3 - group)
					if otherOK {
						t.Error("other duplicate-name group should not participate")
					}
				}
				if v.GroupCount() != wantCount {
					t.Errorf("GroupCount=%d, want %d", v.GroupCount(), wantCount)
				}
				i, n, ok := v.GroupRange(group)
				if !ok || i != calls || n != 1 {
					t.Errorf("group %d = (%d,%d,%v)", group, i, n, ok)
				}
				calls++
				return true
			})
			if err != nil || calls != 2 {
				t.Fatalf("calls=%d err=%v", calls, err)
			}
		})
	}
}

func TestFindRunesMatchesControlAndReuse(t *testing.T) {
	for _, opt := range []RegexOptions{None, ECMAScript} {
		t.Run(fmt.Sprint(opt), func(t *testing.T) {
			re := MustCompile(`(a)`, opt)
			input := []rune("aa")
			calls := 0
			if err := re.FindRunesMatches(input, -1, nil); err != nil {
				t.Fatal(err)
			}
			if err := re.FindRunesMatches(input, -1, func(v MatchView) bool { calls++; return false }); err != nil || calls != 1 {
				t.Fatalf("early stop: calls=%d err=%v", calls, err)
			}
			retained, err := re.FindStringMatch("éa")
			if err != nil || retained == nil {
				t.Fatalf("retained match: %v", err)
			}
			calls = 0
			err = re.FindRunesMatches(input, -1, func(v MatchView) bool {
				// Nested scans on the same Regexp must not invalidate the outer view.
				nested := 0
				err := re.FindRunesMatches(input, -1, func(inner MatchView) bool { nested++; return true })
				if err != nil || nested != 2 {
					t.Fatalf("nested: calls=%d err=%v", nested, err)
				}
				i, n, ok := v.GroupRange(1)
				if !ok || i != calls || n != 1 {
					t.Errorf("outer group = (%d,%d,%v)", i, n, ok)
				}
				calls++
				return true
			})
			if err != nil || calls != 2 {
				t.Fatalf("calls=%d err=%v", calls, err)
			}
			if retained.String() != "a" || retained.Captures[0].String() != "a" || retained.GroupByNumber(1).RuneIndex != 1 {
				t.Fatal("retained full match changed")
			}
			const marker = "callback panic"
			func() {
				defer func() {
					if p := recover(); p != marker {
						t.Errorf("panic=%v, want %q", p, marker)
					}
				}()
				_ = re.FindRunesMatches(input, -1, func(MatchView) bool { panic(marker) })
			}()
			if matched, err := re.MatchRunes(input); err != nil || !matched {
				t.Fatalf("after panic: matched=%v err=%v", matched, err)
			}
		})
	}
}

func TestFindRunesMatchesExecutionError(t *testing.T) {
	const pattern = `(?<view_execution_error>a)`
	sentinel := errors.New("test execution failure")
	RegisterEngine(pattern, RuntimeEngineData{
		CapSize:       2,
		FindFirstChar: func(r *Runner) bool { return r.Runtextpos < r.Runtextend },
		Execute: func(r *Runner) error {
			if r.Runtextpos == 1 {
				return sentinel
			}
			start := r.Runtextpos
			r.Runtextpos++
			r.Capture(0, start, r.Runtextpos)
			r.Capture(1, start, r.Runtextpos)
			return nil
		},
	})
	re := MustCompile(pattern)
	calls := 0
	err := re.FindRunesMatches([]rune("aa"), -1, func(MatchView) bool { calls++; return true })
	if !errors.Is(err, sentinel) || calls != 1 {
		t.Fatalf("calls=%d err=%v, want 1 and execution error", calls, err)
	}
	if err := re.FindRunesMatches([]rune("aa"), -1, func(MatchView) bool { return false }); err != nil {
		t.Fatalf("early stop should not execute the failing match: %v", err)
	}
	if m, err := re.FindRunesMatch([]rune("a")); err != nil || m == nil || m.String() != "a" {
		t.Fatalf("full match after error: %v", err)
	}
}

func TestFindRunesMatchesConcurrent(t *testing.T) {
	for _, opt := range []RegexOptions{None, ECMAScript} {
		re := MustCompile(`(a)?b`, opt)
		var wg sync.WaitGroup
		for worker := 0; worker < 4; worker++ {
			wg.Add(1)
			go func() {
				defer wg.Done()
				for n := 0; n < 20; n++ {
					calls := 0
					err := re.FindRunesMatches([]rune("abb"), -1, func(v MatchView) bool {
						_, _, ok := v.GroupRange(1)
						if ok != (calls == 0) {
							t.Error("stale capture")
						}
						calls++
						return true
					})
					if err != nil || calls != 2 {
						t.Errorf("calls=%d err=%v", calls, err)
					}
				}
			}()
		}
		wg.Wait()
	}
}
