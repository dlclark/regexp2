package regexp2

import (
	"errors"
	"slices"
	"strconv"
	"testing"

	"github.com/dlclark/regexp2/v2/syntax"
)

func TestBacktrackingStackLimit(t *testing.T) {
	for _, pattern := range []string{`(?:^){100}`, `(?:^){100}?`} {
		t.Run(pattern, func(t *testing.T) {
			re := MustCompile(pattern, OptionMaxBacktrackingStackSize(32))
			matched, err := re.MatchString("")
			if matched {
				t.Fatal("MatchString unexpectedly matched")
			}
			if !errors.Is(err, ErrBacktrackingStackLimit) {
				t.Fatalf("MatchString error = %v, want ErrBacktrackingStackLimit", err)
			}

			runner := re.getRunner()
			trackSize := len(runner.runtrack)
			re.putRunner(runner)
			if got, want := trackSize, 32; got != want {
				t.Fatalf("allocated backtracking stack size = %d, want %d", got, want)
			}
		})
	}
}

func TestDefaultBacktrackingStackLimit(t *testing.T) {
	re := MustCompile(`(?:^){60000}`)
	matched, err := re.MatchString("")
	if matched {
		t.Fatal("MatchString unexpectedly matched")
	}
	if !errors.Is(err, ErrBacktrackingStackLimit) {
		t.Fatalf("MatchString error = %v, want ErrBacktrackingStackLimit", err)
	}
}

func TestBacktrackingStackLimitOverride(t *testing.T) {
	for _, limit := range []int{200000, -1} {
		t.Run(strconv.Itoa(limit), func(t *testing.T) {
			re := MustCompile(`(?:^){60000}`, OptionMaxBacktrackingStackSize(limit))
			matched, err := re.MatchString("")
			if err != nil {
				t.Fatalf("MatchString failed: %v", err)
			}
			if !matched {
				t.Fatal("MatchString did not match")
			}
		})
	}
}

func TestRunnerReusableAfterBacktrackingStackLimit(t *testing.T) {
	re := MustCompile(`x|(?:^){100}`, OptionMaxBacktrackingStackSize(64))
	if matched, err := re.MatchString(""); matched || !errors.Is(err, ErrBacktrackingStackLimit) {
		t.Fatalf("first MatchString = %v, %v; want false, ErrBacktrackingStackLimit", matched, err)
	}
	if matched, err := re.MatchString("x"); !matched || err != nil {
		t.Fatalf("second MatchString = %v, %v; want true, nil", matched, err)
	}
}

func TestLargeEmptyRepeatDoesNotUseBacktrackingStack(t *testing.T) {
	for _, pattern := range []string{
		`(?:){50000000}`,
		`(?:){50000000}?`,
		`a(?:){50000000}b`,
		`(?:){2147483647}`,
	} {
		t.Run(pattern, func(t *testing.T) {
			re := MustCompile(pattern, OptionMaxBacktrackingStackSize(16))
			input := ""
			if pattern[0] == 'a' {
				input = "ab"
			}
			matched, err := re.MatchString(input)
			if err != nil {
				t.Fatalf("MatchString failed: %v", err)
			}
			if !matched {
				t.Fatal("MatchString did not match")
			}
		})
	}
}

func TestEmptyCaptureRepeatIsNotReduced(t *testing.T) {
	re := MustCompile(`(){2}`)
	match, err := re.FindStringMatch("")
	if err != nil {
		t.Fatalf("FindStringMatch failed: %v", err)
	}
	if match == nil {
		t.Fatal("FindStringMatch did not match")
	}
	if got, want := len(match.GroupByNumber(1).Captures), 2; got != want {
		t.Fatalf("capture count = %d, want %d", got, want)
	}
}

func TestReplacerDataCacheBounded(t *testing.T) {
	re := MustCompile(`a`, OptionMaxCachedReplacerDataEntries(2), OptionMaxCachedReplacerDataBytes(-1))
	for _, replacement := range []string{"one", "two", "three"} {
		if _, err := re.Replace("a", replacement, -1, -1); err != nil {
			t.Fatalf("Replace(%q) failed: %v", replacement, err)
		}
	}

	if got := len(re.replaceCache.cache); got != 2 {
		t.Fatalf("cache size = %d, want 2", got)
	}
	if _, ok := re.replaceCache.cache["one"]; ok {
		t.Fatalf("least recently used replacement was not evicted")
	}
}

func TestReplacerDataCacheDisabled(t *testing.T) {
	re := MustCompile(`a`, OptionMaxCachedReplacerDataEntries(0))
	if _, err := re.Replace("a", "x", -1, -1); err != nil {
		t.Fatalf("Replace failed: %v", err)
	}
	if re.replaceCache != nil {
		t.Fatalf("replaceCache was initialized when caching was disabled")
	}
}

func TestReplacerDataCacheMaxBytes(t *testing.T) {
	re := MustCompile(`a`, OptionMaxCachedReplacerDataEntries(4), OptionMaxCachedReplacerDataBytes(3))
	if _, err := re.Replace("a", "toolong", -1, -1); err != nil {
		t.Fatalf("Replace failed: %v", err)
	}
	if got := len(re.replaceCache.cache); got != 0 {
		t.Fatalf("cache size = %d, want 0", got)
	}
}

func TestFindOptimizationsCarriedToCode(t *testing.T) {
	re := MustCompile(`..abc`)
	if re.code.FindOptimizations == nil {
		t.Fatal("FindOptimizations was not retained on compiled code")
	}
	if got, want := re.code.FindOptimizations.FindMode, syntax.FixedDistanceString_LeftToRight; got != want {
		t.Fatalf("FindMode = %v, want %v", got, want)
	}
}

func TestLeadingSetUsesOptimizedScanner(t *testing.T) {
	re := MustCompile(`[a-q]`)
	if got, want := re.code.FindOptimizations.FindMode, syntax.LeadingSet_LeftToRight; got != want {
		t.Fatalf("FindMode = %v, want %v", got, want)
	}
	runner := &Runner{
		re:         re,
		code:       re.code,
		Runtext:    []rune("zzb"),
		Runtextend: 3,
	}
	if !shouldUseFindFirstCharOptimized(runner) {
		t.Fatal("LeadingSet_LeftToRight was not routed to the optimized scanner")
	}
	if handled, found := findFirstCharOptimized(runner); !handled || !found {
		t.Fatalf("findFirstCharOptimized = %v, %v; want true, true", handled, found)
	}
	if got, want := runner.Runtextpos, 2; got != want {
		t.Fatalf("Runtextpos = %d, want %d", got, want)
	}
}

func TestFindLeadingStringsSkipsToFirstRune(t *testing.T) {
	prefixes := [][]rune{[]rune("apple"), []rune("apricot"), []rune("tiger")}
	runner := &Runner{Runtext: []rune("zzapricot"), Runtextend: 9}
	if !findLeadingStringsLeftToRight(runner, prefixes, []rune{'a', 't'}, false) {
		t.Fatal("expected prefix")
	}
	if got, want := runner.Runtextpos, 2; got != want {
		t.Fatalf("Runtextpos = %d, want %d", got, want)
	}
}

func TestQuickMatchCaptureLiveness(t *testing.T) {
	re := MustCompile(`^(a)(b)\1$`)
	if got, want := re.code.CaptureSlotInUse, []bool{true, true, false}; !slices.Equal(got, want) {
		t.Fatalf("CaptureSlotInUse = %v, want %v", got, want)
	}
	if len(re.code.QuickCodes) == 0 || len(re.code.QuickCodes) >= len(re.code.Codes) {
		t.Fatalf("quick code size = %d, full code size = %d; want smaller quick code", len(re.code.QuickCodes), len(re.code.Codes))
	}

	runner := re.getRunner()
	defer re.putRunner(runner)
	runner.code = re.quickCode
	input := []rune("aba")
	m, err := runner.scan(input, nil, 0, true, re.MatchTimeout)
	if err != nil || m == nil {
		t.Fatalf("scan = %v, %v; want match", m, err)
	}
	if got := runner.runmatch.matchcount[1]; got != 1 {
		t.Fatalf("live capture count = %d, want 1", got)
	}
	if got := runner.runmatch.matchcount[2]; got != 0 {
		t.Fatalf("dead capture count = %d, want 0", got)
	}

	// A subsequent full match must still return every capture.
	full, err := re.run(false, 0, input, newMatchText(input))
	if err != nil || full == nil {
		t.Fatalf("full match = %v, %v", full, err)
	}
	if got, want := full.GroupByNumber(2).String(), "b"; got != want {
		t.Fatalf("group 2 = %q, want %q", got, want)
	}
}

func TestQuickMatchRetainsBalancingCaptures(t *testing.T) {
	re := MustCompile(`^\((?>[^()]+|\((?<depth>)|\)(?<-depth>))*(?(depth)(?!))\)$`)
	for i, input := range []string{"((a(b))c)", "((a(b))c"} {
		got, err := re.MatchString(input)
		if err != nil {
			t.Fatalf("MatchString(%q): %v", input, err)
		}
		want := i == 0
		if got != want {
			t.Fatalf("MatchString(%q) = %v, want %v", input, got, want)
		}
	}
}

func TestFixedDistanceStringFindFirstChar(t *testing.T) {
	re := MustCompile(`..abc`)
	m, err := re.FindStringMatch("zzxxabc")
	if err != nil {
		t.Fatalf("FindStringMatch failed: %v", err)
	}
	if m == nil {
		t.Fatal("expected match")
	}
	if got, want := m.RuneIndex, 2; got != want {
		t.Fatalf("match index = %d, want %d", got, want)
	}
}

func TestMinRequiredLengthShortCircuits(t *testing.T) {
	re := MustCompile(`abc`)
	if got, want := re.code.FindOptimizations.MinRequiredLength, 3; got != want {
		t.Fatalf("MinRequiredLength = %d, want %d", got, want)
	}
	m, err := re.FindStringMatch("ab")
	if err != nil {
		t.Fatalf("FindStringMatch failed: %v", err)
	}
	if m != nil {
		t.Fatalf("unexpected match %q", m.String())
	}
}

func TestStringPrefixFilterFindStringMatchStartingAtInvalidBoundary(t *testing.T) {
	re := MustCompile(`abc`)
	if re.stringPrefixFilter == nil {
		t.Fatal("expected string prefix filter")
	}
	if _, err := re.FindStringMatchStartingAt("aéabc", 2); err == nil {
		t.Fatal("expected invalid rune boundary error")
	}
}

func TestStringPrefixFilterFindStringMatchCandidate(t *testing.T) {
	re := MustCompile(`abc`)
	if re.stringPrefixFilter == nil {
		t.Fatal("expected string prefix filter")
	}
	m, err := re.FindStringMatch("xxabc")
	if err != nil {
		t.Fatalf("FindStringMatch failed: %v", err)
	}
	if m == nil {
		t.Fatal("expected match")
	}
	if got, want := m.RuneIndex, 2; got != want {
		t.Fatalf("match index = %d, want %d", got, want)
	}
}

func TestFindOptimizationRepeatPrefixNullableFixedDoesNotUseLiteralAfterLoop(t *testing.T) {
	re := MustCompile(`(\d+)(-)?b.c`)
	if re.stringPrefixFilter != nil {
		t.Fatal("did not expect string prefix filter")
	}
	if got, notWant := re.code.FindOptimizations.FindMode, syntax.LiteralAfterLoop_LeftToRight; got == notWant {
		t.Fatalf("FindMode = %v, should not use nullable fixed suffix as literal-after-loop prefix", got)
	}

	m, err := re.FindStringMatch("1-b.c")
	if err != nil {
		t.Fatalf("FindStringMatch failed: %v", err)
	}
	if m == nil {
		t.Fatal("expected match")
	}
	if got, want := m.String(), "1-b.c"; got != want {
		t.Fatalf("match = %q, want %q", got, want)
	}
}

func TestStringPrefixFilterMatchesUnoptimizedResults(t *testing.T) {
	tests := []struct {
		name     string
		pattern  string
		options  []CompileOption
		wantMode syntax.FindNextStartingPositionMode
		inputs   []string
		startAts []int
	}{
		{
			name:     "leading set",
			pattern:  `[a-c]`,
			wantMode: syntax.LeadingSet_LeftToRight,
			inputs: []string{
				"a",
				"zzc",
				"ééb",
				"zzz",
				"",
			},
			startAts: []int{0, 1, 2, 4},
		},
		{
			name:     "leading string",
			pattern:  `abc\d+`,
			wantMode: syntax.LeadingString_LeftToRight,
			inputs: []string{
				"abc123",
				"xxabc123",
				"xxabc",
				"éabc123",
				"ababc123",
			},
			startAts: []int{0, 1, 2},
		},
		{
			name:     "leading string ignore case",
			pattern:  `(?i)abc\d+`,
			wantMode: syntax.LeadingString_OrdinalIgnoreCase_LeftToRight,
			inputs: []string{
				"ABC123",
				"xxAbC123",
				"xxabc",
				"éABC123",
			},
			startAts: []int{0, 1, 2},
		},
		{
			name:     "leading strings",
			pattern:  `(?:apple|tiger)\d+`,
			options:  []CompileOption{OptionIsCodeGen()},
			wantMode: syntax.LeadingStrings_LeftToRight,
			inputs: []string{
				"apple42",
				"xxxtiger7",
				"xxapple",
				"ééapple42",
				"tiger apple42",
			},
			startAts: []int{0, 1, 2, 4},
		},
		{
			name:     "leading strings ignore case",
			pattern:  `(?i:apple|tiger)\d+`,
			options:  []CompileOption{OptionIsCodeGen()},
			wantMode: syntax.LeadingStrings_OrdinalIgnoreCase_LeftToRight,
			inputs: []string{
				"APPLE42",
				"xxxTiGeR7",
				"xxAPPLE",
				"ééApple42",
			},
			startAts: []int{0, 1, 2, 4},
		},
		{
			name:     "fixed distance char",
			pattern:  `.q.`,
			wantMode: syntax.FixedDistanceChar_LeftToRight,
			inputs: []string{
				"xq1",
				"zzxq1",
				"zqz xq1",
				"éq1",
				"qq",
			},
			startAts: []int{0, 1, 2},
		},
		{
			name:     "fixed distance string",
			pattern:  `..abc\d`,
			wantMode: syntax.FixedDistanceString_LeftToRight,
			inputs: []string{
				"xxabc1",
				"zzxxabc1",
				"éxabc1",
				"xxabc",
				"0abc xxabc1",
			},
			startAts: []int{0, 1, 2, 5},
		},
		{
			name:     "literal after loop char",
			pattern:  `\d*@\w+`,
			wantMode: syntax.LiteralAfterLoop_LeftToRight,
			inputs: []string{
				"123@abc",
				"xx123@abc",
				"xxx@abc",
				"123 abc",
				"é123@abc",
			},
			startAts: []int{0, 1, 2},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			optimized := MustCompile(tt.pattern, tt.options...)
			if optimized.stringPrefixFilter == nil {
				t.Fatalf("expected string prefix filter, FindMode = %v", optimized.code.FindOptimizations.FindMode)
			}
			if got := optimized.code.FindOptimizations.FindMode; got != tt.wantMode {
				t.Fatalf("FindMode = %v, want %v", got, tt.wantMode)
			}

			unoptimized := MustCompile(tt.pattern, tt.options...)
			unoptimized.stringPrefixFilter = nil

			for _, input := range tt.inputs {
				assertEquivalentStringMatchAPIs(t, optimized, unoptimized, input)
				for _, startAt := range tt.startAts {
					if startAt <= len(input) && isStringRuneBoundary(input, startAt) {
						assertEquivalentStringMatchStartingAt(t, optimized, unoptimized, input, startAt)
					}
				}
			}
		})
	}
}

func assertEquivalentStringMatchAPIs(t *testing.T, optimized, unoptimized *Regexp, input string) {
	t.Helper()

	gotMatchString, gotErr := optimized.MatchString(input)
	wantMatchString, wantErr := unoptimized.MatchString(input)
	if (gotErr != nil) != (wantErr != nil) || gotMatchString != wantMatchString {
		t.Fatalf("MatchString(%q) = (%v, %v), want (%v, %v)", input, gotMatchString, gotErr, wantMatchString, wantErr)
	}

	gotMatch, gotErr := optimized.FindStringMatch(input)
	wantMatch, wantErr := unoptimized.FindStringMatch(input)
	assertSameMatchResult(t, "FindStringMatch", input, gotMatch, gotErr, wantMatch, wantErr)

	gotIndexes, gotErr := optimized.FindAllStringIndex(input, -1)
	wantIndexes, wantErr := unoptimized.FindAllStringIndex(input, -1)
	if (gotErr != nil) != (wantErr != nil) || !sameStringIndexes(gotIndexes, wantIndexes) {
		t.Fatalf("FindAllStringIndex(%q) = (%v, %v), want (%v, %v)", input, gotIndexes, gotErr, wantIndexes, wantErr)
	}
}

func assertEquivalentStringMatchStartingAt(t *testing.T, optimized, unoptimized *Regexp, input string, startAt int) {
	t.Helper()

	gotMatch, gotErr := optimized.FindStringMatchStartingAt(input, startAt)
	wantMatch, wantErr := unoptimized.FindStringMatchStartingAt(input, startAt)
	assertSameMatchResult(t, "FindStringMatchStartingAt", input, gotMatch, gotErr, wantMatch, wantErr)
}

func assertSameMatchResult(t *testing.T, api, input string, got *Match, gotErr error, want *Match, wantErr error) {
	t.Helper()

	if (gotErr != nil) != (wantErr != nil) {
		t.Fatalf("%s(%q) error = %v, want %v", api, input, gotErr, wantErr)
	}
	if gotErr != nil {
		return
	}
	if (got == nil) != (want == nil) {
		t.Fatalf("%s(%q) match = %v, want %v", api, input, matchSummary(got), matchSummary(want))
	}
	if got == nil {
		return
	}

	gotByteIndex, gotByteLength := got.ByteRange()
	wantByteIndex, wantByteLength := want.ByteRange()
	if got.String() != want.String() ||
		got.RuneIndex != want.RuneIndex ||
		got.RuneLength != want.RuneLength ||
		gotByteIndex != wantByteIndex ||
		gotByteLength != wantByteLength {
		t.Fatalf("%s(%q) match = %v, want %v", api, input, matchSummary(got), matchSummary(want))
	}
}

func matchSummary(m *Match) string {
	if m == nil {
		return "<nil>"
	}
	byteIndex, byteLength := m.ByteRange()
	return m.String() + " runeIndex=" + strconv.Itoa(m.RuneIndex) + " runeLength=" + strconv.Itoa(m.RuneLength) +
		" byteIndex=" + strconv.Itoa(byteIndex) + " byteLength=" + strconv.Itoa(byteLength)
}

func sameStringIndexes(a, b [][]int) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if len(a[i]) != len(b[i]) {
			return false
		}
		for j := range a[i] {
			if a[i][j] != b[i][j] {
				return false
			}
		}
	}
	return true
}
