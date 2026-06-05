package regexp2

import (
	"strconv"
	"testing"

	"github.com/dlclark/regexp2/v2/syntax"
)

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
