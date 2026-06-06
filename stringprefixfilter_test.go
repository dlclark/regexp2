package regexp2

import (
	"strings"
	"testing"

	"github.com/dlclark/regexp2/v2/syntax"
)

func TestStringIndexPrefixFilter(t *testing.T) {
	filter := stringIndexPrefixFilter("abc", false, 3)
	if filter == nil {
		t.Fatal("expected filter")
	}

	candidateByteIndex, ok := filter("xxabc", 0)
	if !ok {
		t.Fatal("expected candidate")
	}
	if got, want := candidateByteIndex, 2; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}

	candidateByteIndex, ok = filter("xxabcabc", 3)
	if !ok {
		t.Fatal("expected candidate after startAt")
	}
	if got, want := candidateByteIndex, 5; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}

	if _, ok := filter("xxab", 0); ok {
		t.Fatal("unexpected candidate for missing prefix")
	}
}

func TestStringIndexPrefixFilterMinRequiredLengthUsesBytes(t *testing.T) {
	filter := stringIndexPrefixFilter("é", false, 2)
	if filter == nil {
		t.Fatal("expected filter")
	}

	candidateByteIndex, ok := filter("é", 0)
	if !ok {
		t.Fatal("expected candidate")
	}
	if got, want := candidateByteIndex, 0; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}
}

func TestStringIndexPrefixFilterASCIIIgnoreCase(t *testing.T) {
	filter := stringIndexPrefixFilter("abc", true, 3)
	if filter == nil {
		t.Fatal("expected filter")
	}

	candidateByteIndex, ok := filter("xxABc", 0)
	if !ok {
		t.Fatal("expected candidate")
	}
	if got, want := candidateByteIndex, 2; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}
}

func TestStringIndexPrefixFilterRejectsNonASCIIIgnoreCasePrefix(t *testing.T) {
	if filter := stringIndexPrefixFilter("é", true, 1); filter != nil {
		t.Fatal("expected nil filter for non-ASCII ignore-case prefix")
	}
}

func TestStringIndexPrefixesFilter(t *testing.T) {
	filter := stringIndexPrefixesFilter([]string{"xyz", "abc"}, false, 3)
	if filter == nil {
		t.Fatal("expected filter")
	}

	candidateByteIndex, ok := filter("00abcxyz", 0)
	if !ok {
		t.Fatal("expected candidate")
	}
	if got, want := candidateByteIndex, 2; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}

	candidateByteIndex, ok = filter("00abcxyz", 5)
	if !ok {
		t.Fatal("expected candidate after startAt")
	}
	if got, want := candidateByteIndex, 5; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}

	if _, ok := filter("00abxy", 0); ok {
		t.Fatal("unexpected candidate for missing prefixes")
	}
}

func TestStringIndexPrefixesFilterASCIIIgnoreCase(t *testing.T) {
	filter := stringIndexPrefixesFilter([]string{"xyz", "abc"}, true, 3)
	if filter == nil {
		t.Fatal("expected filter")
	}

	candidateByteIndex, ok := filter("00ABcxyz", 0)
	if !ok {
		t.Fatal("expected candidate")
	}
	if got, want := candidateByteIndex, 2; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}
}

func TestStringIndexPrefixesFilterRejectsNonASCIIIgnoreCasePrefix(t *testing.T) {
	if filter := stringIndexPrefixesFilter([]string{"abc", "é"}, true, 1); filter != nil {
		t.Fatal("expected nil filter for non-ASCII ignore-case prefix")
	}
}

func TestStringFixedDistanceCharFilter(t *testing.T) {
	filter := stringFixedDistanceCharFilter('a', 2, 3)
	if filter == nil {
		t.Fatal("expected filter")
	}

	candidateByteIndex, ok := filter("é12a", 0)
	if !ok {
		t.Fatal("expected candidate")
	}
	if got, want := candidateByteIndex, 2; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}

	if _, ok := filter("xyz", 0); ok {
		t.Fatal("unexpected candidate for missing char")
	}

	if _, ok := filter("xyz", 0); ok {
		t.Fatal("unexpected candidate for missing char")
	}
}

func TestStringFixedDistanceCharFilterCandidateBeforeStart(t *testing.T) {
	filter := stringFixedDistanceCharFilter('a', 2, 1)
	if filter == nil {
		t.Fatal("expected filter")
	}

	if _, ok := filter("xya", 2); ok {
		t.Fatal("unexpected candidate when candidate would be before startAt")
	}
}

func TestStringFixedDistanceStringFilter(t *testing.T) {
	filter := stringFixedDistanceStringFilter("abc", 2, 3)
	if filter == nil {
		t.Fatal("expected filter")
	}

	candidateByteIndex, ok := filter("é12abc", 0)
	if !ok {
		t.Fatal("expected candidate")
	}
	if got, want := candidateByteIndex, 2; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}

	if _, ok := filter("é12ab", 0); ok {
		t.Fatal("unexpected candidate for missing literal")
	}
}

func TestStringFixedDistanceStringFilterLongLiteral(t *testing.T) {
	if filter := stringFixedDistanceStringFilter("aaaaaaaaa", 0, 9); filter != nil {
		t.Fatal("expected nil filter for literal exceeding max length")
	}
}

func TestStringLiteralAfterLoopFilter(t *testing.T) {
	filter := stringLiteralAfterLoopFilter(&syntax.LiteralAfterLoop{
		Char: '@',
		LoopNode: &syntax.RegexNode{
			Set: syntax.DigitClass(),
		},
	}, 4)
	if filter == nil {
		t.Fatal("expected filter")
	}

	candidateByteIndex, ok := filter("xx123@", 0)
	if !ok {
		t.Fatal("expected candidate")
	}
	if got, want := candidateByteIndex, 0; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}

	if _, ok := filter("xx123", 0); ok {
		t.Fatal("unexpected candidate for missing literal")
	}
}

func TestNewStringPrefixFilterFixedDistanceChar(t *testing.T) {
	filter := newStringPrefixFilter(&syntax.Code{
		FindOptimizations: &syntax.FindOptimizations{
			FindMode:          syntax.FixedDistanceChar_LeftToRight,
			MinRequiredLength: 2,
			FixedDistanceLiteral: syntax.FixedDistanceLiteral{
				C:        'a',
				Distance: 1,
			},
		},
	})
	if filter == nil {
		t.Fatal("expected filter")
	}

	candidateByteIndex, ok := filter("xa", 0)
	if !ok {
		t.Fatal("expected candidate")
	}
	if got, want := candidateByteIndex, 0; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}

	if _, ok := filter("xx", 0); ok {
		t.Fatal("unexpected candidate for missing char")
	}
}

func TestFindStringPrefixCandidateFallbacks(t *testing.T) {
	re := &Regexp{
		stringPrefixFilter: func(input string, startAt int) (candidateByteIndex int, ok bool) {
			return 1, true
		},
	}

	candidateByteIndex, ok := re.findStringPrefixCandidate("éabc", 0)
	if !ok {
		t.Fatal("expected fallback candidate")
	}
	if got, want := candidateByteIndex, 0; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}

	re.stringPrefixFilter = func(input string, startAt int) (candidateByteIndex int, ok bool) {
		return startAt - 1, true
	}
	candidateByteIndex, ok = re.findStringPrefixCandidate("abc", 1)
	if !ok {
		t.Fatal("expected fallback candidate")
	}
	if got, want := candidateByteIndex, 1; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}
}

func TestFindStringPrefixCandidateDisabledForRightToLeft(t *testing.T) {
	re := &Regexp{
		options: RightToLeft,
		stringPrefixFilter: func(input string, startAt int) (candidateByteIndex int, ok bool) {
			t.Fatal("right-to-left should not call stringPrefixFilter")
			return 0, false
		},
	}

	candidateByteIndex, ok := re.findStringPrefixCandidate("abc", 3)
	if !ok {
		t.Fatal("expected candidate")
	}
	if got, want := candidateByteIndex, 3; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}
}

func TestFindStringMatchStart(t *testing.T) {
	re := &Regexp{}

	candidateByteIndex, ok, err := re.findStringMatchStart("abc", -1)
	if err != nil {
		t.Fatalf("findStringMatchStart failed: %v", err)
	}
	if !ok {
		t.Fatal("expected candidate")
	}
	if got, want := candidateByteIndex, 0; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}

	re.options = RightToLeft
	candidateByteIndex, ok, err = re.findStringMatchStart("abc", -1)
	if err != nil {
		t.Fatalf("findStringMatchStart failed: %v", err)
	}
	if !ok {
		t.Fatal("expected candidate")
	}
	if got, want := candidateByteIndex, 3; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}

	if _, _, err := re.findStringMatchStart("abc", 4); err == nil {
		t.Fatal("expected startAt too large error")
	}
	if _, _, err := re.findStringMatchStart("aé", 2); err == nil {
		t.Fatal("expected invalid rune boundary error")
	}
}

func BenchmarkStringIndexPrefixesFilter(b *testing.B) {
	prefixes := []string{
		"value00", "value01", "value02", "value03",
		"value04", "value05", "value06", "value07",
		"value08", "value09", "value10", "value11",
		"value12", "value13", "value14", "victor",
	}
	input := strings.Repeat("zzzzzzzzzzzzzzzz", 4096) + "victor"

	b.Run("compiled_ascii", func(b *testing.B) {
		filter := stringIndexPrefixesFilter(prefixes, false, 5)
		if filter == nil {
			b.Fatal("expected filter")
		}
		b.ReportAllocs()
		b.SetBytes(int64(len(input)))
		for i := 0; i < b.N; i++ {
			idx, ok := filter(input, 0)
			if !ok || idx != len(input)-len("victor") {
				b.Fatalf("filter = (%d, %v)", idx, ok)
			}
		}
	})

	b.Run("old_fallback", func(b *testing.B) {
		b.ReportAllocs()
		b.SetBytes(int64(len(input)))
		for i := 0; i < b.N; i++ {
			idx, ok := indexAnyPrefixFallback(input, 0, prefixes, false, 5)
			if !ok || idx != len(input)-len("victor") {
				b.Fatalf("filter = (%d, %v)", idx, ok)
			}
		}
	})
}
