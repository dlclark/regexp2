package regexp2

import (
	"reflect"
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

func TestStringFilterInvalidUTF8Regression(t *testing.T) {
	for _, pattern := range []string{`�abc`, `(?:�abc|xyz)`, `.�ab`, `[x]*�abc`} {
		t.Run(pattern, func(t *testing.T) {
			re := MustCompile(pattern)
			for _, prefix := range []string{"x", strings.Repeat("x", 128)} {
				input := prefix + "\xffabc"
				want, err := re.FindRunesMatch([]rune(input))
				if err != nil || want == nil {
					t.Fatalf("rune reference = %v, %v", want, err)
				}
				got, err := re.MatchString(input)
				if err != nil || !got {
					t.Fatalf("MatchString = %v, %v", got, err)
				}
				m, err := re.FindStringMatch(input)
				if err != nil || m == nil || m.RuneIndex != want.RuneIndex || m.RuneLength != want.RuneLength {
					t.Fatalf("FindStringMatch = %v, %v; want %v", m, err, want)
				}
				indices, err := re.FindAllStringIndex(input, -1)
				expected := [][]int{{want.RuneIndex, want.RuneIndex + want.RuneLength}}
				if err != nil || !reflect.DeepEqual(indices, expected) {
					t.Fatalf("indices = %v, %v; want %v", indices, err, expected)
				}
				replaced, err := re.Replace(input, "!", -1, -1)
				expectedReplacement := input[:want.RuneIndex] + "!" + input[want.RuneIndex+want.RuneLength:]
				if err != nil || replaced != expectedReplacement {
					t.Fatalf("Replace = %q, %v; want %q", replaced, err, expectedReplacement)
				}
			}
		})
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

func TestStringIndexPrefixFilterRuneError(t *testing.T) {
	filter := stringIndexPrefixFilter("�abc", false, 4)
	if filter == nil {
		t.Fatal("expected a filter for valid UTF-8")
	}
	for _, tc := range []struct {
		input string
		index int
		ok    bool
	}{
		{"xx�abc", 2, true},
		{"xxxxx", 0, false},
		{"x\xffabc�abc", 0, true}, // invalid input must reach the rune engine
	} {
		index, ok := filter(tc.input, 0)
		if index != tc.index || ok != tc.ok {
			t.Fatalf("filter(%q) = %d, %v; want %d, %v", tc.input, index, ok, tc.index, tc.ok)
		}
	}
	m, err := MustCompile("�abc").FindStringMatch("x\xffabc�abc")
	if err != nil || m == nil || m.RuneIndex != 1 {
		t.Fatalf("expected the invalid-byte match before the valid U+FFFD: %v, %v", m, err)
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

func TestStringFixedDistanceSetFilter(t *testing.T) {
	filter := stringFixedDistanceSetFilter(syntax.FixedDistanceSet{
		Range:    &syntax.SingleRange{First: 'a', Last: 'c'},
		Distance: 2,
	}, 3)
	if filter == nil {
		t.Fatal("expected filter")
	}

	candidateByteIndex, ok := filter("é12b", 0)
	if !ok {
		t.Fatal("expected candidate")
	}
	if got, want := candidateByteIndex, 2; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}
	if _, ok := filter("é12z", 0); ok {
		t.Fatal("unexpected candidate")
	}
}

func TestStringFixedDistanceSetFilterSmallSet(t *testing.T) {
	filter := stringFixedDistanceSetFilter(syntax.FixedDistanceSet{
		Chars: []rune{'a', 'c'},
	}, 1)
	if filter == nil {
		t.Fatal("expected filter")
	}
	if got, ok := filter("zzc", 0); !ok || got != 2 {
		t.Fatalf("filter = %d, %v; want 2, true", got, ok)
	}
}

func TestStringFixedDistanceSetFilterRejectsUnsupportedSets(t *testing.T) {
	for _, set := range []syntax.FixedDistanceSet{
		{Chars: []rune{'é'}},
		{Chars: []rune{'a'}, Negated: true},
		{Range: &syntax.SingleRange{First: 'a', Last: 'é'}},
	} {
		if filter := stringFixedDistanceSetFilter(set, 1); filter != nil {
			t.Fatalf("expected nil filter for %+v", set)
		}
	}
}

func TestNewStringPrefixFilterLeadingSet(t *testing.T) {
	re := MustCompile(`[a-c]`)
	if got, want := re.code.FindOptimizations.FindMode, syntax.LeadingSet_LeftToRight; got != want {
		t.Fatalf("FindMode = %v, want %v", got, want)
	}
	if re.stringPrefixFilter == nil {
		t.Fatal("expected filter")
	}
	if got, ok := re.stringPrefixFilter("zzb", 0); !ok || got != 2 {
		t.Fatalf("filter = %d, %v; want 2, true", got, ok)
	}
	if _, ok := re.stringPrefixFilter("zzz", 0); ok {
		t.Fatal("unexpected candidate")
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

func TestStringSearchOriginRegression(t *testing.T) {
	re := MustCompile(`(?:\Gfoo|bar)`)
	for _, input := range []string{"xfoo", "foo", "xbar"} {
		want := input != "xfoo"
		got, err := re.MatchString(input)
		if err != nil || got != want {
			t.Fatalf("MatchString(%q) = %v, %v", input, got, err)
		}
		m, err := re.FindStringMatch(input)
		if err != nil || (m != nil) != want {
			t.Fatalf("FindStringMatch(%q) = %v, %v", input, m, err)
		}
	}
	for _, tt := range []struct {
		input string
		start int
		want  bool
	}{
		{"xxfoo", 1, false}, {"xxfoo", 2, true}, {"xxbar", 1, true},
		{"éxfoo", 2, false}, {"éxfoo", 3, true}, {"éxbar", 2, true},
	} {
		m, err := re.FindStringMatchStartingAt(tt.input, tt.start)
		if err != nil || (m != nil) != tt.want {
			t.Fatalf("StartingAt(%q,%d) = %v, %v", tt.input, tt.start, m, err)
		}
	}
	indices, err := re.FindAllStringIndex("xfoo", -1)
	if err != nil || len(indices) != 0 {
		t.Fatalf("indices = %v, %v", indices, err)
	}
	parts, err := re.Split("xfoo", -1)
	if err != nil || !reflect.DeepEqual(parts, []string{"xfoo"}) {
		t.Fatalf("split = %q, %v", parts, err)
	}
	got, err := re.ReplaceFunc("xfoo", func(Match) string { return "!" }, -1, -1)
	if err != nil || got != "xfoo" {
		t.Fatalf("ReplaceFunc = %q, %v", got, err)
	}
}

func TestStringSearchOriginNextMatches(t *testing.T) {
	for _, tc := range []struct{ pattern, input string }{
		{`(?:\Gfoo|bar)`, "ébarfoo"},
		{`(?:\Gfoo|bar)`, "ébarxfoo"},
		{`(?<=\Gx)foo|bar`, "xfoo"},
		{`(?<=x)foo`, "éxfoo"},
	} {
		t.Run(tc.pattern+tc.input, func(t *testing.T) {
			re := MustCompile(tc.pattern)
			want, err := re.FindRunesMatch([]rune(tc.input))
			if err != nil {
				t.Fatal(err)
			}
			matched, err := re.MatchString(tc.input)
			if err != nil || matched != (want != nil) {
				t.Fatalf("MatchString = %v, %v; reference = %v", matched, err, want)
			}
			got, err := re.FindStringMatch(tc.input)
			var expectedIndices [][]int
			for got != nil || want != nil {
				if err != nil || got == nil || want == nil || got.RuneIndex != want.RuneIndex || got.String() != want.String() {
					t.Fatalf("string = %v, %v; runes = %v", got, err, want)
				}
				start, length := want.ByteRange()
				expectedIndices = append(expectedIndices, []int{start, start + length})
				got, err = re.FindNextMatch(got)
				next, nextErr := re.FindNextMatch(want)
				if nextErr != nil {
					t.Fatal(nextErr)
				}
				want = next
			}
			if err != nil {
				t.Fatal(err)
			}
			indices, err := re.FindAllStringIndex(tc.input, -1)
			if err != nil || !reflect.DeepEqual(indices, expectedIndices) {
				t.Fatalf("indices = %v, %v; want %v", indices, err, expectedIndices)
			}
		})
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

func TestFindPrefixMatches(t *testing.T) {
	for _, tc := range []struct{ pattern, input string }{
		{`\d*(?:a[bc]){2}d`, "abacd"},
		{`\d*((abc)|(ade)|(abf))`, "ade"},
		{`\d*(éx|êy)`, "éx"},
	} {
		t.Run(tc.pattern, func(t *testing.T) {
			re := MustCompile(tc.pattern)
			if m, err := re.FindStringMatch(tc.input); err != nil || m == nil || m.String() != tc.input {
				t.Errorf("FindStringMatch(%q) = %v, %v; want full match", tc.input, m, err)
			}
			if m, err := re.FindRunesMatch([]rune(tc.input)); err != nil || m == nil || m.String() != tc.input {
				t.Errorf("FindRunesMatch(%q) = %v, %v; want full match", tc.input, m, err)
			}
		})
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
