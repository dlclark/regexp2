package regexp2

import (
	"strings"
	"testing"

	"github.com/dlclark/regexp2/v2/syntax"
)

func TestAnalyzeLeftContext(t *testing.T) {
	t.Run("literal", func(t *testing.T) {
		re := MustCompile(`foo`)
		if got := re.code.LeftContextRunes; got != 0 {
			t.Fatalf("LeftContextRunes = %d, want 0", got)
		}
	})
	t.Run("word boundary", func(t *testing.T) {
		re := MustCompile(`\bfoo`)
		if got := re.code.LeftContextRunes; got != 1 {
			t.Fatalf("LeftContextRunes = %d, want 1", got)
		}
	})
	t.Run("bol", func(t *testing.T) {
		re := MustCompile(`(?m)^foo`)
		if got := re.code.LeftContextRunes; got != 1 {
			t.Fatalf("LeftContextRunes = %d, want 1", got)
		}
	})
	t.Run("beginning", func(t *testing.T) {
		re := MustCompile(`\Afoo`)
		if got := re.code.LeftContextRunes; got != 1 {
			t.Fatalf("LeftContextRunes = %d, want 1", got)
		}
	})
	t.Run("lookbehind", func(t *testing.T) {
		re := MustCompile(`(?<=x)foo`)
		if got := re.code.LeftContextRunes; got != -1 {
			t.Fatalf("LeftContextRunes = %d, want -1", got)
		}
	})
}

func TestDecodeInputSkipsPrefixOffset(t *testing.T) {
	re := MustCompile(`needle`)
	prefix := strings.Repeat("漢", 20)
	input := prefix + "needle"
	startAt := len(prefix)
	d := decodeInput(input, startAt, re.decodeFrom(input, startAt), 0)
	defer d.release()
	if d.runeOffset != 0 {
		t.Fatalf("internal scans should leave runeOffset 0, got %d", d.runeOffset)
	}
	if got := string(d.runes); got != "needle" {
		t.Fatalf("decoded %q, want needle", got)
	}
	if d.runeStart != 0 {
		t.Fatalf("runeStart = %d, want 0", d.runeStart)
	}
}

func TestDecodeInputUnspecifiedStart(t *testing.T) {
	d := decodeInput("abc", -1, 0, 0)
	defer d.release()
	if d.runeStart != -1 {
		t.Fatalf("runeStart = %d, want -1 for startAt < 0", d.runeStart)
	}
}

func TestDecodeInputMidRuneStart(t *testing.T) {
	// "aéx": 'é' occupies bytes 1..2, so 2 is not a rune boundary.
	d := decodeInput("aéx", 2, 0, 0)
	defer d.release()
	if d.runeStart != -1 {
		t.Fatalf("runeStart = %d, want -1 for mid-rune startAt", d.runeStart)
	}
}

func TestSlicedDecodeSkipsPrefix(t *testing.T) {
	re := MustCompile(`needle`)
	input := strings.Repeat("x", 1000) + "needle"
	d := re.decodeStringInput(input, 1000, true)
	defer d.release()
	if d.byteOffset != 1000 {
		t.Fatalf("byteOffset = %d, want 1000", d.byteOffset)
	}
	if got := string(d.runes); got != "needle" {
		t.Fatalf("decoded %q, want needle", got)
	}
}

func TestSlicedDecodeKeepsBoundarySlack(t *testing.T) {
	re := MustCompile(`\bneedle`)
	if re.code.LeftContextRunes != 1 {
		t.Fatalf("LeftContextRunes = %d", re.code.LeftContextRunes)
	}
	input := strings.Repeat("x", 1000) + " needle"
	// candidate is the 'n' of needle, one past the space
	d := re.decodeStringInput(input, 1001, true)
	defer d.release()
	if d.byteOffset != 1000 {
		t.Fatalf("byteOffset = %d, want slack at 1000, got decode from %d (%q)", d.byteOffset, d.byteOffset, string(d.runes))
	}
	if got := string(d.runes); got != " needle" {
		t.Fatalf("decoded %q", got)
	}
}

func TestSlicedDecodeDoesNotSliceLookbehind(t *testing.T) {
	re := MustCompile(`(?<=x)foo`)
	input := strings.Repeat("z", 50) + "xfoo"
	d := re.decodeStringInput(input, 51, true)
	defer d.release()
	if d.byteOffset != 0 {
		t.Fatalf("lookbehind should decode from 0, got %d", d.byteOffset)
	}
	if len(d.runes) != len([]rune(input)) {
		t.Fatalf("rune count = %d, want %d", len(d.runes), len([]rune(input)))
	}
}

func TestFindStringMatchIndexesAfterSlice(t *testing.T) {
	re := MustCompile(`needle`)
	prefix := strings.Repeat("x", 200)
	input := prefix + "needle" + "tail"
	m, err := re.FindStringMatch(input)
	if err != nil || m == nil {
		t.Fatalf("match = %v, %v", m, err)
	}
	if got, want := m.RuneIndex, 200; got != want {
		t.Fatalf("RuneIndex = %d, want %d", got, want)
	}
	if got, want := m.String(), "needle"; got != want {
		t.Fatalf("String = %q", got)
	}
	idx, length := m.ByteRange()
	if idx != 200 || length != 6 {
		t.Fatalf("ByteRange = %d, %d", idx, length)
	}
}

func TestFindStringMatchUnicodeIndexesAfterSlice(t *testing.T) {
	re := MustCompile(`needle`)
	prefix := strings.Repeat("é", 20)
	input := prefix + "needle"
	m, err := re.FindStringMatch(input)
	if err != nil || m == nil {
		t.Fatalf("match = %v, %v", m, err)
	}
	if got, want := m.RuneIndex, 20; got != want {
		t.Fatalf("RuneIndex = %d, want %d", got, want)
	}
	if got := m.String(); got != "needle" {
		t.Fatalf("String = %q", got)
	}
	idx, length := m.ByteRange()
	if idx != len(prefix) || length != 6 {
		t.Fatalf("ByteRange = %d, %d; prefix bytes %d", idx, length, len(prefix))
	}
}

func TestFindStringMatchUnicodeCaptureAfterSlice(t *testing.T) {
	re := MustCompile(`é.`)
	prefix := strings.Repeat("漢", 20)
	input := prefix + "é界"
	m, err := re.FindStringMatch(input)
	if err != nil || m == nil {
		t.Fatalf("match = %v, %v", m, err)
	}
	if got, want := m.RuneIndex, 20; got != want {
		t.Fatalf("RuneIndex = %d, want %d", got, want)
	}
	if got, want := m.String(), "é界"; got != want {
		t.Fatalf("String = %q, want %q", got, want)
	}
	idx, length := m.ByteRange()
	if idx != len(prefix) || length != len("é界") {
		t.Fatalf("ByteRange = %d, %d; want %d, %d", idx, length, len(prefix), len("é界"))
	}
}

func TestMatchStringBoundaryAfterSlice(t *testing.T) {
	re := MustCompile(`\bcat\b`)
	ok, err := re.MatchString(strings.Repeat("x", 100) + " cat ")
	if err != nil || !ok {
		t.Fatalf("expected match, got %v %v", ok, err)
	}
	ok, err = re.MatchString(strings.Repeat("x", 100) + "cat")
	if err != nil || ok {
		t.Fatalf("expected miss inside word, got %v %v", ok, err)
	}
}

func TestMatchStringLookbehind(t *testing.T) {
	re := MustCompile(`(?<=x)foo`)
	ok, err := re.MatchString(strings.Repeat("z", 80) + "xfoo")
	if err != nil || !ok {
		t.Fatalf("expected match, got %v %v", ok, err)
	}
	ok, err = re.MatchString(strings.Repeat("z", 80) + "yfoo")
	if err != nil || ok {
		t.Fatalf("expected miss, got %v %v", ok, err)
	}
}

func TestMatchStringBeginningAfterCandidate(t *testing.T) {
	re := MustCompile(`\Afoo`)
	ok, err := re.MatchString("xxfoo")
	if err != nil || ok {
		t.Fatalf("\\A should not match mid-string, got %v %v", ok, err)
	}
	ok, err = re.MatchString("foo")
	if err != nil || !ok {
		t.Fatalf("\\A should match at start, got %v %v", ok, err)
	}
}

func TestCaptureStringUsesOriginalInput(t *testing.T) {
	re := MustCompile(`needle`)
	input := strings.Repeat("x", 50) + "needle"
	m, err := re.FindStringMatch(input)
	if err != nil || m == nil {
		t.Fatalf("match = %v, %v", m, err)
	}
	if got := m.String(); got != "needle" {
		t.Fatalf("String = %q", got)
	}
	if m.text == nil || !m.text.hasStringInput {
		t.Fatal("expected string-backed match text")
	}
	if !m.text.byteOffsetsReady {
		t.Fatal("byte offsets should be initialized when the match is returned")
	}
	if got := m.String(); got != input[50:56] {
		t.Fatalf("String = %q, want input[50:56]", got)
	}
}

func TestFindAllStringIndexAfterSlice(t *testing.T) {
	re := MustCompile(`ab`)
	input := strings.Repeat("x", 50) + "abxxab"
	got, err := re.FindAllStringIndex(input, -1)
	if err != nil {
		t.Fatal(err)
	}
	want := [][]int{{50, 52}, {54, 56}}
	if len(got) != len(want) {
		t.Fatalf("got %v", got)
	}
	for i := range want {
		if got[i][0] != want[i][0] || got[i][1] != want[i][1] {
			t.Fatalf("got %v, want %v", got, want)
		}
	}
}

func TestFindNextMatchAfterSlicedStart(t *testing.T) {
	re := MustCompile(`ab`)
	input := strings.Repeat("z", 40) + "ab--ab"
	m, err := re.FindStringMatch(input)
	if err != nil || m == nil {
		t.Fatalf("first = %v %v", m, err)
	}
	if m.RuneIndex != 40 {
		t.Fatalf("first index = %d", m.RuneIndex)
	}
	m, err = re.FindNextMatch(m)
	if err != nil || m == nil {
		t.Fatalf("second = %v %v", m, err)
	}
	if m.RuneIndex != 44 {
		t.Fatalf("second index = %d", m.RuneIndex)
	}
}

func TestFindNextMatchUnicodeAfterSlice(t *testing.T) {
	re := MustCompile(`ab`)
	prefix := strings.Repeat("漢", 25)
	input := prefix + "ab--ab"
	m, err := re.FindStringMatch(input)
	if err != nil || m == nil {
		t.Fatalf("first = %v %v", m, err)
	}
	if m.RuneIndex != 25 || m.String() != "ab" {
		t.Fatalf("first = %q at %d", m.String(), m.RuneIndex)
	}

	m, err = re.FindNextMatch(m)
	if err != nil || m == nil {
		t.Fatalf("second = %v %v", m, err)
	}
	if m.RuneIndex != 29 || m.String() != "ab" {
		t.Fatalf("second = %q at %d", m.String(), m.RuneIndex)
	}

	startAt := len(prefix) + len("ab--")
	m, err = re.FindStringMatchStartingAt(input, startAt)
	if err != nil || m == nil {
		t.Fatalf("StartingAt = %v %v", m, err)
	}
	if m.RuneIndex != 29 {
		t.Fatalf("StartingAt index = %d", m.RuneIndex)
	}
}

func TestFindAllStringIndexUnicodeAfterSlice(t *testing.T) {
	re := MustCompile(`ab`)
	prefix := strings.Repeat("漢", 25)
	input := prefix + "ab--ab"
	got, err := re.FindAllStringIndex(input, -1)
	if err != nil {
		t.Fatal(err)
	}
	want0 := len(prefix)
	want1 := want0 + len("ab--")
	want := [][]int{{want0, want0 + 2}, {want1, want1 + 2}}
	if len(got) != len(want) {
		t.Fatalf("got %v, want %v", got, want)
	}
	for i := range want {
		if got[i][0] != want[i][0] || got[i][1] != want[i][1] {
			t.Fatalf("got %v, want %v", got, want)
		}
	}
}

func TestFindStringMatchBoundaryIndexes(t *testing.T) {
	re := MustCompile(`\bcat\b`)
	input := strings.Repeat("x", 80) + " cat "
	m, err := re.FindStringMatch(input)
	if err != nil || m == nil {
		t.Fatalf("match = %v, %v", m, err)
	}
	if m.RuneIndex != 81 || m.String() != "cat" {
		t.Fatalf("got RuneIndex=%d String=%q", m.RuneIndex, m.String())
	}
	idx, length := m.ByteRange()
	if idx != 81 || length != 3 {
		t.Fatalf("ByteRange = %d, %d", idx, length)
	}
}

func TestFindStringMatchLookbehindIndexes(t *testing.T) {
	re := MustCompile(`(?<=z)foo`)
	input := strings.Repeat("y", 40) + "zfoo"
	m, err := re.FindStringMatch(input)
	if err != nil || m == nil {
		t.Fatalf("match = %v, %v", m, err)
	}
	if m.RuneIndex != 41 || m.String() != "foo" {
		t.Fatalf("got RuneIndex=%d String=%q", m.RuneIndex, m.String())
	}
}

func TestMatchStringAgreesWithFindStringMatch(t *testing.T) {
	tests := []struct {
		pattern string
		input   string
	}{
		{`needle`, strings.Repeat("x", 100) + "needle"},
		{`\bcat\b`, strings.Repeat("漢", 20) + " cat "},
		{`\bcat\b`, strings.Repeat("x", 100) + "cat"},
		{`(?<=x)foo`, strings.Repeat("z", 40) + "xfoo"},
		{`(?<=x)foo`, strings.Repeat("z", 40) + "yfoo"},
		{`apple|tiger`, strings.Repeat("z", 40) + "tiger9"},
		{`\Gfoo`, "xxfoo"},
		{`\Gfoo`, strings.Repeat("x", 80) + "foo"},
		{`(?m)^foo`, "foo\nfoo"},
	}
	for _, tt := range tests {
		re := MustCompile(tt.pattern)
		ok, err := re.MatchString(tt.input)
		if err != nil {
			t.Fatalf("%s MatchString(%q): %v", tt.pattern, tt.input, err)
		}
		m, err := re.FindStringMatch(tt.input)
		if err != nil {
			t.Fatalf("%s FindStringMatch(%q): %v", tt.pattern, tt.input, err)
		}
		if ok != (m != nil) {
			t.Fatalf("%s on %q: MatchString=%v FindStringMatch=%v", tt.pattern, tt.input, ok, m != nil)
		}
	}
}

func TestNegatedClassNotConvertedToPrefixes(t *testing.T) {
	re := MustCompile(`a[^bc]d`)
	if re.code.FindOptimizations != nil && re.code.FindOptimizations.FindMode == syntax.LeadingStrings_LeftToRight {
		t.Fatalf("negated class was treated as leading strings: %v", re.code.FindOptimizations.LeadingPrefixes)
	}
	ok, err := re.MatchString("aed")
	if err != nil || !ok {
		t.Fatalf("MatchString = %v, %v", ok, err)
	}
}

func TestInterpreterLeadingStrings(t *testing.T) {
	re := MustCompile(`(?:apple|tiger)\d+`)
	if got, want := re.code.FindOptimizations.FindMode, syntax.LeadingStrings_LeftToRight; got != want {
		t.Fatalf("FindMode = %v, want %v", got, want)
	}
	ok, err := re.MatchString(strings.Repeat("z", 200) + "tiger9")
	if err != nil || !ok {
		t.Fatalf("expected match, got %v %v", ok, err)
	}
}

func TestRegisteredEngineUnknownLeftContextDoesNotSlice(t *testing.T) {
	const pattern = `registered-unknown-left-context`
	RegisterEngine(pattern, RuntimeEngineData{
		CapSize: 1,
		StringPrefixFilter: func(input string, startAt int) (int, bool) {
			return strings.Index(input[startAt:], "foo") + startAt, true
		},
		FindFirstChar: func(r *Runner) bool { return true },
		Execute: func(r *Runner) error {
			if r.Runtextpos+3 > len(r.Runtext) || string(r.Runtext[r.Runtextpos:r.Runtextpos+3]) != "foo" {
				return nil
			}
			// \b: previous rune must be absent or a non-word character.
			if r.Runtextpos > 0 && syntax.IsWordChar(r.Runtext[r.Runtextpos-1]) {
				return nil
			}
			r.Capture(0, r.Runtextpos, r.Runtextpos+3)
			return nil
		},
	})

	re := MustCompile(pattern)
	if d := re.decodeFrom(strings.Repeat("x", 20)+"foo", 20); d != 0 {
		t.Fatalf("unspecified left context sliced from %d, want 0", d)
	}
	ok, err := re.MatchString("xxxfoo")
	if err != nil {
		t.Fatalf("MatchString: %v", err)
	}
	if ok {
		t.Fatal("expected no match for \\bfoo against xxxfoo")
	}
}

func TestRegisteredEngineKnownLeftContextSlices(t *testing.T) {
	const pattern = `registered-known-left-context`
	RegisterEngine(pattern, RuntimeEngineData{
		CapSize:          1,
		LeftContextKnown: true,
		LeftContextRunes: 0,
		StringPrefixFilter: func(input string, startAt int) (int, bool) {
			idx := strings.Index(input[startAt:], "needle")
			if idx < 0 {
				return 0, false
			}
			return startAt + idx, true
		},
		FindFirstChar: func(r *Runner) bool { return true },
		Execute: func(r *Runner) error {
			if r.Runtextpos+6 > len(r.Runtext) || string(r.Runtext[r.Runtextpos:r.Runtextpos+6]) != "needle" {
				return nil
			}
			r.Capture(0, r.Runtextpos, r.Runtextpos+6)
			return nil
		},
	})

	re := MustCompile(pattern)
	input := strings.Repeat("x", 100) + "needle"
	if got := re.decodeFrom(input, 100); got != 100 {
		t.Fatalf("decodeFrom = %d, want 100", got)
	}
	ok, err := re.MatchString(input)
	if err != nil || !ok {
		t.Fatalf("MatchString = %v, %v", ok, err)
	}
}
