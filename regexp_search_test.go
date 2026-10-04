package regexp2

import (
	"strings"
	"testing"
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
