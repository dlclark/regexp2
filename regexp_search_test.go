package regexp2

import (
	"strconv"
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

func TestRequiredLandmarkChainSearchReusesWork(t *testing.T) {
	for _, tc := range []struct {
		pattern, input string
	}{
		// One alternative's walk says nothing about another's.
		{`\S*(\t|\s*b|\s*c)a()`, "aa ca"},
		// A walk that stops at a rune outside the whitespace says nothing
		// about cores after that rune.
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
