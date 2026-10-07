package regexp2

import (
	"errors"
	"fmt"
	"slices"
	"strconv"
	"strings"
	"testing"
	"time"
	"unicode"

	"github.com/dlclark/regexp2/v2/syntax"
)

// Test262 has hundreds of RegExp tests under test/built-ins/RegExp. These
// tables intentionally distill pattern-level scenarios that map to regexp2's
// API rather than importing the JavaScript harness or one file per scenario.
//
// Suites/features deliberately left as review markers:
//   - RegExp constructor/prototype object semantics, Symbol.match/search/split,
//     lastIndex, source, flags, species, and property descriptor tests.
//   - Unsupported ECMAScript flags and feature suites: /v Unicode sets, /d match
//     indices, /y sticky matching, and regexp modifier groups such as (?i:...).
//   - RegExp.escape, because regexp2 does not expose the ECMAScript built-in.
//   - Unicode sets and Unicode-string properties requiring /v. Property escapes
//     in /u mode are covered below.

type test262ExecCase struct {
	source    string
	expr      string
	input     string
	opt       RegexOptions
	want      []string
	undefined []int
	index     int
}

func TestECMA_Test262ExecScenarios(t *testing.T) {
	// RepeatMatcher cases include the specification's capture-clearing and
	// empty-backreference examples, plus these Test262 regressions:
	// https://github.com/tc39/test262/blob/main/test/built-ins/RegExp/S15.10.2.5_A1_T4.js
	// https://github.com/tc39/test262/blob/main/test/built-ins/RegExp/S15.10.2.5_A1_T5.js
	// https://github.com/tc39/test262/blob/main/test/built-ins/RegExp/nullable-quantifier.js
	// https://github.com/tc39/test262/blob/main/test/built-ins/RegExp/lookahead-quantifier-match-groups.js
	tests := []test262ExecCase{
		{
			source:    "S15.10.2.3_A1_T15.js",
			expr:      `(Rob)|(Bob)|(Robert)|(Bobby)`,
			input:     "Hi Bob",
			want:      []string{"Bob", "", "Bob", "", ""},
			undefined: []int{1, 3, 4},
			index:     3,
		},
		{
			source: "S15.10.2.5_A1_T2.js",
			expr:   `a[a-z]{2,4}?`,
			input:  "abcdefghi",
			want:   []string{"abc"},
		},
		{
			source: "S15.10.2.5_A1_T3.js",
			expr:   `(aa|aabaac|ba|b|c)*`,
			input:  "aabaac",
			want:   []string{"aaba", "ba"},
		},
		{
			source:    "S15.10.2.5_A1_T4.js",
			expr:      `(z)((a+)?(b+)?(c))*`,
			input:     "zaacbbbcac",
			want:      []string{"zaacbbbcac", "z", "ac", "a", "", "c"},
			undefined: []int{4},
		},
		{
			source: "S15.10.2.5_A1_T5.js",
			expr:   `(a*)b\1+`,
			input:  "baaaac",
			want:   []string{"b", ""},
		},
		{
			source: "nullable-quantifier.js",
			expr:   `(a?b??)*`,
			input:  "ab",
			want:   []string{"ab", "b"},
		},
		{
			source: "lookahead-quantifier-match-groups.js/unquantified",
			expr:   `(?:(?=(abc)))a`,
			input:  "abc",
			want:   []string{"a", "abc"},
		},
		{
			source:    "lookahead-quantifier-match-groups.js/optional",
			expr:      `(?:(?=(abc)))?a`,
			input:     "abc",
			want:      []string{"a", ""},
			undefined: []int{1},
		},
		{
			source: "lookahead-quantifier-match-groups.js/required",
			expr:   `(?:(?=(abc))){1,1}a`,
			input:  "abc",
			want:   []string{"a", "abc"},
		},
		{
			source:    "lookahead-quantifier-match-groups.js/bounded-optional",
			expr:      `(?:(?=(abc))){0,1}a`,
			input:     "abc",
			want:      []string{"a", ""},
			undefined: []int{1},
		},
		{
			source: "S15.10.2.6_A1_T2.js",
			expr:   `e$`,
			input:  "pairs\nmakes\tdouble",
			want:   []string{"e"},
			index:  17,
		},
		{
			source: "S15.10.2.6_A2_T2.js",
			expr:   `^m`,
			input:  "pairs\nmakes\tdouble",
			opt:    Multiline,
			want:   []string{"m"},
			index:  6,
		},
		{
			source: "S15.10.2.6_A2_T10.js",
			expr:   `^\d+`,
			input:  "abc\n123xyz",
			opt:    Multiline,
			want:   []string{"123"},
			index:  4,
		},
		{
			source: "S15.10.2.6_A3_T8.js",
			expr:   `\bro`,
			input:  "pilot\nsoviet robot\topenoffice",
			want:   []string{"ro"},
			index:  13,
		},
		{
			source: "S15.10.2.6_A4_T4.js",
			expr:   `\B\w\B`,
			input:  "devils arise\tfor\nrevil",
			want:   []string{"e"},
			index:  1,
		},
		{
			source: "S15.10.2.7_A1_T4.js",
			expr:   `\d{2,4}`,
			input:  "the Fahrenheit 451 book",
			want:   []string{"451"},
			index:  15,
		},
		{
			source: "S15.10.2.7_A3_T8.js",
			expr:   `[a-z]+(\d+)`,
			input:  "__abc123.0",
			want:   []string{"abc123", "123"},
			index:  2,
		},
		{
			source: "S15.10.2.7_A4_T10.js",
			expr:   `d*`,
			input:  "abcddddefg",
			want:   []string{""},
		},
		{
			source: "S15.10.2.7_A4_T14.js",
			expr:   `(\d*)(\d+)`,
			input:  "1234567890",
			want:   []string{"1234567890", "123456789", "0"},
		},
		{
			source: "S15.10.2.7_A4_T15.js",
			expr:   `(\d*)\d(\d+)`,
			input:  "1234567890",
			want:   []string{"1234567890", "12345678", "0"},
		},
		{
			source: "S15.10.2.8_A1_T2.js",
			expr:   `(?=(a+))a*b\1`,
			input:  "baaabac",
			want:   []string{"aba", "a"},
			index:  3,
		},
		{
			source: "S15.10.2.8_A2_T5.js",
			expr:   `Java(?!Script)([A-Z]\w*)`,
			input:  "JavaScr oops ipt ",
			want:   []string{"JavaScr", "Scr"},
		},
		{
			source: "S15.10.2.8_A2_T7.js",
			expr:   `(\.(?!com|org)|/)`,
			input:  "ah/info",
			want:   []string{"/", "/"},
			index:  2,
		},
		{
			source: "S15.10.2.8_A3_T8.js",
			expr:   `(aa).+\1`,
			input:  "aabcdaabcd",
			want:   []string{"aabcdaa", "aa"},
		},
		{
			source: "S15.10.2.13_A1_T10.js",
			expr:   `[a-c\d]+`,
			input:  "\n\nabc324234\n",
			want:   []string{"abc324234"},
			index:  2,
		},
		{
			source: "S15.10.2.13_A1_T13.js",
			expr:   `[a-z][^1-9][a-z]`,
			input:  "a1b  b2c  c3d  def  f4g",
			want:   []string{"def"},
			index:  15,
		},
		{
			source: "S15.10.2.13_A1_T15.js",
			expr:   `[\d][\n][^\d]`,
			input:  "line1\nline2",
			want:   []string{"1\nl"},
			index:  4,
		},
		{
			source: "S15.10.2.13_A2_T1.js",
			expr:   `[^]a`,
			input:  "a\naba",
			opt:    Multiline,
			want:   []string{"\na"},
			index:  1,
		},
		{
			source: "S15.10.2.13_A3_T1.js",
			expr:   `.[\b].`,
			input:  "abc\bdef",
			want:   []string{"c\bd"},
			index:  2,
		},
		{
			source: "S15.10.2.13_A3_T4.js",
			expr:   `[^\[\b\]]+`,
			input:  "abcdef",
			want:   []string{"abcdef"},
		},
	}

	for _, options := range []RegexOptions{ECMAScript, ECMAScript | Unicode} {
		for _, tt := range tests {
			t.Run(tt.source+"/"+strconv.Itoa(int(options)), func(t *testing.T) {
				re := MustCompile(tt.expr, options|tt.opt)
				match, err := re.FindStringMatch(tt.input)
				if err != nil {
					t.Fatal(err)
				}
				if match == nil {
					t.Fatal("expected match, got none")
				}
				if match.RuneIndex != tt.index {
					t.Fatalf("expected index %d, got %d", tt.index, match.RuneIndex)
				}

				groups := match.Groups()
				if len(groups) != len(tt.want) {
					t.Fatalf("expected %d groups, got %d", len(tt.want), len(groups))
				}

				undefined := map[int]bool{}
				for _, group := range tt.undefined {
					undefined[group] = true
				}
				for i, want := range tt.want {
					if undefined[i] {
						if len(groups[i].Captures) != 0 {
							t.Fatalf("group %d expected undefined, got %q", i, groups[i].String())
						}
						continue
					}
					if len(groups[i].Captures) == 0 {
						t.Fatalf("group %d expected %q, got undefined", i, want)
					}
					if got := groups[i].String(); got != want {
						t.Fatalf("group %d expected %q, got %q", i, want, got)
					}
				}
			})
		}
	}
}

func TestECMA_Test262CompileErrors(t *testing.T) {
	tests := []struct {
		source string
		expr   string
	}{
		{source: "S15.10.1_A1_T1.js", expr: `a**`},
		{source: "S15.10.1_A1_T4.js", expr: `a+++`},
		{source: "S15.10.1_A1_T9.js", expr: `+a`},
		{source: "S15.10.1_A1_T11.js", expr: `?a`},
		{source: "S15.10.1_A1_T13.js", expr: `x{1}{1,}`},
		{source: "S15.10.1_A1_T16.js", expr: `x{0,1}{1,}`},
		{source: "S15.10.4.1_A9_T2.js", expr: `[{-z]`},
		{source: "S15.10.4.1_A9_T3.js", expr: `[a--z]`},
		{source: "invalid-optional-lookbehind.js", expr: `.(?<=.)?`},
		{source: "invalid-optional-negative-lookbehind.js", expr: `.(?<!.)?`},
		{source: "invalid-range-lookbehind.js", expr: `.(?<=.){2,3}`},
		{source: "invalid-range-negative-lookbehind.js", expr: `.(?<!.){2,3}`},
	}

	for _, tt := range tests {
		t.Run(tt.source, func(t *testing.T) {
			if _, err := Compile(tt.expr, ECMAScript); err == nil {
				t.Fatalf("expected compile error for %q", tt.expr)
			}
		})
	}
}

func TestECMA_CharSetRange(t *testing.T) {
	tests := map[string]struct {
		expr    string
		data    string
		opt     RegexOptions
		want    []string
		wantErr string
	}{
		"basic": {
			expr: `[a-c]`,
			data: "abcd",
			want: []string{"a", "b", "c"},
		},
		"in-range": {
			expr: `[a-\s\b]`,
			data: "a-b cd",
			want: []string{"a", "-", " "},
		},
		"space": {
			expr: `[a-\s]`,
			data: "a-b cd",
			want: []string{"a", "-", " "},
		},
		"word": {
			expr: `[a-\w]`,
			data: "a-b cd",
			want: []string{"a", "-", "b", "c", "d"},
		},
		"digit": {
			expr: `[a-\d]`,
			data: "a-b1 cd",
			want: []string{"a", "-", "1"},
		},
		"slash-p": {
			expr: `[a-\p]`,
			data: "a-bq cd",
			want: []string{"a", "b", "c", "d"},
		},
		"slash-p-literal": {
			expr: `[a-\p{x}]`,
			data: "a-bq cdx",
			want: []string{"a", "b", "c", "d", "x"},
		},
		"invalid-unicode": {
			expr:    `[a-\p]`,
			opt:     Unicode,
			wantErr: "error parsing regexp: incomplete \\p{X} character escape in `[a-\\p]`",
		},
		"invalid-unicode-letter": {
			expr:    `[a-\p{L}]`,
			opt:     Unicode,
			wantErr: "error parsing regexp: cannot create range with shorthand escape sequence \\p in `[a-\\p{L}]`",
		},
		"invalid-slash-P": {
			expr:    `[a-\P]`,
			wantErr: "error parsing regexp: cannot create range with shorthand escape sequence \\P in `[a-\\P]`",
		},
		"ordered-space": {
			expr: `[\s-z]`,
			data: "a-b czd",
			want: []string{"-", " ", "z"},
		},
		"ordered-word": {
			expr: `[\w-z]`,
			data: "a-b czd",
			want: []string{"a", "-", "b", "c", "z", "d"},
		},
		"ordered-digit": {
			expr: `[\d-z]`,
			data: "a- 0zd",
			want: []string{"-", "0", "z"},
		},
		"ordered-point": {
			expr: `[\p-z]`,
			data: "a-b szd",
			want: []string{"-", "s", "z"},
		},
	}

	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			re, err := Compile(tt.expr, tt.opt|ECMAScript)
			if tt.wantErr != "" {
				if err == nil {
					t.Fatalf("expected error %q, got none", tt.wantErr)
				}
				if err.Error() != tt.wantErr {
					t.Fatalf("expected error %q, got %q", tt.wantErr, err.Error())
				}
				return
			}
			if err != nil {
				t.Fatal(err)
			}

			match, err := re.FindStringMatch(tt.data)
			if err != nil {
				t.Fatal(err)
			}

			var res []string
			for match != nil {
				for _, g := range match.Groups() {
					for _, c := range g.Captures {
						res = append(res, c.String())
					}
				}

				match, err = re.FindNextMatch(match)
				if err != nil {
					t.Fatal(err)
				}
			}

			if !slices.Equal(tt.want, res) {
				t.Fatalf("wanted %v got %v", tt.want, res)
			}
		})
	}
}

func TestEMCAScriptUnicodeSets(t *testing.T) {
	// test unicode sets with and without the unicode flag
	// also test Unicode Category Aliases
	tests := []struct {
		expr           string
		data           string
		wantUnicode    string
		wantNonUnicode string
	}{
		{
			expr:           `\p{L}+`,
			data:           "abc",
			wantUnicode:    "abc",
			wantNonUnicode: "",
		},
		{
			expr:           `\p{Letter}+`,
			data:           "abc\u00E9",
			wantUnicode:    "abc\u00E9",
			wantNonUnicode: "",
		},
		{
			expr:           `\p{L}`,
			data:           "p{L}",
			wantUnicode:    "p",
			wantNonUnicode: "p{L}",
		},
		{
			expr:           `\p{digit}`,
			data:           "abc1",
			wantUnicode:    "1",
			wantNonUnicode: "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.expr, func(t *testing.T) {
			// first check ECMAScript with Unicode flag
			re := MustCompile(tt.expr, ECMAScript|Unicode)

			match, err := re.FindStringMatch(tt.data)
			if err != nil {
				t.Fatal(err)
			}
			if tt.wantUnicode == "" {
				if match != nil {
					t.Fatalf("expected no match unicode, got one: %q", match.String())
				}
			} else {
				if match == nil {
					t.Fatal("expected match unicode, got none")
				}
				if got := match.String(); got != tt.wantUnicode {
					t.Fatalf("expected unicode %q, got %q", tt.wantUnicode, got)
				}
			}

			// validate behavior of ECMAScript without Unicode flag
			re = MustCompile(tt.expr, ECMAScript)
			match, err = re.FindStringMatch(tt.data)
			if err != nil {
				t.Fatal(err)
			}
			if tt.wantNonUnicode == "" {
				if match != nil {
					t.Fatalf("expected no match non-unicode, got one: %q", match.String())
				}
			} else {
				if match == nil {
					t.Fatal("expected match non-unicode, got none")
				}
				if got := match.String(); got != tt.wantNonUnicode {
					t.Fatalf("expected non-unicode %q, got %q", tt.wantNonUnicode, got)
				}
			}
		})
	}

}

// These cases exercise the property-escape contract in ECMA-262 and Test262's
// built-ins/RegExp/property-escapes suite through the public regexp API.
// They include every valid property example from https://github.com/dlclark/regexp2/issues/113.
func TestECMAUnicodePropertyEscapes(t *testing.T) {
	testECMAUnicodePropertyCases(t, []ecmaPropertyCase{
		{"General_Category=Lowercase_Letter", "a", "A"},
		{"gc=Ll", "a", "0"},
		{"L", "α", "0"},
		{"Script=Latin", "a", "α"},
		{"sc=Latn", "a", "α"},
		{"scx=Grek", "α", "a"},
		{"Script=Greek", "α", "a"},
		{"sc=Grek", "α", "a"},
		{"Script_Extensions=Hiragana", "ー", "a"},
		{"scx=Hira", "ー", "a"},
		{"Alphabetic", "a", "0"},
		{"Alpha", "α", "0"},
		{"ASCII", "a", "α"},
		{"ASCII_Hex_Digit", "f", "g"},
		{"Hex", "Ｆ", "Ｇ"},
		{"Any", "\U0010ffff", ""},
		{"Assigned", "a", "\u0378"},
		{"Cn", "\u0378", "a"},
		{"Other", "\U0010ffff", "a"},
		{"Cased_Letter", "ǅ", "0"},
		{"digit", "٣", "a"},
		{"Script=Hiragana", "あ", "ー"},
		{"Script=Common", "ー", "あ"},
		{"Script_Extensions=Common", ".", "ー"},
		{"sc=Zzzz", "\u0378", "a"},
		{"scx=Unknown", "\u0378", "a"},
		{"scx=Zzzz", "\U0010ffff", "a"},
		{"sc=Hrkt", "", "あ"}, // A valid script alias with an empty set.
		{"Bidi_M", "(", "a"},
		{"Lower", "a", "A"},
		{"CWKCF", "Ａ", "a"},
		{"ID_Start", "α", "0"},
		{"ID_Continue", "0", "-"},
		{"XIDS", "α", "0"},
		{"Gr_Ext", "\u0300", "a"},
		{"Emoji", "😀", "a"},
		{"EComp", "\u200d", "a"},
		{"ExtPict", "😀", "a"},
		{"space", "\u0085", "\ufeff"},
	})
}

type ecmaPropertyCase struct {
	property, yes, no string
}

func testECMAUnicodePropertyCases(t *testing.T, cases []ecmaPropertyCase) {
	t.Helper()
	for _, tt := range cases {
		t.Run(tt.property, func(t *testing.T) {
			for _, form := range []string{`\p{%s}`, `[\p{%s}]`, `\P{%s}`, `[\P{%s}]`, `[^\p{%s}]`} {
				negated := form == `\P{%s}` || form == `[\P{%s}]` || form == `[^\p{%s}]`
				pattern := "^" + fmt.Sprintf(form, tt.property) + "$"
				re, err := Compile(pattern, ECMAScript|Unicode)
				if err != nil {
					t.Fatal(err)
				}
				for _, input := range []string{tt.yes, tt.no} {
					if input == "" {
						continue
					}
					want := (input == tt.yes) != negated
					got, err := re.MatchString(input)
					if err != nil || got != want {
						t.Errorf("%s.MatchString(%q) = %v, %v; want %v", pattern, input, got, err, want)
					}
				}
			}
		})
	}
}

// Property membership should follow the standard library shipped with the
// consumer's Go version, including assignments and category corrections.
func TestECMAUnicodePropertiesFollowGoTables(t *testing.T) {
	for _, tt := range []struct {
		property string
		char     rune
		want     bool
	}{
		{"L", '\uA7CB', unicode.IsLetter('\uA7CB')},
		{"Ll", '\u0295', unicode.Is(unicode.Ll, '\u0295')},
		{"sc=Latin", '\uA7CB', unicode.Is(unicode.Latin, '\uA7CB')},
		{"sc=Kawi", '\U00011F5A', unicode.Is(unicode.Kawi, '\U00011F5A')},
		{"scx=Kawi", '\U00011F5A', unicode.Is(unicode.Kawi, '\U00011F5A')},
		{"Assigned", '\U00010D50', !unicode.Is(unicode.Cn, '\U00010D50')},
		{"sc=Unknown", '\U00010D50', unicode.Is(unicode.Cn, '\U00010D50')},
		{"scx=Unknown", '\U00010D50', unicode.Is(unicode.Cn, '\U00010D50')},
		{"Dash", '\U00010D6E', unicode.Is(unicode.Dash, '\U00010D6E')},
	} {
		for _, escape := range []string{`\p`, `\P`} {
			pattern := "^" + escape + "{" + tt.property + "}$"
			re := MustCompile(pattern, ECMAScript|Unicode)
			want := tt.want != (escape == `\P`)
			if got, err := re.MatchRunes([]rune{tt.char}); err != nil || got != want {
				t.Errorf("%s on %U = %v, %v; want %v with Go Unicode %s", pattern, tt.char, got, err, want, unicode.Version)
			}
		}
	}
}

func TestECMAUnicodePropertyRejectsExtensions(t *testing.T) {
	for _, property := range []string{
		"Greek", "Other_Alphabetic", "GCB=Extend", "lowercase_letter",
		"Script=greek", "script=Greek", "ASCII=Yes", "Script", "L&",
		"Script_Extensions", "General_Category", "Script=", "=Greek",
		"sc=Grek=Latn", "gc=Greek", "sc=Letter", "General_Category=letter",
		"Script-Extensions=Greek", "scx =Greek", "scx= Old_Persian",
		"Emoji=true", "Emoji=No", "Basic_Emoji", "RGI_Emoji", "Hyphen",
		"WSpace", "Gr_Extend", "InGreek", "IsGreek", "Block=ASCII", "WB=ALetter",
		"grapheme-cluster-break=extend", "lower", "EMOJI", "Math=Yes",
	} {
		t.Run(property, func(t *testing.T) {
			for _, form := range []string{`\p{%s}`, `\P{%s}`, `[\p{%s}]`, `[\P{%s}]`} {
				pattern := fmt.Sprintf(form, property)
				if _, err := Compile(pattern, ECMAScript|Unicode); err == nil {
					t.Errorf("Compile(%q) succeeded; expected an invalid Unicode property error", pattern)
				}
			}
		})
	}
}

func TestECMAUnicodePropertyComposition(t *testing.T) {
	for _, tt := range []struct {
		pattern, input string
		want           bool
	}{
		{`^[\p{Hex}\P{Hex}]$`, "𝌆", true},
		{`^[\p{ASCII}\p{sc=Greek}]+$`, "Aα0", true},
		{`^[\p{ASCII}\p{sc=Greek}]+$`, "Aあ0", false},
		{`^[\P{L}\p{sc=Greek}]+$`, "1α!", true},
		{`^[\P{L}\p{sc=Greek}]+$`, "1a!", false},
		{`^[^\P{L}\p{sc=Greek}]+$`, "漢a", true},
		{`^[^\P{L}\p{sc=Greek}]+$`, "漢α", false},
		{`^(?:\p{L}|\p{Emoji}|!)+$`, "漢😀!", true},
		{`^(?:\p{L}|\p{Emoji}|!)+$`, "漢😀?", false},
		{`^[^\P{ASCII}]$`, "α", false},
		{`^\P{Any}$`, "a", false},
		{`^\p{sc=Hrkt}$`, "あ", false},
		{`^\P{sc=Hrkt}$`, "\U0010ffff", true},
		{`^\p{Any}$`, "\U0010ffff", true},
		{`^\p{scx=Zzzz}$`, "\U0010ffff", true},
	} {
		re := MustCompile(tt.pattern, ECMAScript|Unicode)
		got, err := re.MatchString(tt.input)
		if err != nil || got != tt.want {
			t.Errorf("%s on %q = %v, %v; want %v", tt.pattern, tt.input, got, err, tt.want)
		}
	}
	// Go strings cannot encode a lone surrogate; the rune API can represent it.
	re := MustCompile(`^\p{Surrogate}$`, ECMAScript|Unicode)
	if got, err := re.MatchRunes([]rune{0xd800}); err != nil || !got {
		t.Fatalf("Surrogate: %v, %v", got, err)
	}
}

func TestECMAUnicodePropertyModeIsolation(t *testing.T) {
	for _, tt := range []struct{ pattern, input string }{
		{`\p{Greek}`, "α"},
		{`\p{Other_Alphabetic}`, "\u0345"},
		{`\p{grapheme-cluster-break=extend}`, "\u0300"},
	} {
		for _, options := range []RegexOptions{None, RE2, Unicode} {
			re := MustCompile(tt.pattern, options)
			if got, err := re.MatchString(tt.input); err != nil || !got {
				t.Errorf("%s options %v: %v, %v", tt.pattern, options, got, err)
			}
		}
	}
	re := MustCompile(`^\p{Script=Greek}$`, ECMAScript)
	if got, err := re.MatchString("p{Script=Greek}"); err != nil || !got {
		t.Fatalf("non-Unicode identity escape: %v, %v", got, err)
	}
}

func TestECMAUnicodePropertyRangeEndpoints(t *testing.T) {
	for _, pattern := range []string{`[\p{Hex}-\uFFFF]`, `[\p{Hex}--]`, `[a-\p{Hex}]`, `[\p{ASCII}-\p{ASCII}]`} {
		if _, err := Compile(pattern, ECMAScript|Unicode); err == nil {
			t.Errorf("Compile(%q) succeeded; a property escape cannot be a range endpoint", pattern)
		}
	}
	for _, pattern := range []string{`^[\p{Hex}-]$`, `^[-\p{Hex}]$`} {
		re := MustCompile(pattern, ECMAScript|Unicode)
		for _, input := range []string{"-", "F"} {
			if got, err := re.MatchString(input); err != nil || !got {
				t.Errorf("%s on %q = %v, %v", pattern, input, got, err)
			}
		}
	}
}

func TestECMAUnicodePropertyIgnoreCase(t *testing.T) {
	for _, tt := range []struct {
		pattern, input string
		want           bool
	}{
		{`^\p{Lu}$`, "a", true},
		{`^\p{Ll}$`, "A", true},
		{`^\p{Lowercase_Letter}$`, "İ", false},
		{`^\p{ASCII}$`, "K", true},
		{`^\p{ASCII}$`, "ſ", true},
		{`^\P{ASCII}$`, "K", true},
		{`^\P{ASCII}$`, "a", false},
		{`^\P{Lowercase_Letter}$`, "a", true},
		{`^[\P{Lowercase_Letter}]$`, "a", true},
		{`^[^\p{Lowercase_Letter}]$`, "a", false},
		{`^[^\P{Lowercase_Letter}]$`, "a", false},
		{`^\p{sc=Greek}$`, "µ", true},
		{`^[\p{Ll}\p{Emoji}]+$`, "AΣ😀", true},
		{`^[\p{Ll}\p{Emoji}]+$`, "AΣ!", false},
		{`^[^\P{Ll}\p{Emoji}]+$`, "AΣ😀", false},
	} {
		re := MustCompile(tt.pattern, ECMAScript|Unicode|IgnoreCase)
		if got, err := re.MatchString(tt.input); err != nil || got != tt.want {
			t.Errorf("%s on %q = %v, %v; want %v", tt.pattern, tt.input, got, err, tt.want)
		}
	}
}

func BenchmarkECMAUnicodeProperties(b *testing.B) {
	for _, tc := range []struct{ name, pattern, input string }{
		{"ASCII", `^\p{ASCII}+$`, strings.Repeat("a", 512)},
		{"LettersASCII", `^\p{L}+$`, strings.Repeat("a", 512)},
		{"LettersGreek", `^\p{L}+$`, strings.Repeat("α", 512)},
		{"LettersHan", `^\p{L}+$`, strings.Repeat("漢", 512)},
		{"Emoji", `^\p{Emoji}+$`, strings.Repeat("😀", 512)},
		{"FoldedLetters", `(?i)^\p{Ll}+$`, strings.Repeat("Σ", 512)},
		{"FoldedComplement", `(?i)^\P{Ll}+$`, strings.Repeat("Σ", 512)},
	} {
		b.Run(tc.name, func(b *testing.B) {
			re := MustCompile(tc.pattern, ECMAScript|Unicode)
			b.Run("Compile", func(b *testing.B) {
				b.ReportAllocs()
				for b.Loop() {
					if _, err := Compile(tc.pattern, ECMAScript|Unicode); err != nil {
						b.Fatal(err)
					}
				}
			})
			b.Run("Match", func(b *testing.B) {
				b.ReportAllocs()
				for b.Loop() {
					if got, err := re.MatchString(tc.input); err != nil || !got {
						b.Fatal(got, err)
					}
				}
			})
		})
	}
}

func TestECMADuplicateNamesInAlternatives(t *testing.T) {
	// Matching and numeric captures adapt Test262's duplicate-names-exec.js:
	// https://github.com/tc39/test262/blob/main/test/built-ins/RegExp/named-groups/duplicate-names-exec.js
	// GroupNumberFromName assertions cover regexp2's static API policy.
	re, err := Compile(`(?<x>b)|(?<x>a)`, ECMAScript)
	if err != nil {
		t.Fatal(err)
	}
	if got := re.GetGroupNumbers(); !slices.Equal(got, []int{0, 1, 2}) {
		t.Fatalf("group numbers = %v, want [0 1 2]", got)
	}
	if got := re.GetGroupNames(); !slices.Equal(got, []string{"", "x", "x"}) {
		t.Fatalf("group names = %q, want [\"\" \"x\" \"x\"]", got)
	}
	if got := re.GroupNumberFromName("x"); got != 1 {
		t.Fatalf("GroupNumberFromName(x) = %d, want 1", got)
	}
	for _, tc := range []struct {
		input string
		slot  int
	}{
		{"bab", 1},
		{"a", 2},
	} {
		t.Run(tc.input, func(t *testing.T) {
			m, err := re.FindStringMatch(tc.input)
			if err != nil || m == nil {
				t.Fatalf("FindStringMatch = %v, %v; want match", m, err)
			}
			if m.GroupByName("x") != m.GroupByNumber(tc.slot) {
				t.Fatalf("GroupByName(x) does not select group %d", tc.slot)
			}
			groups := m.Groups()
			if len(groups) != 3 {
				t.Fatalf("group count = %d, want 3", len(groups))
			}
			for i := 1; i <= 2; i++ {
				if groups[i].Name != "x" || re.GroupNameFromNumber(i) != "x" {
					t.Fatalf("group %d should be named x", i)
				}
				if got := len(groups[i].Captures); (got != 0) != (i == tc.slot) {
					t.Fatalf("group %d capture count = %d", i, got)
				}
			}
		})
	}
}

func TestECMADuplicateNamesValidation(t *testing.T) {
	// The same-alternative case comes from Test262:
	// https://github.com/tc39/test262/blob/main/test/built-ins/RegExp/duplicate-named-capturing-groups-syntax.js
	// Remaining cases exercise ECMAScript 2025 §22.2.1.4 MightBothParticipate
	// across nesting, sequential disjunctions, and normalized group names.
	for _, options := range []RegexOptions{ECMAScript, ECMAScript | Unicode} {
		for _, pattern := range []string{
			`(?<x>a)(?<x>b)`,
			`(?<x>(?<x>a))`,
			`(?<x>a)|(?<x>b)(?<x>c)`,
			`(?:(?<x>a)|b)(?:c|(?<x>d))`,
			`(?=(?<x>a))(?<x>a)`,
			`(?<x>a){0}(?<x>b)`,
			`(?<x>a)(?<\u0078>b)`,
		} {
			t.Run(pattern+"/"+strconv.Itoa(int(options)), func(t *testing.T) {
				_, err := Compile(pattern, options)
				var parseErr *syntax.Error
				if !errors.As(err, &parseErr) || parseErr.Code != syntax.ErrDuplicateGroupName {
					t.Fatalf("Compile = %v, want duplicate group name error", err)
				}
			})
		}
	}
}

func TestECMADuplicateNamesMatching(t *testing.T) {
	// Basic named backreferences adapt these Test262 cases:
	// https://github.com/tc39/test262/blob/main/test/built-ins/RegExp/named-groups/duplicate-names-exec.js
	// https://github.com/tc39/test262/blob/main/test/built-ins/RegExp/named-groups/duplicate-names-test.js
	// Additional rows cover numeric references, lookarounds, and RepeatMatcher
	// capture resets (ECMAScript 2025 §22.2.2.3.1, step 4).
	for _, options := range []RegexOptions{ECMAScript, ECMAScript | Unicode} {
		for _, tc := range []struct {
			pattern string
			input   string
			want    []string
			slot    int
		}{
			// Exact backreference sample from issue #116 (also in Test262's
			// duplicate-names-exec.js linked above).
			// https://github.com/dlclark/regexp2/issues/116
			{`(?:(?<x>a)|(?<x>b))\k<x>`, "bb", []string{"bb", "", "b"}, 2},
			{`^(?:(?<x>a)|(?<x>b))\k<x>$`, "bb", []string{"bb", "", "b"}, 2},
			{`^(?:(?<x>a)|(?<x>b))\k<x>$`, "aa", []string{"aa", "a", ""}, 1},
			{`^(?:(?<x>a)|(?<x>b))\k<x>$`, "ab", nil, 0},
			{`^(?:(?<x>a)|(?<x>b))\1$`, "b", []string{"b", "", "b"}, 2},
			{`^(?:(?<x>a)|(?<x>b))\2$`, "a", []string{"a", "a", ""}, 1},
			{`^(?:(?<x>a)|(?<x>b))\k<x>+$`, "bbb", []string{"bbb", "", "b"}, 2},
			{`^\k<x>(?:(?<x>a)|(?<x>b))$`, "b", []string{"b", "", "b"}, 2},
			{`^(?:(?<x>)a|(?<x>b))$`, "a", []string{"a", "", ""}, 1},
			{`^(?:(?<x>a)|(?<x>b)|c)$`, "c", []string{"c", "", ""}, 0},
			{`^(?:(?<x>a)|(?:(?<x>b)|(?<x>c)))\k<x>$`, "cc", []string{"cc", "", "", "c"}, 3},
			{`^(?:(?<x>a)|(?<\u0078>b))\k<\u{78}>$`, "bb", []string{"bb", "", "b"}, 2},
			{`^(?=(?<x>a)|(?<x>b))\k<x>$`, "b", []string{"b", "", "b"}, 2},
			{`(?<=(?<x>a)|(?<x>b))\k<x>`, "bb", []string{"b", "", "b"}, 2},
			{`^(?:(?<x>a)|(?<x>b))+\k<x>$`, "abb", []string{"abb", "", "b"}, 2},
			{`^(?:(?<x>a)|(?<x>b))+?\k<x>$`, "baa", []string{"baa", "a", ""}, 1},
			{`^(?:(?<x>a)|(?<x>b)|c)+$`, "abc", []string{"abc", "", ""}, 0},
			{`^(?:(?<x>a)|(?<x>b)){2}\k<x>$`, "baa", []string{"baa", "a", ""}, 1},
			{`^(?:(?<x>a)|(?<x>b)){1,3}\k<x>$`, "abb", []string{"abb", "", "b"}, 2},
			{`^(?:(?<x>a*)|(?<x>b*)){1,3}\k<x>$`, "baaaa", []string{"baaaa", "aa", ""}, 1},
			{`^(?:(?<x>a)|(?<x>b)?|c)+$`, "abc", []string{"abc", "", ""}, 0},
			{`^(?:(?<x>a)|(?<x>b)?)+\k<x>$`, "bb", []string{"bb", "", "b"}, 2},
			{`^(?:(?<x>a)|(?<x>b)?)*?\k<x>$`, "bb", []string{"bb", "", "b"}, 2},
			{`^(?:(?<x>a*)|(?<x>b*)){2,4}$`, "", []string{"", "", ""}, 1},
			{`^(?:(?<x>a)|(?<x>b))+b\k<x>$`, "aba", []string{"aba", "a", ""}, 1},
			{`^(?:(?<x>a)|(?<x>b))+b\k<x>$`, "abb", nil, 0},
			{`^(?:(?:(?<x>a)|(?<x>b))+|c)+\k<x>$`, "abc", []string{"abc", "", ""}, 0},
			{`^(?:(?<x>a)|(?<x>b)|(?<y>c))*\k<x>$`, "abc", []string{"abc", "", "", "c"}, 0},
			{`^(a)(?:(?<x>b)|(?<x>c))(d)\k<x>\4$`, "acdcd", []string{"acdcd", "a", "", "c", "d"}, 3},
		} {
			t.Run(tc.pattern+"/"+tc.input+"/"+strconv.Itoa(int(options)), func(t *testing.T) {
				re, err := Compile(tc.pattern, options)
				if err != nil {
					t.Fatal(err)
				}
				for _, runes := range []bool{false, true} {
					var m *Match
					if runes {
						m, err = re.FindRunesMatch([]rune(tc.input))
					} else {
						m, err = re.FindStringMatch(tc.input)
					}
					if err != nil {
						t.Fatal(err)
					}
					if tc.want == nil {
						if m != nil {
							t.Fatalf("unexpected match %q", m.String())
						}
						continue
					}
					if m == nil {
						t.Fatal("expected match")
					}
					groups := m.Groups()
					if len(groups) != len(tc.want) {
						t.Fatalf("group count = %d, want %d", len(groups), len(tc.want))
					}
					for i := range groups {
						if got := groups[i].String(); got != tc.want[i] {
							t.Fatalf("group %d = %q, want %q", i, got, tc.want[i])
						}
						if i > 0 && groups[i].Name == "x" {
							if (len(groups[i].Captures) > 0) != (i == tc.slot) {
								t.Fatalf("group %d has %d captures, active slot = %d", i, len(groups[i].Captures), tc.slot)
							}
						}
					}
					selected := tc.slot
					if selected == 0 {
						selected = re.GroupNumberFromName("x")
					}
					if m.GroupByName("x") != m.GroupByNumber(selected) {
						t.Fatalf("GroupByName(x) does not select group %d", selected)
					}
				}
				matched, err := re.MatchString(tc.input)
				if err != nil || matched != (tc.want != nil) {
					t.Fatalf("MatchString = %v, %v", matched, err)
				}
				matched, err = re.MatchRunes([]rune(tc.input))
				if err != nil || matched != (tc.want != nil) {
					t.Fatalf("MatchRunes = %v, %v", matched, err)
				}
			})
		}
	}
}

func TestECMADuplicateNamesReplacement(t *testing.T) {
	// Named versus numbered replacements adapt Test262, using regexp2's
	// ${name} replacement syntax in place of ECMAScript's $<name>:
	// https://github.com/tc39/test262/blob/main/test/built-ins/RegExp/named-groups/duplicate-names-replace.js
	for _, options := range []RegexOptions{ECMAScript, ECMAScript | RightToLeft} {
		re := MustCompile(`(?<x>a)|(?<x>b)|c`, options)
		for _, tc := range []struct{ replacement, want string }{
			{`${x}`, "ab"},
			{`${1}:${2}:${x}`, "a::a:b:b::"},
			{`$1/$2/${x}`, "a//a/b/b//"},
			{`$${x}`, "${x}${x}${x}"},
			{`${missing}`, "${missing}${missing}${missing}"},
		} {
			for i := 0; i < 2; i++ {
				got, err := re.Replace("abc", tc.replacement, -1, -1)
				if err != nil || got != tc.want {
					t.Fatalf("Replace(%q) = %q, %v; want %q", tc.replacement, got, err, tc.want)
				}
			}
		}
		got, err := re.ReplaceFunc("abc", func(m Match) string { return m.GroupByName("x").String() }, -1, -1)
		if err != nil || got != "ab" {
			t.Fatalf("ReplaceFunc = %q, %v; want ab", got, err)
		}
	}
	re := MustCompile(`^(?:(?<x>a)|(?<x>b))+\k<x>$`, ECMAScript)
	got, err := re.Replace("abb", `${1}:${2}:${x}`, -1, -1)
	if err != nil || got != ":b:b" {
		t.Fatalf("Replace repeated alternatives = %q, %v; want :b:b", got, err)
	}
}

func TestECMADuplicateNamesEmptyIterations(t *testing.T) {
	// regexp2 regression for ECMAScript 2025 §22.2.2.3.1 RepeatMatcher,
	// step 2.2: a failed optional empty iteration must preserve the captures
	// from the previous successful iteration.
	re := MustCompile(`^(?:(?<x>a)|(?<x>b)?)+$`, ECMAScript)
	m, err := re.FindStringMatch("b")
	if err != nil || m == nil {
		t.Fatalf("FindStringMatch = %v, %v; want match", m, err)
	}
	if got := m.GroupByName("x").String(); got != "b" {
		t.Fatalf("GroupByName(x) = %q, want b from the last nonempty iteration", got)
	}
}

func TestECMAQuantifiedCaptureBackreference(t *testing.T) {
	re, err := Compile(`^(a(b)?)+\2$`, ECMAScript|Unicode)
	if err != nil {
		t.Fatal(err)
	}
	re.MatchTimeout = time.Second
	matched, err := re.MatchString("aba")
	if err != nil {
		t.Fatal(err)
	}
	if !matched {
		t.Fatal(`MatchString("aba") = false, want true: the final repetition clears group 2`)
	}
}

func TestECMAQuantifiedCaptures(t *testing.T) {
	// ECMAScript RepeatMatcher clears the atom's captures before each
	// repetition, restores them on backtracking, and rejects optional empty
	// iterations. An undefined capture differs from a captured empty string.
	// https://tc39.es/ecma262/2025/multipage/text-processing.html#sec-repeatmatcher
	tests := []struct {
		name, pattern, input string
		want                 []string
		undefined            []int
	}{
		{"cleared backreference", `^(a(b)?)+\2$`, "aba", []string{"aba", "a", ""}, []int{2}},
		{"stale backreference rejected", `^(a(b)?)+\2$`, "abab", nil, nil},
		{"optional capture", `^(a(b)?)+$`, "aba", []string{"aba", "a", ""}, []int{2}},
		{"backtracking restores captures", `^(a(b)?)+a\2$`, "abab", []string{"abab", "ab", "b"}, nil},
		{"lookahead restores captures", `^(?:(?=(a(b)?))\1)+a\2$`, "abab", []string{"abab", "ab", "b"}, nil},
		{"atomic group restores captures", `^(?:(?>(a(b)?))|c)+a\2$`, "abab", []string{"abab", "ab", "b"}, nil},
		{"greedy empty iteration rejected", `^(a?)*$`, "a", []string{"a", "a"}, nil},
		{"zero repetitions", `^(a?)*$`, "", []string{"", ""}, []int{1}},
		{"lazy repetition", `^(a?)+?$`, "a", []string{"a", "a"}, nil},
		{"required empty repetition", `^(a?)+?$`, "", []string{"", ""}, nil},
		{"bounded required empty repetition", `^(a?){2,3}$`, "a", []string{"a", ""}, nil},
		{"single named capture", `^(?:(?<x>a)|b)+$`, "ab", []string{"ab", ""}, []int{1}},
	}
	for _, options := range []RegexOptions{ECMAScript, ECMAScript | Unicode} {
		for _, codegen := range []bool{false, true} {
			for _, tc := range tests {
				t.Run(fmt.Sprintf("%s/options=%d/codegen=%v", tc.name, options, codegen), func(t *testing.T) {
					compileOptions := []CompileOption{options}
					if codegen {
						compileOptions = append(compileOptions, OptionIsCodeGen())
					}
					re, err := Compile(tc.pattern, compileOptions...)
					if err != nil {
						t.Fatal(err)
					}
					re.MatchTimeout = time.Second
					matched, err := re.MatchString(tc.input)
					if err != nil || matched != (tc.want != nil) {
						t.Errorf("MatchString = %v, %v; want %v", matched, err, tc.want != nil)
					}
					m, err := re.FindStringMatch(tc.input)
					if err != nil {
						t.Fatal(err)
					}
					if tc.want == nil {
						if m != nil {
							t.Errorf("FindStringMatch = %q, want nil", m.String())
						}
						return
					}
					if m == nil {
						t.Fatal("FindStringMatch = nil, want match")
					}
					for i, want := range tc.want {
						g := m.GroupByNumber(i)
						if got := g.String(); got != want {
							t.Errorf("group %d = %q, want %q", i, got, want)
						}
						wantCaptures := 1
						if slices.Contains(tc.undefined, i) {
							wantCaptures = 0
						}
						if got := len(g.Captures); got != wantCaptures {
							t.Errorf("group %d has %d captures, want %d", i, got, wantCaptures)
						}
					}
				})
			}
		}
	}
}

func TestQuantifiedCapturesNonECMAUnchanged(t *testing.T) {
	for _, options := range []RegexOptions{None, Unicode, RE2, RE2 | Unicode} {
		t.Run(fmt.Sprint(options), func(t *testing.T) {
			re := MustCompile(`^(a(b)?)+\2$`, options)
			matched, err := re.MatchString("aba")
			if err != nil || matched {
				t.Errorf("MatchString(aba) = %v, %v; want false", matched, err)
			}
			matched, err = re.MatchString("abab")
			if err != nil || !matched {
				t.Errorf("MatchString(abab) = %v, %v; want true", matched, err)
			}
			re = MustCompile(`^(a(b)?)+$`, options)
			m, err := re.FindStringMatch("aba")
			if err != nil || m == nil {
				t.Fatalf("FindStringMatch = %v, %v; want match", m, err)
			}
			g := m.GroupByNumber(1)
			if len(g.Captures) != 2 || g.Captures[0].String() != "ab" || g.Captures[1].String() != "a" {
				t.Errorf("group 1 capture history = %v, want [ab a]", g.Captures)
			}
			g = m.GroupByNumber(2)
			if len(g.Captures) != 1 || g.String() != "b" {
				t.Errorf("group 2 capture history = %v, want [b]", g.Captures)
			}
		})
	}
}

func TestDuplicateNamesDefaultModeUnchanged(t *testing.T) {
	// Non-ECMAScript patterns retain capture history and shared group numbers.
	for _, options := range []CompileOption{None, OptionMaintainCaptureOrder()} {
		// The default-mode results in issue #116 keep one shared group.
		// https://github.com/dlclark/regexp2/issues/116
		for _, tc := range []struct {
			pattern, input, match, value string
			captures                     int
		}{
			{`(?<x>b)|(?<x>a)`, "bab", "b", "b", 1},
			{`(?<x>b)|(?<x>a)`, "a", "a", "a", 1},
			{`(?:(?<x>a)|(?<x>b))\k<x>`, "bb", "bb", "b", 1},
			{`(?<x>a)(?<x>b)`, "ab", "ab", "b", 2},
		} {
			re := MustCompile(tc.pattern, options)
			m, err := re.FindStringMatch(tc.input)
			if err != nil || m == nil {
				t.Fatalf("%s: FindStringMatch = %v, %v", tc.pattern, m, err)
			}
			group := m.GroupByName("x")
			if m.String() != tc.match || m.GroupCount() != 2 || group.String() != tc.value || len(group.Captures) != tc.captures {
				t.Fatalf("%s: default mode duplicate capture behavior changed", tc.pattern)
			}
		}
	}
	re := MustCompile(`^(?:(?<x>a)|b)+$`, None)
	m, err := re.FindStringMatch("ab")
	if err != nil || m == nil || m.GroupByName("x").String() != "a" {
		t.Fatal("non-ECMAScript capture history changed")
	}
}

func TestECMADuplicateNamesNextMatch(t *testing.T) {
	// Related Test262 iteration coverage:
	// https://github.com/tc39/test262/blob/main/test/built-ins/RegExp/named-groups/duplicate-names-matchall.js
	// ByteRange assertions also exercise regexp2's UTF-8 API.
	re := MustCompile(`(?<x>é)|(?<x>β)`, ECMAScript)
	m, err := re.FindStringMatch("éβ")
	for i, want := range []string{"é", "β"} {
		if err != nil || m == nil {
			t.Fatalf("match %d = %v, %v", i, m, err)
		}
		group := m.GroupByName("x")
		if group != m.GroupByNumber(i+1) || group.String() != want {
			t.Fatalf("match %d selected the wrong group", i)
		}
		if index, length := group.ByteRange(); index != i*2 || length != 2 {
			t.Fatalf("match %d byte range = (%d, %d)", i, index, length)
		}
		m, err = re.FindNextMatch(m)
	}
	if err != nil || m != nil {
		t.Fatalf("extra match = %v, %v", m, err)
	}
}

func TestECMADuplicateNamesWithRE2Syntax(t *testing.T) {
	re := MustCompile(`^(?:(?P<x>a)|(?P<x>b))(?P=x)$`, ECMAScript|RE2)
	m, err := re.FindStringMatch("bb")
	if err != nil || m == nil || m.GroupByName("x") != m.GroupByNumber(2) {
		t.Fatalf("FindStringMatch = %v, %v; want second x group", m, err)
	}
}

func TestRegisterEngineDuplicateCaptureNames(t *testing.T) {
	// Model a generated engine through the public registration adapter. The
	// same existing metadata fields identify both slots without a new contract.
	const pattern = `^(?:(?<registered>a)|(?<registered>b)|c)$`
	RegisterEngine(pattern, RuntimeEngineData{
		CapNames:      map[string]int{"registered": 1},
		CapsList:      []string{"", "registered", "registered"},
		CapSize:       3,
		FindFirstChar: func(r *Runner) bool { return r.Runtextpos == 0 },
		Execute: func(r *Runner) error {
			if r.Runtextend != 1 || r.Runtextpos != 0 {
				return nil
			}
			switch r.Runtext[0] {
			case 'a':
				r.Capture(1, 0, 1)
			case 'b':
				r.Capture(2, 0, 1)
			case 'c':
			default:
				return nil
			}
			r.Capture(0, 0, 1)
			r.Runtextpos = 1
			return nil
		},
	}, ECMAScript)
	re := MustCompile(pattern, ECMAScript)
	for _, tc := range []struct {
		input       string
		slot        int
		replacement string
	}{
		{"a", 1, "a:a:"},
		{"b", 2, "b::b"},
		{"c", 1, "::"},
	} {
		m, err := re.FindStringMatch(tc.input)
		if err != nil || m == nil {
			t.Fatalf("FindStringMatch(%q) = %v, %v", tc.input, m, err)
		}
		if m.GroupByName("registered") != m.GroupByNumber(tc.slot) || re.GroupNumberFromName("registered") != 1 {
			t.Fatalf("registered group resolution failed for %q", tc.input)
		}
		got, err := re.Replace(tc.input, `${registered}:${1}:${2}`, -1, -1)
		if err != nil || got != tc.replacement {
			t.Fatalf("Replace(%q) = %q, %v; want %q", tc.input, got, err, tc.replacement)
		}
	}
}

func TestRegisterEngineDuplicateCaptureNamesSparseNumbers(t *testing.T) {
	const pattern = `(?<sparse>a)|(?<sparse>b)`
	RegisterEngine(pattern, RuntimeEngineData{
		Caps:          map[int]int{0: 0, 5: 1, 9: 2},
		CapNames:      map[string]int{"sparse": 5},
		CapsList:      []string{"", "sparse", "sparse"},
		CapSize:       3,
		FindFirstChar: func(r *Runner) bool { return r.Runtextpos == 0 },
		Execute: func(r *Runner) error {
			if r.Runtextend == 1 && r.Runtextpos == 0 && r.Runtext[0] == 'b' {
				r.Capture(2, 0, 1)
				r.Capture(0, 0, 1)
				r.Runtextpos = 1
			}
			return nil
		},
	}, ECMAScript)
	re := MustCompile(pattern, ECMAScript, OptionMaxCachedReplacerDataEntries(0))
	m, err := re.FindStringMatch("b")
	if err != nil || m == nil {
		t.Fatalf("FindStringMatch = %v, %v", m, err)
	}
	if re.GroupNumberFromName("sparse") != 5 || m.GroupByName("sparse") != m.GroupByNumber(9) {
		t.Fatal("sparse group numbers were confused with dense capture slots")
	}
	got, err := re.Replace("b", `${sparse}:${5}:${9}`, -1, -1)
	if err != nil || got != "b::b" {
		t.Fatalf("Replace = %q, %v; want b::b", got, err)
	}
}

func TestECMADuplicateCaptureUse(t *testing.T) {
	for _, tc := range []struct {
		pattern string
		inUse   []bool
	}{
		{`^(?:(?<x>a)|(?<x>b))+$`, []bool{true, false, false}},
		{`^(?:(?<x>a)|(?<x>b))+\k<x>$`, []bool{true, true, true}},
		{`^(?:(?<x>a)|(?<x>b))+\2$`, []bool{true, false, true}},
	} {
		t.Run(tc.pattern, func(t *testing.T) {
			tree, err := syntax.Parse(tc.pattern, syntax.ParseOptions{RegexOptions: syntax.ECMAScript})
			if err != nil {
				t.Fatal(err)
			}
			code, err := syntax.Write(tree)
			if err != nil {
				t.Fatal(err)
			}
			for slot, want := range tc.inUse {
				if got := code.CaptureSlotInUse[slot]; got != want {
					t.Errorf("capture slot %d in use = %v, want %v", slot, got, want)
				}
			}
		})
	}
}
