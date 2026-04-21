package regexp2

import (
	"slices"
	"testing"
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
//   - Generated property-escape/unicodeSets suites that depend on /u+/v
//     property aliases and Unicode-string properties beyond regexp2's syntax.
//   - S15.10.2.5_A1_T4.js style capture reset semantics inside quantified
//     groups, where ECMAScript reports captures from unmatched final iterations
//     as undefined.

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

	for _, tt := range tests {
		t.Run(tt.source, func(t *testing.T) {
			re := MustCompile(tt.expr, ECMAScript|tt.opt)
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
