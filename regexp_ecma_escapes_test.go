package regexp2_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/dlclark/regexp2/v2"
)

// These cases specify ECMA-262 Annex B.1.2's non-Unicode escape grammar.
// https://tc39.es/ecma262/#sec-regular-expressions-patterns
func TestECMALegacyEscapes(t *testing.T) {
	cases := []struct {
		pattern string
		matches []string
		misses  []string
	}{
		{`\a`, []string{"a"}, []string{"\x07"}},
		{`[\a]`, []string{"a"}, []string{"\x07"}},
		{`\e`, []string{"e"}, []string{"\x1b"}},
		{`[\e]`, []string{"e"}, []string{"\x1b"}},
		{`\Afoo`, []string{"Afoo"}, []string{"foo"}},
		{`\Gfoo`, []string{"Gfoo"}, []string{"foo"}},
		{`foo\Z`, []string{"fooZ"}, []string{"foo"}},
		{`foo\z`, []string{"fooz"}, []string{"foo"}},
		{`\cA`, []string{"\x01"}, []string{`\cA`, "cA"}},
		{`\cz`, []string{"\x1a"}, []string{`\cz`}},
		{`\c`, []string{`\c`}, []string{"c", `\`}},
		{`\c1`, []string{`\c1`}, []string{"c1", "\x11"}},
		{`\c_`, []string{`\c_`}, []string{"c_", "\x1f"}},
		{`\c@`, []string{`\c@`}, []string{"c@", "\x00"}},
		{`\cА`, []string{`\cА`}, []string{"cА"}}, // Cyrillic, not ASCII A.
		{`\c[ab]`, []string{`\ca`, `\cb`}, []string{`\c[ab]`, "\x1bab]"}},
		{`\c*`, []string{`\`, `\c`, `\ccc`}, []string{"", "ccc", `\c\c`}},
		{`\c{2}`, []string{`\cc`}, []string{`\c\c`, "cc"}},
		{`[\c]`, []string{`\`, "c"}, []string{"\x1d", `\c`}},
		{`[^\c]`, []string{"a"}, []string{`\`, "c"}},
		{`[\c-]`, []string{`\`, "c", "-"}, []string{"a"}},
		{`[\c@]`, []string{`\`, "c", "@"}, []string{"\x00"}},
		{`[\c[]`, []string{`\`, "c", "["}, []string{"\x1b"}},
		{`[\c\]]`, []string{`\`, "c", "]"}, []string{"\x1d"}},
		{`[\cA]`, []string{"\x01"}, []string{`\`, "c", "A"}},
		{`[\c1]`, []string{"\x11"}, []string{`\`, "c", "1"}},
		{`[\c_]`, []string{"\x1f"}, []string{`\`, "c", "_"}},
		{`[\c0-\c9]`, []string{"\x10", "\x15", "\x19"}, []string{"\x0f", "\x1a", "0"}},
		{`[Z-\c]`, []string{"Z", "[", `\`, "c"}, []string{"Y", "]", "b"}},
		{`[\c-z]`, []string{`\`, "c", "m", "z"}, []string{"b", "-"}},
		{`[\P]`, []string{"P"}, []string{"a"}},
		{`[\P{L}]`, []string{"P", "{", "L", "}"}, []string{"a"}},
		{`[A-\P]`, []string{"A", "M", "P"}, []string{"@", "Q", "-"}},
		{`[\P-Z]`, []string{"P", "T", "Z"}, []string{"O", "[", "-"}},
		{`[\p-z]`, []string{"p", "s", "z"}, []string{"o", "-"}},
		{`[\p-\z]`, []string{"p", "s", "z"}, []string{"o", "-"}},
		{`\k`, []string{"k"}, []string{`\k`}},
		{`[\k]`, []string{"k"}, []string{`\`}},
		// Defined escapes and existing malformed-hex identity fallbacks remain intact.
		{`\n`, []string{"\n"}, []string{"n"}},
		{`[\b]`, []string{"\b"}, []string{"b"}},
		{`\x1Z`, []string{"x1Z"}, []string{"\x01Z"}},
		{`\u123`, []string{"u123"}, []string{`\u123`}},
	}
	for _, options := range []regexp2.RegexOptions{
		regexp2.ECMAScript, regexp2.ECMAScript | regexp2.IgnoreCase,
		regexp2.ECMAScript | regexp2.RE2, regexp2.ECMAScript | regexp2.RightToLeft,
	} {
		for _, tc := range cases {
			t.Run(fmt.Sprintf("%s/%d", tc.pattern, options), func(t *testing.T) {
				re, err := regexp2.Compile(`^(?:`+tc.pattern+`)$`, options)
				if err != nil {
					t.Fatal(err)
				}
				for _, inputs := range []struct {
					values []string
					want   bool
				}{{tc.matches, true}, {tc.misses, false}} {
					for _, input := range inputs.values {
						got, err := re.MatchString(input)
						if err != nil || got != inputs.want {
							t.Errorf("MatchString(%q) = %v, %v; want %v", input, got, err, inputs.want)
						}
					}
				}
			})
		}
	}
}

func TestECMALegacyEscapeIgnoreCase(t *testing.T) {
	for _, tc := range []struct{ pattern, input string }{
		{`\a`, "A"}, {`\e`, "E"}, {`\c1`, `\C1`}, {`[\c]`, "C"}, {`[\P]`, "p"},
	} {
		t.Run(tc.pattern, func(t *testing.T) {
			re := regexp2.MustCompile(`^(?:`+tc.pattern+`)$`, regexp2.ECMAScript|regexp2.IgnoreCase)
			if got, err := re.MatchString(tc.input); err != nil || !got {
				t.Fatalf("MatchString(%q) = %v, %v; want true", tc.input, got, err)
			}
		})
	}
}

func TestECMALegacyEscapeCaptures(t *testing.T) {
	for _, tc := range []struct {
		pattern, input, capture string
	}{
		{`(?<x>\a)`, "a", "a"},
		{`\c(?<x>a)\k<x>`, `\caa`, "a"},
		{`\c(a)\1`, `\caa`, "a"},
		{`[\c](?<x>a)\k<x>`, `\aa`, "a"},
		{`\k<x>(?<x>a)`, "a", "a"}, // Forward named reference, not an identity escape.
	} {
		t.Run(tc.pattern, func(t *testing.T) {
			re, err := regexp2.Compile(`^(?:`+tc.pattern+`)$`, regexp2.ECMAScript)
			if err != nil {
				t.Fatal(err)
			}
			m, err := re.FindStringMatch(tc.input)
			if err != nil || m == nil {
				t.Fatalf("FindStringMatch(%q) = %v, %v", tc.input, m, err)
			}
			groups := m.Groups()
			if len(groups) != 2 || groups[1].String() != tc.capture {
				t.Fatalf("Groups() = %v; want one capture %q", groups, tc.capture)
			}
		})
	}
}

func TestECMALegacyEscapeErrors(t *testing.T) {
	for _, pattern := range []string{`\`, `\c[`, `[\c-a]`, `[a-\P]`, `[\k](?<x>a)`, `(?<x>a)[\k]`} {
		t.Run(pattern, func(t *testing.T) {
			if _, err := regexp2.Compile(pattern, regexp2.ECMAScript); err == nil {
				t.Fatal("expected a compile error")
			}
		})
	}
}

// Annex B is not applied for Unicode RegExp:
// invalid control escape sequences are early errors when the Unicode flag is set.
func TestECMAUnicodeControlEscapes(t *testing.T) {
	for _, pattern := range []string{
		`\c`, `\c0`, `\c1`, `\c_`, `\c-`, `\c `, `\cА`, // Cyrillic, not ASCII A.
		`[\c]`, `[\c0]`, `[\c_]`, `[\cA`,
	} {
		t.Run(pattern, func(t *testing.T) {
			if _, err := regexp2.Compile(pattern, regexp2.ECMAScript|regexp2.Unicode); err == nil {
				t.Fatal("expected a compile error")
			}
		})
	}
	// Valid control escapes must match the control character itself, not fall
	// back to a literal interpretation of the pattern text.
	for _, tc := range []struct {
		pattern string
		match   []string // inputs that must match
		noMatch []string // inputs a literal fallback would match
	}{
		{`\cA`, []string{"\x01"}, []string{"cA", `\cA`}},
		{`\cz`, []string{"\x1a"}, []string{"cz", `\cz`}},
		{`[\cA]`, []string{"\x01"}, []string{"c", "A", `\cA`}},
		{`[\cA-\cZ]`, []string{"\x01", "\x1a"}, []string{"c", "A", "Z", `\cA`}},
	} {
		t.Run(tc.pattern, func(t *testing.T) {
			re, err := regexp2.Compile(tc.pattern, regexp2.ECMAScript|regexp2.Unicode)
			if err != nil {
				t.Fatal(err)
			}
			for _, input := range tc.match {
				got, err := re.MatchString(input)
				if err != nil || !got {
					t.Errorf("MatchString(%q) = %v, %v; want true", input, got, err)
				}
			}
			for _, input := range tc.noMatch {
				got, err := re.MatchString(input)
				if err != nil || got {
					t.Errorf("MatchString(%q) = %v, %v; want false", input, got, err)
				}
			}
		})
	}
}

// Annex B's legacy octal escapes and its fallback for decimal escapes that
// exceed the group count do not apply with the Unicode flag; only \0 not
// followed by a digit is a decimal escape that is not a backreference.
func TestECMAUnicodeDecimalEscapes(t *testing.T) {
	for _, pattern := range []string{
		`\1`, `(a)\2`, `(a)\10`, `\8`, `\9`, `\00`, `\01`, `\07`, `\08`,
		`[\1]`, `[\7]`, `[\8]`, `[\00]`, `[\01]`, `[\09]`,
	} {
		t.Run(pattern, func(t *testing.T) {
			if _, err := regexp2.Compile(pattern, regexp2.ECMAScript|regexp2.Unicode); err == nil {
				t.Fatal("expected a compile error")
			}
		})
	}
	for _, tc := range []struct{ pattern, input string }{
		{`^\0$`, "\x00"}, {`^[\0]$`, "\x00"}, {`^\0a$`, "\x00a"},
		{`^(a)\1$`, "aa"}, {`^\1(a)$`, "a"},
	} {
		t.Run(tc.pattern, func(t *testing.T) {
			re, err := regexp2.Compile(tc.pattern, regexp2.ECMAScript|regexp2.Unicode)
			if err != nil {
				t.Fatal(err)
			}
			got, err := re.MatchString(tc.input)
			if err != nil || !got {
				t.Fatalf("MatchString(%q) = %v, %v; want true", tc.input, got, err)
			}
		})
	}
	// Without ECMAScript, Unicode keeps the engine's octal escapes.
	if got, err := regexp2.MustCompile(`^\01$`, regexp2.Unicode).MatchString("\x01"); err != nil || !got {
		t.Fatalf("Unicode octal escape: MatchString = %v, %v; want true", got, err)
	}
}

func TestLegacyEscapeModeIsolation(t *testing.T) {
	for _, options := range []regexp2.RegexOptions{regexp2.None, regexp2.RE2, regexp2.Unicode, regexp2.RE2 | regexp2.Unicode} {
		for _, tc := range []struct{ pattern, input string }{
			{`^\a$`, "\x07"}, {`^\e$`, "\x1b"}, {`^\c@$`, "\x00"}, {`^\c[$`, "\x1b"},
			{`\Afoo\Z`, "foo"}, {`\Gfoo\z`, "foo"}, {`^\P{L}$`, "1"}, {`^[\P{L}]$`, "1"},
		} {
			t.Run(fmt.Sprintf("%s/%d", tc.pattern, options), func(t *testing.T) {
				re, err := regexp2.Compile(tc.pattern, options)
				if err != nil {
					t.Fatal(err)
				}
				got, err := re.MatchString(tc.input)
				if err != nil || !got {
					t.Fatalf("MatchString(%q) = %v, %v; want true", tc.input, got, err)
				}
			})
		}
	}
	got, err := regexp2.Unescape(`\a\e\c@\c[`)
	if err != nil || got != "\x07\x1b\x00\x1b" {
		t.Fatalf("Unescape = %q, %v; want legacy control characters", got, err)
	}
}

func TestEscapeBELRoundTrip(t *testing.T) {
	input := "\x07"
	for _, options := range []regexp2.RegexOptions{regexp2.None, regexp2.RE2, regexp2.ECMAScript, regexp2.ECMAScript | regexp2.Unicode} {
		t.Run(fmt.Sprint(options), func(t *testing.T) {
			re, err := regexp2.Compile(`^(?:`+regexp2.Escape(input)+`)$`, options)
			if err != nil {
				t.Fatal(err)
			}
			for _, tc := range []struct {
				input string
				want  bool
			}{{input, true}, {"a", false}} {
				got, err := re.MatchString(tc.input)
				if err != nil || got != tc.want {
					t.Fatalf("MatchString(%q) = %v, %v; want %v", tc.input, got, err, tc.want)
				}
			}
		})
	}
	got, err := regexp2.Unescape(regexp2.Escape(input))
	if err != nil || got != input {
		t.Fatalf("Unescape(Escape(BEL)) = %q, %v; want BEL", got, err)
	}
}

func BenchmarkECMALegacyEscapesCompile(b *testing.B) {
	for _, tc := range []struct{ name, pattern string }{
		{"plain", `abc`},
		{"control_letters", `\cA\cz`},
		{"literal_controls", `\c1\cА`},
		{"control_classes", `[\c1\c_]`},
	} {
		b.Run(tc.name, func(b *testing.B) {
			pattern := strings.Repeat(tc.pattern, 64)
			b.ReportAllocs()
			for b.Loop() {
				if _, err := regexp2.Compile(pattern, regexp2.ECMAScript); err != nil {
					b.Fatal(err)
				}
			}
		})
	}
}
