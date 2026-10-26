package regexp2

import (
	"fmt"
	"testing"
)

func TestECMAUnicodeSurrogatePairs(t *testing.T) {
	for _, tt := range []struct {
		name, pattern, input string
		want                 bool
	}{
		{"literal pair", `^\uD83D\uDE00$`, "😀", true},
		{"class pair", `^[\uD83D\uDE00]$`, "😀", true},
		{"negated class pair", `^[^\uD83D\uDE00]$`, "😀", false},
		{"range endpoints", `^[\uD83D\uDE00-\uD83D\uDE01]$`, "😁", true},
		{"outside range", `^[\uD83D\uDE00-\uD83D\uDE01]$`, "😂", false},
		{"pair quantifier", `^(?:\uD83D\uDE00){2}$`, "😀😀", true},
		{"atom quantifier", `^\uD83D\uDE00{2}$`, "😀😀", true},
		{"lookbehind", `(?<=\uD83D\uDE00)a`, "😀a", true},
		{"capture", `^(\uD83D\uDE00)\1$`, "😀😀", true},
		{"escaped capture name", `^(?<\uD835\uDC9C>a)\k<𝒜>$`, "aa", true},
		{"braced high stays separate", `^\u{D83D}\uDE00$`, "😀", false},
		{"braced low stays separate", `^\uD83D\u{DE00}$`, "😀", false},
		{"both braced stay separate", `^\u{D83D}\u{DE00}$`, "😀", false},
		{"isolated high", `^\uD83D$`, "😀", false},
		{"isolated low", `^\uDE00$`, "😀", false},
		{"reversed", `^\uDE00\uD83D$`, "😀", false},
		{"group separates", `^\uD83D(?:)\uDE00$`, "😀", false},
		{"high followed by ordinary escape", `^\uD83D\u0061$`, "😀a", false},
	} {
		t.Run(tt.name, func(t *testing.T) {
			re, err := Compile(tt.pattern, ECMAScript|Unicode)
			if err != nil {
				t.Fatal(err)
			}

			got, err := re.MatchString(tt.input)

			if err != nil || got != tt.want {
				t.Fatalf("%s on %q = %v, %v; want %v", tt.pattern, tt.input, got, err, tt.want)
			}
		})
	}
}

// The rune API can represent isolated UTF-16 code units that UTF-8 strings
// cannot. A failed pair lookahead must leave the next escape unconsumed.
func TestECMAUnicodeUnpairedSurrogateRunes(t *testing.T) {
	for _, tt := range []struct {
		pattern string
		input   []rune
	}{
		{`^\uD83D$`, []rune{0xd83d}},
		{`^\uD83D\u0061$`, []rune{0xd83d, 'a'}},
		{`^\uD83D\uD83D$`, []rune{0xd83d, 0xd83d}},
		{`^\uD83D\uD83D\uDE00$`, []rune{0xd83d, '😀'}},
		{`^\uD83D\u{DE00}$`, []rune{0xd83d, 0xde00}},
		{`^\u{D83D}\uDE00$`, []rune{0xd83d, 0xde00}},
	} {
		t.Run(tt.pattern, func(t *testing.T) {
			re := MustCompile(tt.pattern, ECMAScript|Unicode)

			matched, err := re.MatchRunes(tt.input)

			if err != nil || !matched {
				t.Fatalf("MatchRunes(%U) = %v, %v; want true, nil", tt.input, matched, err)
			}
		})
	}
}

func TestECMAUnicodeMalformedSurrogateFollower(t *testing.T) {
	for _, pattern := range []string{`\uD83D\u`, `\uD83D\uXYZW`, `[\uD83D\uXYZW]`, `\uD83D\u{}`} {
		t.Run(pattern, func(t *testing.T) {
			re, err := Compile(pattern, ECMAScript|Unicode)

			if err == nil || re != nil {
				t.Fatalf("Compile = %v, %v; want nil expression and syntax error", re, err)
			}
		})
	}
}

func TestSurrogatePairModeIsolation(t *testing.T) {
	for _, options := range []RegexOptions{None, Unicode, RE2, ECMAScript} {
		t.Run(fmt.Sprint(options), func(t *testing.T) {
			re := MustCompile(`^\uD83D\uDE00$`, options)

			matched, err := re.MatchRunes([]rune{0xd83d, 0xde00})

			if err != nil || !matched {
				t.Fatalf("MatchRunes options %v = %v, %v; want separate surrogate runes", options, matched, err)
			}
		})
	}
}
