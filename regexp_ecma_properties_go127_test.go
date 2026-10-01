//go:build go1.27

package regexp2

import "testing"

// Go 1.27 updates unicode.Version to 17.0.0. Keep examples that require its
// new assignments and category corrections separate from the portable cases.
// https://go.dev/doc/go1.27#unicode
func TestECMAUnicode17PropertyEscapes(t *testing.T) {
	testECMAUnicodePropertyCases(t, []ecmaPropertyCase{
		{"sc=Gara", "\U00010d50", "a"}, // Garay was added in Unicode 16.
		{"scx=Gara", "\U00010d50", "a"},
		{"sc=Tayo", "\U0001e6c0", "a"}, // Tai Yo was added in Unicode 17.
		{"scx=Tayo", "\U0001e6c0", "a"},
		{"Script=Latin", "\ua7cb", "α"},
		{"L", "\ua7cb", "0"},
		{"Ll", "\u1c8a", "\u0295"}, // U+0295 moved from Ll to Lo.
		{"Lo", "\u0295", "A"},
		{"Nd", "\U00010d40", "A"},
		{"Assigned", "\U00010d50", "\u0378"},
		{"sc=Unknown", "\u0378", "\U00010d50"},
		{"scx=Unknown", "\u0378", "\U00010d50"},
		{"Dash", "\U00010d6e", "a"},
	})
}
