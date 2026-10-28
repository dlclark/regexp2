package syntax

import (
	"testing"
	"unicode"
)

func TestCharSetSerializeRoundTrip(t *testing.T) {
	// create our set
	set := &CharSet{}
	set.addRange('a', 'z')
	set.addRange('A', 'Z')
	set.addChar(':')
	set.addDigit(false, false)

	// serialize and de-serialize it
	hash := set.Hash()
	newSet := NewCharSetRuntime(string(hash))

	// make sure it was a clean round-trip
	if !set.Equals(&newSet) {
		t.Fail()
	}

	t.Run("ECMAProperties", func(t *testing.T) {
		// Generated runners must restore shared properties, including their
		// case folding. Normal regexp matching doesn't deserialize sets.
		tree, err := Parse(`(?i)[^\p{Ll}\p{Emoji}]`, ParseOptions{RegexOptions: ECMAScript | Unicode})
		if err != nil {
			t.Fatal(err)
		}
		code, err := Write(tree)
		if err != nil {
			t.Fatal(err)
		}
		if len(code.Sets) == 0 {
			t.Fatal("pattern has no character sets")
		}
		for _, set := range code.Sets {
			restored := NewCharSetRuntime(string(set.Hash()))
			for _, r := range []rune{'!', 'A', 'Σ', '😀'} {
				if got, want := restored.Contains(r), r == '!'; got != want {
					t.Errorf("restored set Contains(%U) = %v; want %v", r, got, want)
				}
			}
		}
	})
}

func TestCategorySetCopyAndSerialization(t *testing.T) {
	// Cached membership data must survive Copy and be reconstructed by
	// NewCharSetRuntime, including categories inside composed sets.
	for _, tc := range []struct {
		pattern string
		yes, no string
	}{
		{`\d`, "5٥", "aé"},
		{`\p{Uppercase_Letter}`, "AΣ", "aσ"},
		{`\p{Greek}`, "Σσ", "Aé"},
		{`\p{White_Space}`, " \u2003", "Aé"},
		// Math includes letters as well as the general category Sm.
		{`\p{Math}`, "+𝐀", "a😀"},
		{`\p{gcb=Extend}`, "\u0301\u0308", "aΣ"},
		{`[\p{Greek}\p{Nd}]`, "Σ٥", "a😀"},
		{`\P{Nd}`, "aΣ", "5٥"},
		{`[\p{L}-[\p{Greek}]]`, "aé", "Σσ"},
		{`(?i)\p{Lu}`, "AΣaσ", "5٥"},
	} {
		t.Run(tc.pattern, func(t *testing.T) {
			t.Parallel()
			tree, err := Parse(tc.pattern, ParseOptions{})
			if err != nil {
				t.Fatal(err)
			}
			code, err := Write(tree)
			if err != nil {
				t.Fatal(err)
			}
			if len(code.Sets) != 1 {
				t.Fatalf("single character class produced %d sets", len(code.Sets))
			}
			original := code.Sets[0]
			encoded := string(original.Hash())
			copied := original.Copy()
			restored := NewCharSetRuntime(encoded)
			// Compiled sets have an ASCII bitmap; generated runners can use
			// deserialized sets without one. Exercise both public paths.
			code.PrepareCharSetASCIIBitmaps()
			for name, set := range map[string]*CharSet{
				"compiled": original, "copied": &copied, "restored": &restored,
			} {
				if string(set.Hash()) != encoded || !original.Equals(set) {
					t.Errorf("%s changed the character set representation", name)
				}
				for _, r := range tc.yes {
					if !set.Contains(r) {
						t.Errorf("%s.Contains(%U) = false, want true", name, r)
					}
				}
				for _, r := range tc.no {
					if set.Contains(r) {
						t.Errorf("%s.Contains(%U) = true, want false", name, r)
					}
				}
			}
		})
	}
}

func TestCategorySetLegacySerialization(t *testing.T) {
	// Serialized Nd class emitted by existing generated engines: flags,
	// zero ranges, one category, then its length and name. A round trip
	// alone would not catch an incompatible change to both reader and writer.
	const encoded = "\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02Nd"
	set := NewCharSetRuntime(encoded)
	if !set.Contains('5') || !set.Contains('٥') || set.Contains('Σ') {
		t.Fatal("legacy digit class has incorrect membership")
	}
	if string(set.Hash()) != encoded {
		t.Fatal("legacy digit class encoding changed")
	}
}

func TestCanonicalizePreservesMembership(t *testing.T) {
	for _, tc := range []struct {
		name   string
		ranges []SingleRange
		negate bool
	}{
		{"empty", nil, false},
		{"negated empty", nil, true},
		{"overlapping and adjacent", []SingleRange{{'d', 'f'}, {'a', 'c'}, {'b', 'e'}, {'z', 'z'}}, false},
		{"negated union", []SingleRange{{'d', 'f'}, {'a', 'c'}, {'b', 'e'}, {'z', 'z'}}, true},
		{"complement range", []SingleRange{{':', unicode.MaxRune}, {0, '/'}}, false},
		{"full domain", []SingleRange{{'a', unicode.MaxRune}, {0, 'b'}}, false},
	} {
		t.Run(tc.name, func(t *testing.T) {
			set := &CharSet{ranges: append([]SingleRange(nil), tc.ranges...), negate: tc.negate}
			// Repeated canonicalization must preserve the original union,
			// including gaps, negation, and the endpoints of the rune domain.
			for pass := 0; pass < 2; pass++ {
				set.canonicalize()
				chars := []rune{'é', '中', unicode.MaxRune - 1, unicode.MaxRune}
				for r := rune(0); r < 128; r++ {
					chars = append(chars, r)
				}
				for _, r := range chars {
					want := false
					for _, span := range tc.ranges {
						want = want || (span.First <= r && r <= span.Last)
					}
					want = want != tc.negate
					if got := set.Contains(r); got != want {
						t.Errorf("pass %d: Contains(%U) = %v, want %v", pass, r, got, want)
					}
				}
			}
		})
	}
}

func TestCanonicalizeAllowsFurtherMembers(t *testing.T) {
	for _, missing := range []rune{0, '5', unicode.MaxRune} {
		set := &CharSet{}
		if missing > 0 {
			set.addRange(0, missing-1)
		}
		if missing < unicode.MaxRune {
			set.addRange(missing+1, unicode.MaxRune)
		}
		set.canonicalize()
		if set.Contains(missing) {
			t.Fatalf("Contains(%U) before addition = true, want false", missing)
		}
		set.addChar(missing)
		if !set.Contains(missing) || !set.IsAnything() {
			t.Errorf("adding %U to its complement did not produce the full set", missing)
		}
	}
}

func TestSimplifyPreservesMembership(t *testing.T) {
	withoutNewline := []SingleRange{{0, '\n' - 1}, {'\n' + 1, unicode.MaxRune}}
	for _, tc := range []struct {
		name         string
		set          CharSet
		wantNegated  bool
		wantInverse  bool
		wantAnything bool
	}{
		{name: "empty"},
		{name: "full domain", set: CharSet{ranges: []SingleRange{{0, unicode.MaxRune}}}, wantAnything: true},
		{name: "missing first rune", set: CharSet{ranges: []SingleRange{{1, unicode.MaxRune}}}, wantNegated: true, wantInverse: true},
		{name: "missing last rune", set: CharSet{ranges: []SingleRange{{0, unicode.MaxRune - 1}}}, wantNegated: true, wantInverse: true},
		{name: "missing interior range", set: CharSet{ranges: []SingleRange{{0, '/'}, {':', unicode.MaxRune}}}, wantNegated: true},
		{name: "category fills gap", set: CharSet{ranges: withoutNewline, categories: []charCategory{newCharCategory(Category{Cat: SpaceCategoryText})}}, wantAnything: true},
		{name: "category leaves gap", set: CharSet{ranges: withoutNewline, categories: []charCategory{newCharCategory(Category{Cat: "Nd"})}}, wantNegated: true, wantInverse: true},
		{name: "already negated", set: CharSet{ranges: withoutNewline, negate: true}, wantNegated: true},
		{name: "subtraction", set: CharSet{ranges: withoutNewline, sub: ECMADigitClass()}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			set := tc.set.Copy()
			set.canonicalize()
			before := set.Copy()
			chars := []rune{'é', '中', unicode.MaxRune - 1, unicode.MaxRune}
			for r := rune(0); r < 128; r++ {
				chars = append(chars, r)
			}
			for pass := 0; pass < 2; pass++ {
				set.simplify()
				for _, r := range chars {
					if got, want := set.Contains(r), before.Contains(r); got != want {
						t.Errorf("pass %d: Contains(%U) = %v, want %v", pass, r, got, want)
					}
				}
				// These public predicates are used by the engine and code
				// generator to select equivalent specialized matchers.
				if set.IsNegated() != tc.wantNegated || set.IsSingletonInverse() != tc.wantInverse || set.IsAnything() != tc.wantAnything {
					t.Errorf("pass %d: negated/inverse/anything = %v/%v/%v, want %v/%v/%v", pass,
						set.IsNegated(), set.IsSingletonInverse(), set.IsAnything(), tc.wantNegated, tc.wantInverse, tc.wantAnything)
				}
			}
		})
	}
}

func TestAddLowercaseComplementRange(t *testing.T) {
	set := NotECMADigitClass()
	set.addLowercase()

	if set.CharIn('1') {
		t.Fatal("digit should remain excluded")
	}
	if !set.CharIn('t') {
		t.Fatal("letter should remain included")
	}
}

func TestCharSetMayOverlap(t *testing.T) {
	scenarios := []struct {
		a, b *CharSet
		want bool
	}{
		// Both cursors must advance before the ranges meet at an endpoint.
		{&CharSet{ranges: []SingleRange{{'a', 'c'}, {'g', 'i'}}}, &CharSet{ranges: []SingleRange{{'d', 'f'}, {'i', 'j'}}}, true},
		{&CharSet{ranges: []SingleRange{{'\n', '\n'}, {'\r', '\r'}}}, ECMAAnyClass(), false},
		{&CharSet{ranges: []SingleRange{{'\u2028', '\u2029'}}}, ECMAAnyClass(), false},
		// Sets with categories or subtraction keep the existing path.
		{DigitClass(), ECMADigitClass(), true},
		{&CharSet{ranges: []SingleRange{{'0', '9'}}, sub: ECMADigitClass()}, ECMADigitClass(), false},
	}
	for _, s := range scenarios {
		for _, pair := range [][2]*CharSet{{s.a, s.b}, {s.b, s.a}} {
			if got := pair[0].MayOverlap(pair[1]); got != s.want {
				t.Errorf("MayOverlap(%s, %s) = %v, want %v", pair[0], pair[1], got, s.want)
			}
		}
	}
}
