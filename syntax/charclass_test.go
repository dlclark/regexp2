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

func TestCanonicalize(t *testing.T) {
	set := &CharSet{}
	set.addRange('\x01', unicode.MaxRune)

	if want, got := "[^\\x00]", set.String(); want != got {
		t.Fatalf("wanted: %s, got %s", want, got)
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
