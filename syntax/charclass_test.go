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
