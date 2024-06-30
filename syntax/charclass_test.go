package syntax

import "testing"

func TestCharSetSerializeRoundTrip(t *testing.T) {
	// create our set
	set := &CharSet{}
	set.addRange('a', 'z')
	set.addRange('A', 'Z')
	set.addChar(':')
	set.addDigit(false, false, "\\d")

	// serialize and de-serialize it
	hash := set.Hash()
	newSet := NewCharSetRuntime(string(hash))

	// make sure it was a clean round-trip
	if !set.Equals(&newSet) {
		t.Fail()
	}
}
