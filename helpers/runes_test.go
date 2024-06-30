package helpers

import "testing"

func TestIsInMask32_Basic(t *testing.T) {
	mask := uint32(0x8f00_0000)
	val := 'e' - ']'
	if !IsInMask32(val, mask) {
		t.Fatalf("failed to find e in mask %b from %v", mask, val)
	}
}
