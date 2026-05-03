package helpers

import "testing"

func TestIsInMask32_Basic(t *testing.T) {
	mask := uint32(0x8f00_0000)
	val := 'e' - 'a'
	if !IsInMask32(val, mask) {
		t.Fatalf("failed to find e in mask %b from %v", mask, val)
	}
}

func TestIsInASCIIBitmap(t *testing.T) {
	var lo, hi uint64
	for _, ch := range []rune("Az9_") {
		if ch < 64 {
			lo |= 1 << uint(ch)
		} else {
			hi |= 1 << uint(ch-64)
		}
	}

	for _, ch := range []rune("Az9_") {
		if !IsInASCIIBitmap(ch, lo, hi) {
			t.Fatalf("expected %q to be in bitmap", ch)
		}
	}
	for _, ch := range []rune("b:") {
		if IsInASCIIBitmap(ch, lo, hi) {
			t.Fatalf("expected %q to be outside bitmap", ch)
		}
	}
	if IsInASCIIBitmap(128, lo, hi) {
		t.Fatal("expected non-ASCII input to be outside bitmap")
	}
}
