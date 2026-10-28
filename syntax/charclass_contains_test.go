package syntax

import "testing"

func TestCharSetContainsCompatibility(t *testing.T) {
	// Keep CharIn callable on values, including non-addressable values and
	// interface assignments used by callers and existing generated engines.
	// In v3 CharIn can be removed.
	var value interface{ CharIn(rune) bool } = *DigitClass()
	if !value.CharIn('5') || !NewCharSetRuntime(string(DigitClass().Hash())).CharIn('5') {
		t.Fatal("value CharIn API changed")
	}
	subtracted := WordClass()
	subtracted.addSubtraction(DigitClass())
	for _, set := range []*CharSet{DigitClass(), NotDigitClass(), WordClass(), NotWordClass(), subtracted} {
		for _, bitmap := range []bool{false, true} {
			if bitmap {
				set.prepareASCIIBitmap()
			}
			for _, ch := range []rune{-1, 0, 'a', '5', '_', 127, 128, 'é', '界', '٥', '😀', 0x110000} {
				if got, want := set.Contains(ch), set.charInSlow(ch); got != want {
					t.Fatalf("Contains(%U)=%v; slow path=%v", ch, got, want)
				}
			}
		}
	}
}

func BenchmarkCategoryContains(b *testing.B) {
	for _, tc := range []struct {
		name string
		r    rune
		want bool
	}{
		{"DigitASCII", '5', true},
		{"DigitUnicode", '٥', true},
		{"DigitMiss", 'é', false},
	} {
		b.Run(tc.name, func(b *testing.B) {
			// Generated runners may use deserialized sets without an ASCII bitmap.
			set := NewCharSetRuntime(string(DigitClass().Hash()))
			b.ReportAllocs()
			for b.Loop() {
				if got := set.Contains(tc.r); got != tc.want {
					b.Fatalf("Contains(%U) = %v, want %v", tc.r, got, tc.want)
				}
			}
		})
	}
}
