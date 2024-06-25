package helpers

import "testing"

func TestAsciiSearchValues_Basic(t *testing.T) {
	sv := NewAsciiSearchValues("DFHJLNPRTVXZ")
	i := sv.IndexOfAny([]rune("xyzxyzxyzxyzxyzxyzxyDxyzxyz"))
	if want, got := 20, i; want != got {
		t.Errorf("Failed to find index, want %v got %v", want, got)
	}
}

func TestAsciiSearchValues_NotFound(t *testing.T) {
	sv := NewAsciiSearchValues("abcdef0123!@#")
	i := sv.IndexOfAny([]rune("xyzxyzxyzxyzxyzxyzxyxyzxyz"))
	if want, got := -1, i; want != got {
		t.Errorf("Failed to find index, want %v got %v", want, got)
	}
}

func TestAsciiSearchValues_NotAscii(t *testing.T) {
	sv := NewAsciiSearchValues("abcdef0123!@#")
	i := sv.IndexOfAny([]rune("xyzx\u1234zxyzxyzxyzxyzxydxyzxyz"))
	if want, got := 20, i; want != got {
		t.Errorf("Failed to find index, want %v got %v", want, got)
	}
}

func TestAsciiSearchValues_Boundaries(t *testing.T) {
	sv := NewAsciiSearchValues("abcdef\u00000123!@#\u007f")
	i := sv.IndexOfAny([]rune("xyzx\u1234zxyzxyzxyzxyzxy\u0000xyzxyz"))
	if want, got := 20, i; want != got {
		t.Errorf("Failed to find index, want %v got %v", want, got)
	}

	i = sv.IndexOfAny([]rune("xyzx\u1234zxyzxyzxyzxyzxy\u007fxyzxyz"))
	if want, got := 20, i; want != got {
		t.Errorf("Failed to find index, want %v got %v", want, got)
	}
}

var text []rune

func makeText(n int) []rune {
	if len(text) >= n {
		return text[:n]
	}
	text = make([]rune, n)
	x := ^uint32(0)
	for i := range text {
		x += x
		x ^= 1
		if int32(x) < 0 {
			x ^= 0x88888eef
		}
		if x%31 == 0 {
			text[i] = '\n'
		} else {
			text[i] = rune(x%(0x7E+1-0x20) + 0x20)
		}
	}
	return text
}

var val = 0

func benchmarkAsciiSearchValues(b *testing.B, chars string, n int) {
	sv := NewAsciiSearchValues(chars)
	t := makeText(n)
	b.ResetTimer()
	b.SetBytes(int64(n))
	for i := 0; i < b.N; i++ {
		idx := 0
		for j := 0; j < len(t) && idx > -1; j++ {
			idx = sv.IndexOfAny(t[j:])
			if idx > j {
				j = idx + 1
			}
		}
	}
}

func BenchmarkAsciiSearchValues_32(b *testing.B) {
	benchmarkAsciiSearchValues(b, "DFHJLNPRTVXZ", 32<<0)
}
func BenchmarkAsciiSearchValues_1K(b *testing.B) {
	benchmarkAsciiSearchValues(b, "DFHJLNPRTVXZ", 1<<10)
}
func BenchmarkAsciiSearchValues_32K(b *testing.B) {
	benchmarkAsciiSearchValues(b, "DFHJLNPRTVXZ", 32<<10)
}
func BenchmarkAsciiSearchValues_1M(b *testing.B) {
	benchmarkAsciiSearchValues(b, "DFHJLNPRTVXZ", 1<<20)
}
func BenchmarkAsciiSearchValues_32M(b *testing.B) {
	benchmarkAsciiSearchValues(b, "DFHJLNPRTVXZ", 32<<20)
}
