package helpers

import "testing"

func TestIndexOf_Miss(t *testing.T) {
	i := IndexOf([]rune("GHMJ"), []rune("HIJ"))
	if want, got := -1, i; want != got {
		t.Fatalf("Expected %v got %v", want, got)
	}
}

func TestIndexOf_Hit0(t *testing.T) {
	i := IndexOf([]rune("the quick brown fox"), []rune("the quick brown fox"))
	if want, got := 0, i; want != got {
		t.Fatalf("Expected %v got %v", want, got)
	}
}

func TestIndexOf_IgnoreCaseHit(t *testing.T) {
	i := IndexOfIgnoreCase([]rune("what do you know of the quick brown fox?"), []rune("the quick brown fox"))
	if want, got := 20, i; want != got {
		t.Fatalf("Expected %v got %v", want, got)
	}
}

func TestIndexOfIgnoreCaseAscii_Hit(t *testing.T) {
	i := IndexOfIgnoreCaseAscii([]rune("what do you know of the quick brown fox?"), []rune("THE QUICK BROWN FOX"))
	if want, got := 20, i; want != got {
		t.Fatalf("Expected %v got %v", want, got)
	}
}

func TestIndexStringIgnoreCaseASCII(t *testing.T) {
	i := IndexStringIgnoreCaseASCII("0123456789TeStToken", "testtoken")
	if want, got := 10, i; want != got {
		t.Fatalf("Expected %v got %v", want, got)
	}

	i = IndexStringIgnoreCaseASCII("0123456789", "testtoken")
	if want, got := -1, i; want != got {
		t.Fatalf("Expected %v got %v", want, got)
	}
}

func TestStartsWith_Miss(t *testing.T) {
	if want, got := false, StartsWith([]rune("GHMJ")[1:], []rune("HIJ")); want != got {
		t.Fatalf("Expected %v got %v", want, got)
	}
}
