package helpers

import "testing"

func TestIndexOf_Miss(t *testing.T) {
	i := IndexOf([]rune("GHMJ"), []rune("HIJ"))
	if want, got := -1, i; want != got {
		t.Fatalf("Expected %v got %v", want, got)
	}
}

func TestStartsWith_Miss(t *testing.T) {
	if want, got := false, StartsWith([]rune("GHMJ")[1:], []rune("HIJ")); want != got {
		t.Fatalf("Expected %v got %v", want, got)
	}
}
