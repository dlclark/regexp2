package regexp2

import (
	"testing"

	"slices"
)

func TestBasicSplit(t *testing.T) {
	re := MustCompile("a(.)c(.)e", 0)
	vals, err := re.Split("123abcde456aBCDe789", -1)
	if err != nil {
		t.Fatalf("Expected no error, got %v", err)
	}
	if want, got := []string{"123", "b", "d", "456aBCDe789"}, vals; !slices.Equal(want, got) {
		t.Errorf("wanted %v got %v", want, got)
	}
}

func TestBasicSplit_IgnoreCase(t *testing.T) {
	re := MustCompile("a(.)c(.)e", IgnoreCase)
	vals, err := re.Split("123abcde456aBCDe789", -1)
	if err != nil {
		t.Fatalf("Expected no error, got %v", err)
	}
	if want, got := []string{"123", "b", "d", "456", "B", "D", "789"}, vals; !slices.Equal(want, got) {
		t.Errorf("wanted %v got %v", want, got)
	}
}

func TestSplit_ZeroWidth(t *testing.T) {
	re := MustCompile(`(?<=\G..)(?=..)`, 0)
	vals, err := re.Split("aabbccdd", -1)
	if err != nil {
		t.Fatalf("Expected no error, got %v", err)
	}
	if want, got := []string{"aa", "bb", "cc", "dd"}, vals; !slices.Equal(want, got) {
		t.Errorf("wanted %v got %v", want, got)
	}
}

func TestSplit_LimitCountRemainder(t *testing.T) {
	re := MustCompile("a(.)c(.)e", IgnoreCase)
	vals, err := re.Split("123abcde456aBCDe789abcde", 2)
	if err != nil {
		t.Fatalf("Expected no error, got %v", err)
	}
	if want, got := []string{"123", "b", "d", "456", "B", "D", "789abcde"}, vals; !slices.Equal(want, got) {
		t.Errorf("wanted %v got %v", want, got)
	}
}

func TestSplit_LimitCount1(t *testing.T) {
	re := MustCompile("a(.)c(.)e", IgnoreCase)
	vals, err := re.Split("123abcde456aBCDe789abcde", 1)
	if err != nil {
		t.Fatalf("Expected no error, got %v", err)
	}
	if want, got := []string{"123abcde456aBCDe789abcde"}, vals; !slices.Equal(want, got) {
		t.Errorf("wanted %v got %v", want, got)
	}
}

func TestSplit_LimitCount0(t *testing.T) {
	re := MustCompile("a(.)c(.)e", IgnoreCase)
	vals, err := re.Split("123abcde456aBCDe789abcde", 0)
	if err != nil {
		t.Fatalf("Expected no error, got %v", err)
	}
	if want, got := []string{}, vals; !slices.Equal(want, got) {
		t.Errorf("wanted %v got %v", want, got)
	}
}
