package runecacher

import (
	"testing"
)

func TestStringBasicCacheFirstChar(t *testing.T) {
	rc := NewFromString("test")
	if want, got := 't', rc.RuneAt(0); want != got {
		t.Fatalf("wanted %v, got %v", want, got)
	}
}

func TestStringEnsureCached(t *testing.T) {
	rc := NewFromString("test")

	if want, got := cachePrimeSize, len(rc.runes); want != got {
		t.Fatalf("wanted %v, got %v", want, got)
	}

	if want, got := 't', rc.runes[0]; want != got {
		t.Fatalf("wanted %v, got %v", want, got)
	}
	if want, got := 't', rc.RuneAt(0); want != got {
		t.Fatalf("wanted %v, got %v", want, got)
	}

	rc.cachedNext(1)
	if want, got := 'e', rc.runes[1]; want != got {
		t.Fatalf("wanted %v, got %v", want, got)
	}
	if want, got := 'e', rc.RuneAt(1); want != got {
		t.Fatalf("wanted %v, got %v", want, got)
	}

	rc.cachedNext(2)
	if want, got := 's', rc.runes[2]; want != got {
		t.Fatalf("wanted %v, got %v", want, got)
	}
	if want, got := 't', rc.runes[3]; want != got {
		t.Fatalf("wanted %v, got %v", want, got)
	}

	if want, got := 's', rc.RuneAt(2); want != got {
		t.Fatalf("wanted %v, got %v", want, got)
	}
	if want, got := 't', rc.RuneAt(3); want != got {
		t.Fatalf("wanted %v, got %v", want, got)
	}
}

func TestStringBasicCacheSecondChar(t *testing.T) {
	rc := NewFromString("test")
	t.Logf("runes: %v", rc.runes)
	if want, got := 'e', rc.RuneAt(1); want != got {
		t.Fatalf("wanted %v, got %v", want, got)
	}
}

func TestRuneBasicCacheFirstChar(t *testing.T) {
	rc := NewFromRunes([]rune("test"))
	if want, got := 't', rc.RuneAt(0); want != got {
		t.Fatalf("wanted %v, got %v", want, got)
	}
}

func TestRuneBasicCacheSecondChar(t *testing.T) {
	rc := NewFromRunes([]rune("test"))
	if want, got := 'e', rc.RuneAt(1); want != got {
		t.Fatalf("wanted %v, got %v", want, got)
	}
}
