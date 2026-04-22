package regexp2

import (
	"testing"

	"github.com/dlclark/regexp2/v2/syntax"
)

func TestReplacerDataCacheBounded(t *testing.T) {
	re := MustCompile(`a`, OptionMaxCachedReplacerDataEntries(2), OptionMaxCachedReplacerDataBytes(-1))
	for _, replacement := range []string{"one", "two", "three"} {
		if _, err := re.Replace("a", replacement, -1, -1); err != nil {
			t.Fatalf("Replace(%q) failed: %v", replacement, err)
		}
	}

	if got := len(re.replaceCache.cache); got != 2 {
		t.Fatalf("cache size = %d, want 2", got)
	}
	if _, ok := re.replaceCache.cache["one"]; ok {
		t.Fatalf("least recently used replacement was not evicted")
	}
}

func TestReplacerDataCacheDisabled(t *testing.T) {
	re := MustCompile(`a`, OptionMaxCachedReplacerDataEntries(0))
	if _, err := re.Replace("a", "x", -1, -1); err != nil {
		t.Fatalf("Replace failed: %v", err)
	}
	if re.replaceCache != nil {
		t.Fatalf("replaceCache was initialized when caching was disabled")
	}
}

func TestReplacerDataCacheMaxBytes(t *testing.T) {
	re := MustCompile(`a`, OptionMaxCachedReplacerDataEntries(4), OptionMaxCachedReplacerDataBytes(3))
	if _, err := re.Replace("a", "toolong", -1, -1); err != nil {
		t.Fatalf("Replace failed: %v", err)
	}
	if got := len(re.replaceCache.cache); got != 0 {
		t.Fatalf("cache size = %d, want 0", got)
	}
}

func TestFindOptimizationsCarriedToCode(t *testing.T) {
	re := MustCompile(`..abc`)
	if re.code.FindOptimizations == nil {
		t.Fatal("FindOptimizations was not retained on compiled code")
	}
	if got, want := re.code.FindOptimizations.FindMode, syntax.FixedDistanceString_LeftToRight; got != want {
		t.Fatalf("FindMode = %v, want %v", got, want)
	}
}

func TestFixedDistanceStringFindFirstChar(t *testing.T) {
	re := MustCompile(`..abc`)
	m, err := re.FindStringMatch("zzxxabc")
	if err != nil {
		t.Fatalf("FindStringMatch failed: %v", err)
	}
	if m == nil {
		t.Fatal("expected match")
	}
	if got, want := m.RuneIndex, 2; got != want {
		t.Fatalf("match index = %d, want %d", got, want)
	}
}

func TestMinRequiredLengthShortCircuits(t *testing.T) {
	re := MustCompile(`abc`)
	if got, want := re.code.FindOptimizations.MinRequiredLength, 3; got != want {
		t.Fatalf("MinRequiredLength = %d, want %d", got, want)
	}
	m, err := re.FindStringMatch("ab")
	if err != nil {
		t.Fatalf("FindStringMatch failed: %v", err)
	}
	if m != nil {
		t.Fatalf("unexpected match %q", m.String())
	}
}

func TestStringPrefixFilterFindStringMatchStartingAtInvalidBoundary(t *testing.T) {
	re := MustCompile(`abc`)
	if re.stringPrefixFilter == nil {
		t.Fatal("expected string prefix filter")
	}
	if _, err := re.FindStringMatchStartingAt("aéabc", 2); err == nil {
		t.Fatal("expected invalid rune boundary error")
	}
}

func TestStringPrefixFilterFindStringMatchCandidate(t *testing.T) {
	re := MustCompile(`abc`)
	if re.stringPrefixFilter == nil {
		t.Fatal("expected string prefix filter")
	}
	m, err := re.FindStringMatch("xxabc")
	if err != nil {
		t.Fatalf("FindStringMatch failed: %v", err)
	}
	if m == nil {
		t.Fatal("expected match")
	}
	if got, want := m.RuneIndex, 2; got != want {
		t.Fatalf("match index = %d, want %d", got, want)
	}
}
