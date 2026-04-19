package regexp2

import "testing"

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

func TestRunnerBuffersRespectCacheLimits(t *testing.T) {
	re := MustCompile(`a+`,
		OptionMaxCachedRuneBufferBytes(8),
		OptionMaxCachedReplaceBufferBytes(8),
		OptionMaxCachedReplacerDataEntries(0))
	r := re.getRunner()
	_ = r.decodeString("abcdef")
	r.replaceBuf.Grow(100)

	re.putRunner(r)

	if cap(r.runeBuf) != 0 {
		t.Fatalf("rune buffer cap = %d, want 0 after exceeding cache limit", cap(r.runeBuf))
	}
	if r.replaceBuf.Cap() != 0 {
		t.Fatalf("replace buffer cap = %d, want 0 after exceeding cache limit", r.replaceBuf.Cap())
	}
}
