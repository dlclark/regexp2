package regexp2

import "testing"

func corpusFindStringSubmatchIndex(t *testing.T, re *Regexp, input string) (bool, []int) {
	t.Helper()

	m, err := re.FindStringMatch(input)
	if err != nil {
		t.Fatalf("FindStringMatch failed: %v", err)
	}
	if m == nil {
		return false, nil
	}
	return true, corpusMatchSubmatchIndex(m)
}

func corpusMatchSubmatchIndex(m *Match) []int {
	groups := m.Groups()
	out := make([]int, 0, len(groups)*2)
	for i := range groups {
		if len(groups[i].Captures) == 0 {
			out = append(out, -1, -1)
			continue
		}
		start, length := groups[i].ByteRange()
		out = append(out, start, start+length)
	}
	return out
}

func assertCorpusOptimizedEquivalent(t *testing.T, pattern string, options []CompileOption, input string) {
	t.Helper()

	optimized := MustCompile(pattern, options...)
	unoptimized := MustCompile(pattern, options...)
	unoptimized.stringPrefixFilter = nil

	gotMatch, got := corpusFindStringSubmatchIndex(t, optimized, input)
	wantMatch, want := corpusFindStringSubmatchIndex(t, unoptimized, input)
	if gotMatch != wantMatch || !corpusIntSlicesEqual(got, want) {
		t.Fatalf("optimized FindStringMatch = (%v, %v), unoptimized = (%v, %v)", gotMatch, got, wantMatch, want)
	}

	gotIndexes, err := optimized.FindAllStringIndex(input, -1)
	if err != nil {
		t.Fatalf("optimized FindAllStringIndex failed: %v", err)
	}
	wantIndexes, err := unoptimized.FindAllStringIndex(input, -1)
	if err != nil {
		t.Fatalf("unoptimized FindAllStringIndex failed: %v", err)
	}
	if !sameStringIndexes(gotIndexes, wantIndexes) {
		t.Fatalf("optimized FindAllStringIndex = %v, unoptimized = %v", gotIndexes, wantIndexes)
	}
}

func corpusIntSlicesEqual(a, b []int) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
