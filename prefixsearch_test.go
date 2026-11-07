package regexp2

import (
	"fmt"
	"math/rand"
	"strings"
	"testing"
	"unicode/utf8"

	"github.com/dlclark/regexp2/v2/helpers"
)

func TestCompileEqualASCIIPrefixSearch(t *testing.T) {
	for _, tc := range []struct {
		name     string
		prefixes []string
		want     bool
	}{
		{"nil", nil, false},
		{"empty", []string{}, false},
		{"single", []string{"abc"}, false},
		{"emptyPrefix", []string{"", ""}, false},
		{"unequalLengths", []string{"ab", "abc"}, false},
		{"nonASCII", []string{"é", "ab"}, false},
		{"invalidUTF8", []string{"\xffa", "ab"}, false},
		{"duplicates", []string{"ab", "ab"}, true},
		{"maximumWidth", []string{strings.Repeat("a", 32), strings.Repeat("b", 32)}, true},
		{"tooWide", []string{strings.Repeat("a", 33), strings.Repeat("b", 33)}, false},
		{"maximumAlternatives", strings.Split(strings.Repeat("a", 64), ""), true},
		{"tooManyAlternatives", strings.Split(strings.Repeat("a", 65), ""), false},
	} {
		t.Run(tc.name, func(t *testing.T) {
			if got := compileEqualASCIIPrefixSearch(tc.prefixes) != nil; got != tc.want {
				t.Fatalf("compiled = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestEqualASCIIPrefixSearch(t *testing.T) {
	for _, tc := range []struct {
		name     string
		prefixes []string
		input    []rune
	}{
		{"leftmostAlternative", []string{"tiger", "apple"}, []rune("apple tiger")},
		{"sharedFirst", []string{"abac", "abca", "acba"}, []rune("aaaabcaabac")},
		{"overlap", []string{"abab", "baba"}, []rune("ababababab")},
		{"repeatedCharacter", []string{"aaaa", "aaab"}, []rune("baaaaab")},
		{"prefixBoundary", []string{"ab", "bc"}, []rune("ac")},
		{"unicodeBoundary", []string{"abcd", "abce"}, []rune("abcéabcd")},
		{"invalidRunes", []string{"ab", "bc"}, []rune{'a', -1, 'b', 'a', utf8.MaxRune + 1, 'b', 'a', 'b'}},
		{"nilInput", []string{"ab", "bc"}, nil},
		{"maximumWidth", []string{strings.Repeat("a", 32), strings.Repeat("b", 32)}, []rune(strings.Repeat("b", 32))},
		{"maximumAlternatives", strings.Split(strings.Repeat("a", 63)+"z", ""), []rune("xyz")},
	} {
		t.Run(tc.name, func(t *testing.T) {
			checkEqualASCIIPrefixSearch(t, tc.prefixes, tc.input)
		})
	}
	// Invalid bytes reset partial ASCII matches just as decoded RuneErrors do.
	search := compileEqualASCIIPrefixSearch([]string{"abcd", "abce"})
	if got := search.indexString("abc\xffabcd", 0); got != 4 {
		t.Fatalf("invalid UTF-8: got %d, want 4", got)
	}
}

func TestEqualASCIIPrefixSearchDifferential(t *testing.T) {
	rng := rand.New(rand.NewSource(1))
	alphabet := []rune{'a', 'b', 'c', 'd', 0, 127, 128, 'é', -1, utf8.RuneError, utf8.MaxRune + 1}
	for i := 0; i < 300; i++ {
		width := 1 + rng.Intn(8)
		prefixes := make([]string, 2+rng.Intn(7))
		for j := range prefixes {
			prefix := make([]byte, width)
			for k := range prefix {
				prefix[k] = byte(alphabet[rng.Intn(6)])
			}
			prefixes[j] = string(prefix)
		}
		input := make([]rune, rng.Intn(100))
		for j := range input {
			input[j] = alphabet[rng.Intn(len(alphabet))]
		}
		if len(input) >= width {
			// Exercise exact matches as well as random misses and partial prefixes.
			copy(input[rng.Intn(len(input)-width+1):], []rune(prefixes[rng.Intn(len(prefixes))]))
		}
		checkEqualASCIIPrefixSearch(t, prefixes, input)
	}
}

func checkEqualASCIIPrefixSearch(t *testing.T, prefixes []string, input []rune) {
	t.Helper()
	search := compileEqualASCIIPrefixSearch(prefixes)
	if search == nil {
		t.Fatalf("unexpectedly ineligible prefixes: %q", prefixes)
	}
	runePrefixes := make([][]rune, len(prefixes))
	for i, prefix := range prefixes {
		runePrefixes[i] = []rune(prefix)
	}
	for startAt := -1; startAt <= len(input)+1; startAt++ {
		want := -1
		if startAt >= 0 && startAt <= len(input) {
			for i := startAt; i < len(input) && want < 0; i++ {
				for _, prefix := range runePrefixes {
					if helpers.StartsWith(input[i:], prefix) {
						want = i
						break
					}
				}
			}
		}
		if got := search.indexRunes(input, startAt); got != want {
			t.Fatalf("runes: prefixes %q input %v start %d: got %d, want %d", prefixes, input, startAt, got, want)
		}
	}
	str := string(input)
	for startAt := -1; startAt <= len(str)+1; startAt++ {
		want := -1
		if startAt >= 0 && startAt <= len(str) {
			for _, prefix := range prefixes {
				if found := strings.Index(str[startAt:], prefix); found >= 0 && (want < 0 || startAt+found < want) {
					want = startAt + found
				}
			}
		}
		if got := search.indexString(str, startAt); got != want {
			t.Fatalf("string: prefixes %q input %q start %d: got %d, want %d", prefixes, str, startAt, got, want)
		}
	}
}

func TestEqualASCIIPrefixSearchSelection(t *testing.T) {
	shared := compileEqualASCIIPrefixSearch([]string{"aaab", "aaac", "baaa"})
	distinct := compileEqualASCIIPrefixSearch([]string{"apple", "tiger"})
	for _, tc := range []struct {
		name       string
		search     *equalASCIIPrefixSearch
		input      string
		startAt    int
		wantRunes  bool
		wantString bool
	}{
		{"dense", shared, strings.Repeat("a", 1000), 0, true, true},
		{"sparse", shared, strings.Repeat("x", 1000), 0, false, false},
		{"moderate", shared, strings.Repeat("axxxxxxxxxxxxxxx", 64), 0, true, false},
		{"distinctModerate", distinct, strings.Repeat("axxxxxxxxxxxxxxx", 64), 0, false, false},
		{"distinct", distinct, strings.Repeat("a", 1000), 0, true, false},
		{"short", shared, strings.Repeat("a", 63), 0, false, false},
		{"shortTail", shared, strings.Repeat("a", 1000), 990, false, false},
		{"offset", shared, strings.Repeat("x", 64) + strings.Repeat("a", 64), 64, true, true},
		{"negative", shared, strings.Repeat("a", 1000), -1, false, false},
		{"pastEnd", shared, strings.Repeat("a", 1000), 1001, false, false},
		{"unicode", shared, strings.Repeat("é", 1000), 0, false, false},
		{"invalidBytes", shared, strings.Repeat("\xff", 1000), 0, false, false},
	} {
		t.Run(tc.name, func(t *testing.T) {
			if got := tc.search.shouldUseRunes([]rune(tc.input), tc.startAt); got != tc.wantRunes {
				t.Errorf("rune selection: got %v, want %v", got, tc.wantRunes)
			}
			if got := tc.search.shouldUseString(tc.input, tc.startAt); got != tc.wantString {
				t.Errorf("string selection: got %v, want %v", got, tc.wantString)
			}
		})
	}
}

// Compare the compiled search with the existing candidate scanning strategies.
// End-to-end regexp benchmarks separately cover decoding and engine execution.
func BenchmarkEqualASCIIPrefixSearch(b *testing.B) {
	for _, tc := range []struct {
		name     string
		prefixes []string
		input    string
	}{
		{"DenseMiss", []string{"aaab", "aaac", "aaad"}, strings.Repeat("a", 65536)},
		{"SparseMiss", []string{"aaab", "aaac", "aaad"}, strings.Repeat("x", 65536)},
		{"SparseLateHit", []string{"aaab", "aaac", "aaad"}, strings.Repeat("x", 65536) + "aaad"},
		{"ModerateMiss", []string{"aaab", "aaac", "aaad"}, strings.Repeat("axxxxxxxxxxxxxxx", 4096)},
		{"DistinctModerateMiss", []string{"apple", "tiger"}, strings.Repeat("axxxxxxxxxxxxxxx", 4096)},
		{"DistinctMiss", []string{"apple", "tiger"}, strings.Repeat("x", 65536)},
		{"ShortHit", []string{"aaab", "aaac", "aaad"}, "aaaaaaac"},
	} {
		search := compileEqualASCIIPrefixSearch(tc.prefixes)
		runePrefixes := make([][]rune, len(tc.prefixes))
		var firstRunes []rune
		seen := make(map[rune]bool)
		for i, prefix := range tc.prefixes {
			runePrefixes[i] = []rune(prefix)
			if first := rune(prefix[0]); !seen[first] {
				firstRunes = append(firstRunes, first)
				seen[first] = true
			}
		}
		var stringFallback StringPrefixFilter
		if scanner, ok := compileASCIIStringSetPrefixFilter(tc.prefixes, false, search.width); ok {
			stringFallback = scanner.index
		} else {
			stringFallback = func(input string, startAt int) (int, bool) {
				return indexAnyPrefixFallback(input, startAt, tc.prefixes, false, search.width)
			}
		}
		inputRunes := []rune(tc.input)
		for _, useSearch := range []bool{false, true} {
			b.Run(fmt.Sprintf("%s/String/Adaptive=%v", tc.name, useSearch), func(b *testing.B) {
				want := search.indexString(tc.input, 0)
				b.ReportAllocs()
				b.ResetTimer()
				for i := 0; i < b.N; i++ {
					got := -1
					if useSearch && search.shouldUseString(tc.input, 0) {
						got = search.indexString(tc.input, 0)
					} else if found, ok := stringFallback(tc.input, 0); ok {
						got = found
					}
					if got != want {
						b.Fatalf("got %d, want %d", got, want)
					}
				}
			})
			b.Run(fmt.Sprintf("%s/Runes/Adaptive=%v", tc.name, useSearch), func(b *testing.B) {
				want := search.indexRunes(inputRunes, 0)
				b.ReportAllocs()
				b.ResetTimer()
				for i := 0; i < b.N; i++ {
					got := -1
					if useSearch && search.shouldUseRunes(inputRunes, 0) {
						got = search.indexRunes(inputRunes, 0)
					} else {
						got = benchmarkEqualASCIIPrefixFallback(inputRunes, runePrefixes, firstRunes)
					}
					if got != want {
						b.Fatalf("got %d, want %d", got, want)
					}
				}
			})
		}
	}
}

func benchmarkEqualASCIIPrefixFallback(input []rune, prefixes [][]rune, firstRunes []rune) int {
	for searchAt := 0; searchAt < len(input); {
		offset := indexOfAnyRunes(input[searchAt:], firstRunes, nil)
		if offset < 0 {
			return -1
		}
		start := searchAt + offset
		for _, prefix := range prefixes {
			if prefix[0] == input[start] && helpers.StartsWith(input[start:], prefix) {
				return start
			}
		}
		searchAt = start + 1
	}
	return -1
}
