package regexp2

import (
	"fmt"
	stdregexp "regexp"
	"strings"
	"testing"
)

func BenchmarkFindRunesMatchStartingAt(b *testing.B) {
	input := []rune(strings.Repeat("x := someIdentifier + 42 // comment\n", 100))
	for _, tc := range []struct {
		name, pattern string
		wantMatch     bool
	}{
		{"no_match", `^(?:func\b)`, false},
		{"match", `^(?<identifier>\w+)`, true},
	} {
		b.Run(tc.name, func(b *testing.B) {
			re := MustCompile(tc.pattern)
			b.ReportAllocs()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				m, err := re.FindRunesMatchStartingAt(input, 0)
				if err != nil || (m != nil) != tc.wantMatch {
					b.Fatal(m, err)
				}
			}
		})
	}
}

func BenchmarkSearchCaseInsensitive(b *testing.B) {
	for _, n := range []int{16, 1024, 16384, 65536} {
		for _, tc := range []struct {
			name, input string
			want        bool
		}{
			{"lower_miss", strings.Repeat("a", n), false},
			{"upper_miss", strings.Repeat("A", n), false},
			{"no_candidates", strings.Repeat("z", n), false},
			{"late_hit", strings.Repeat("z", n) + "AB", true},
			{"early_hit", "ab" + strings.Repeat("z", n), true},
		} {
			b.Run(fmt.Sprintf("%s/%d", tc.name, n), func(b *testing.B) {
				re := MustCompile(`(?i)ab`)
				b.ReportAllocs()
				b.ResetTimer()
				for i := 0; i < b.N; i++ {
					got, err := re.MatchString(tc.input)
					if err != nil || got != tc.want {
						b.Fatal(got, err)
					}
				}
			})
		}
	}
}

// Bounded character-class repetitions are also used by Rebar's subtitle
// benchmarks. This fixture is generated locally so the benchmark is standalone.
// https://github.com/BurntSushi/rebar/blob/463d00f31887e84c38467805b9e3122c314b9521/benchmarks/definitions/curated/10-bounded-repeat.toml
func BenchmarkSearchFixedDistanceSets(b *testing.B) {
	input := strings.Repeat("The performance benchmark measures familiar functions and unexpected outcomes. 1234567890\n", 512)
	for _, tc := range []struct{ name, pattern string }{
		{"letters", `[A-Za-z]{8,13}`},
		{"alphanumeric", `[A-Za-z0-9]{8,13}`},
		{"range", `[a-z]{8,13}`},
		{"negated", `[^A-Za-z]{8,13}`},
	} {
		b.Run(tc.name, func(b *testing.B) {
			re := MustCompile(tc.pattern)
			want := len(stdregexp.MustCompile(tc.pattern).FindAllStringIndex(input, -1))
			b.ReportAllocs()
			b.SetBytes(int64(len(input)))
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				got, err := re.FindAllStringIndex(input, -1)
				if err != nil || len(got) != want {
					b.Fatal(len(got), want, err)
				}
			}
		})
	}
}

// These are the counting patterns from the Benchmarks Game regex-redux task.
// The deterministic DNA fixture exercises dense prefix candidates without
// requiring a downloaded FASTA file; this is not the full regex-redux program.
// https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/regexredux.html
func BenchmarkSearchDNAPrefixes(b *testing.B) {
	patterns := []string{
		`agggtaaa|tttaccct`,
		`[cgt]gggtaaa|tttaccc[acg]`,
		`a[act]ggtaaa|tttacc[agt]t`,
		`ag[act]gtaaa|tttac[agt]ct`,
		`agg[act]taaa|ttta[agt]cct`,
		`aggg[acg]aaa|ttt[cgt]ccct`,
		`agggt[cgt]aa|tt[acg]accct`,
		`agggta[cgt]a|t[acg]taccct`,
		`agggtaa[cgt]|[acg]ttaccct`,
	}
	data := make([]byte, 65536)
	var state uint32 = 12345
	for i := range data {
		state ^= state << 13
		state ^= state >> 17
		state ^= state << 5
		data[i] = "acgt"[state%4]
	}
	input := string(data)
	runes := []rune(input)
	for n, pattern := range patterns {
		for _, mode := range []string{"string", "runes"} {
			b.Run(fmt.Sprintf("%d/%s", n, mode), func(b *testing.B) {
				re := MustCompile(pattern, RE2)
				want := len(stdregexp.MustCompile(pattern).FindAllStringIndex(input, -1))
				b.ReportAllocs()
				b.SetBytes(int64(len(input)))
				b.ResetTimer()
				for i := 0; i < b.N; i++ {
					var got [][]int
					var err error
					if mode == "runes" {
						got, err = re.FindAllRunesIndex(runes, -1)
					} else {
						got, err = re.FindAllStringIndex(input, -1)
					}
					if err != nil || len(got) != want {
						b.Fatal(len(got), want, err)
					}
				}
			})
		}
	}
}

func BenchmarkSearchPrefixesSparse(b *testing.B) {
	for _, pattern := range []string{`apple|tiger`, `apple|apply|tiger`} {
		for _, tc := range []struct {
			name, input string
			want        bool
		}{
			{"miss", strings.Repeat("z", 32768), false},
			{"late_hit", strings.Repeat("z", 32768) + "tiger", true},
			{"short_hit", "tiger", true},
		} {
			b.Run(pattern+"/"+tc.name, func(b *testing.B) {
				re := MustCompile(pattern)
				b.ReportAllocs()
				b.ResetTimer()
				for i := 0; i < b.N; i++ {
					got, err := re.MatchString(tc.input)
					if err != nil || got != tc.want {
						b.Fatal(got, err)
					}
				}
			})
		}
	}
}

// Account for the cost of compiling and retaining the prefix search masks.
func BenchmarkSearchCompilePrefixes(b *testing.B) {
	for _, tc := range []struct{ name, pattern string }{
		{"literal", `needle`},
		{"two_distinct", `apple|tiger`},
		{"three_shared", `apple|apply|tiger`},
		{"many_distinct", hard1},
		{"dna", `[cgt]gggtaaa|tttaccc[acg]`},
	} {
		b.Run(tc.name, func(b *testing.B) {
			b.ReportAllocs()
			for i := 0; i < b.N; i++ {
				if _, err := Compile(tc.pattern); err != nil {
					b.Fatal(err)
				}
			}
		})
	}
}
