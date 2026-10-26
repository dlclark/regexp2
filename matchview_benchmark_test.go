package regexp2

import (
	"strings"
	"testing"
)

func BenchmarkFindRunesMatches(b *testing.B) {
	for _, tc := range []struct {
		name, pattern, input string
		options              RegexOptions
		group                int
		matches              int
	}{
		{"single", `(a)`, "a", None, 1, 1},
		{"captures", `(a)`, strings.Repeat("a", 1000), None, 1, 1000},
		{"ecmascript", `(a)`, strings.Repeat("a", 1000), ECMAScript, 1, 1000},
		{"sparse", `(?<100>a)`, strings.Repeat("a", 1000), None, 100, 1000},
		{"balancing", `(?<x>a)+(?<-x>b)+`, strings.Repeat(strings.Repeat("a", 64)+strings.Repeat("b", 32)+" ", 100), None, 1, 100},
	} {
		re := MustCompile(tc.pattern, tc.options)
		input := []rune(tc.input)
		b.Run(tc.name+"/view", func(b *testing.B) {
			b.ReportAllocs()
			for b.Loop() {
				count := 0
				err := re.FindRunesMatches(input, -1, func(v MatchView) bool {
					_, _, ok := v.GroupRange(tc.group)
					if !ok {
						b.Fatal("missing capture")
					}
					count++
					return true
				})
				if err != nil || count != tc.matches {
					b.Fatalf("matches=%d err=%v", count, err)
				}
			}
		})
		b.Run(tc.name+"/full_match", func(b *testing.B) {
			b.ReportAllocs()
			for b.Loop() {
				count := 0
				m, err := re.FindRunesMatch(input)
				for m != nil && err == nil {
					g := m.GroupByNumber(tc.group)
					if g == nil || len(g.Captures) == 0 {
						b.Fatal("missing capture")
					}
					count++
					m, err = re.FindNextMatch(m)
				}
				if err != nil || count != tc.matches {
					b.Fatalf("matches=%d err=%v", count, err)
				}
			}
		})
	}
}
