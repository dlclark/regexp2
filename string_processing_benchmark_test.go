package regexp2

import (
	"strings"
	"testing"
)

func BenchmarkFindStringMatchMixed(b *testing.B) {
	for _, tc := range []struct{ name, pattern string }{
		{"Literal", `^a+$`},
		{"RepeatedCapture", `^(a)+$`},
	} {
		b.Run(tc.name, func(b *testing.B) {
			re := MustCompile(tc.pattern)
			hit := strings.Repeat("a", 256)
			miss := hit[:len(hit)-1] + "b"
			b.ReportAllocs()
			// Each iteration measures a miss followed by a hit on the same regexp.
			for b.Loop() {
				if m, err := re.FindStringMatch(miss); m != nil || err != nil {
					b.Fatal(m, err)
				}
				if m, err := re.FindStringMatch(hit); m == nil || err != nil {
					b.Fatal(m, err)
				}
			}
		})
	}
}

func BenchmarkFindNextMatch(b *testing.B) {
	for _, tc := range []struct{ name, pattern string }{
		{"Literal", `[ab]`},
		{"Capture", `([ab])`},
	} {
		b.Run(tc.name, func(b *testing.B) {
			re := MustCompile(tc.pattern)
			first, err := re.FindStringMatch("a b")
			if err != nil || first == nil {
				b.Fatal(first, err)
			}
			b.ReportAllocs()
			for b.Loop() {
				if m, err := re.FindNextMatch(first); m == nil || err != nil || m.String() != "b" {
					b.Fatal(m, err)
				}
			}
		})
	}
}

func BenchmarkStringOperationsNoMatch(b *testing.B) {
	for _, tc := range []struct {
		name, pattern, input string
		options              RegexOptions
	}{
		{"Anchored", `^func\b`, strings.Repeat("x := someIdentifier + 42 // comment\n", 100), None},
		{"RejectedCandidate", `func\b`, "funcx" + strings.Repeat("x", 3400), None},
		{"RightToLeft", `func\b`, strings.Repeat("x", 3400), RightToLeft},
	} {
		for _, api := range []struct {
			name string
			call func(*Regexp, string) error
		}{
			{"FindStringMatch", func(re *Regexp, input string) error {
				_, err := re.FindStringMatch(input)
				return err
			}},
			{"Replace", func(re *Regexp, input string) error {
				_, err := re.Replace(input, "replacement", -1, -1)
				return err
			}},
			{"ReplaceFunc", func(re *Regexp, input string) error {
				_, err := re.ReplaceFunc(input, func(Match) string { return "replacement" }, -1, -1)
				return err
			}},
			{"Split", func(re *Regexp, input string) error {
				_, err := re.Split(input, -1)
				return err
			}},
			{"FindAllStringIndex", func(re *Regexp, input string) error {
				_, err := re.FindAllStringIndex(input, 100)
				return err
			}},
		} {
			b.Run(api.name+"/"+tc.name, func(b *testing.B) {
				re := MustCompile(tc.pattern, tc.options)
				b.ReportAllocs()
				b.ResetTimer()
				for i := 0; i < b.N; i++ {
					if err := api.call(re, tc.input); err != nil {
						b.Fatal(err)
					}
				}
			})
		}
	}
}

func BenchmarkFindAllRunesIndexNoMatch(b *testing.B) {
	re := MustCompile(`^func\b`)
	input := []rune("x := someIdentifier + 42")
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if got, err := re.FindAllRunesIndex(input, 100); len(got) != 0 || err != nil {
			b.Fatal(got, err)
		}
	}
}

func BenchmarkStringProcessing(b *testing.B) {
	for _, tc := range []struct {
		name, input string
		options     RegexOptions
	}{
		{"ASCII", "needle" + strings.Repeat("x", 64<<10), None},
		{"Unicode", "needle" + strings.Repeat("界", 21845), None},
		{"UnicodeRTL", strings.Repeat("界", 21845) + "needle", RightToLeft},
	} {
		b.Run("FindAll/"+tc.name, func(b *testing.B) {
			re := MustCompile(`needle`, tc.options)
			b.ReportAllocs()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				got, err := re.FindAllStringIndex(tc.input, 1)
				if err != nil || len(got) != 1 || tc.input[got[0][0]:got[0][1]] != "needle" {
					b.Fatal(got, err)
				}
			}
		})
		b.Run("FindMatch/"+tc.name, func(b *testing.B) {
			re := MustCompile(`needle`, tc.options)
			b.ReportAllocs()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				got, err := re.FindStringMatch(tc.input)
				if err != nil || got == nil || got.String() != "needle" {
					b.Fatal(got, err)
				}
			}
		})
	}
	for _, tc := range []struct {
		name, input string
		options     RegexOptions
	}{
		{"Miss", strings.Repeat("x", 64<<10), None},
		{"LateHit", strings.Repeat("x", 64<<10) + "needle", None},
		{"LateHitRTL", strings.Repeat("x", 64<<10) + "needle", RightToLeft},
		{"ManyHitsRTL", strings.Repeat("xxneedlexx", 256), RightToLeft},
		{"ShortHit", "xxneedlexx", None},
	} {
		b.Run("Replace/"+tc.name, func(b *testing.B) {
			re := MustCompile(`needle`, tc.options)
			want := strings.ReplaceAll(tc.input, "needle", "pin")
			b.ReportAllocs()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				got, err := re.Replace(tc.input, "pin", -1, -1)
				if err != nil || got != want {
					b.Fatal(got, err)
				}
			}
		})
	}
	for _, tc := range []struct {
		name, pattern string
		want          int
	}{
		{"NoCaptures", `,`, 1001}, {"Captures", `(,)`, 2001},
	} {
		b.Run("Split/"+tc.name, func(b *testing.B) {
			re := MustCompile(tc.pattern)
			input := strings.Repeat("abc,", 1000) + "abc"
			b.ReportAllocs()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				got, err := re.Split(input, -1)
				if err != nil || len(got) != tc.want {
					b.Fatal(len(got), err)
				}
			}
		})
	}
}

func BenchmarkSplitUnicodePrefix(b *testing.B) {
	re := MustCompile(`(needle)`)
	prefix := strings.Repeat("界", 21845)
	input := prefix + "needle尾"
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		got, err := re.Split(input, -1)
		if err != nil || len(got) != 3 || got[0] != prefix || got[1] != "needle" || got[2] != "尾" {
			b.Fatal(got, err)
		}
	}
}
