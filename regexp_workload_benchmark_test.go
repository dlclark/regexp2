package regexp2

import (
	"strings"
	"testing"
)

type matchWorkload struct {
	name        string
	pattern     string
	options     RegexOptions
	input       string
	expectMatch bool
}

type findIndicesWorkload struct {
	name        string
	pattern     string
	options     RegexOptions
	input       string
	expectCount int
}

type replaceWorkload struct {
	name    string
	pattern string
	options RegexOptions
	input   string
	repl    string
	want    string
}

func benchmarkLogCorpus(lines int) string {
	var b strings.Builder
	b.Grow(lines * 72)
	for i := 0; i < lines; i++ {
		if i%97 == 0 {
			b.WriteString("2026-01-01T00:00:00Z service=payments level=ERROR msg=timeout request_id=abc123\n")
		} else {
			b.WriteString("2026-01-01T00:00:00Z service=payments level=INFO msg=ok request_id=abc123\n")
		}
	}
	return b.String()
}

func benchmarkWordCorpus(words int) string {
	var b strings.Builder
	b.Grow(words * 8)
	for i := 0; i < words; i++ {
		if i > 0 {
			b.WriteRune(' ')
		}
		b.WriteString("word")
	}
	return b.String()
}

func workloadMatchCases() []matchWorkload {
	return []matchWorkload{
		{
			name:        "literal-log-scan",
			pattern:     "ERROR",
			input:       benchmarkLogCorpus(1200),
			expectMatch: true,
		},
		{
			name:        "alternation-log-level",
			pattern:     "(ERROR|WARN|INFO|DEBUG|TRACE)",
			input:       benchmarkLogCorpus(1200),
			expectMatch: true,
		},
		{
			name:        "lookaround-token",
			pattern:     `(?<=token=)[A-Za-z0-9_]+(?=;)`,
			input:       strings.Repeat("a=1;", 200) + "token=session_ABC123;z=9;",
			expectMatch: true,
		},
		{
			name:        "backref-duplicate-word",
			pattern:     `\b(\w+)\s+\1\b`,
			input:       "one two two three",
			expectMatch: true,
		},
		{
			name:        "unicode-letter-class",
			pattern:     `\p{L}+\s+\p{L}+`,
			input:       "creme brulee",
			expectMatch: true,
		},
		{
			name:        "hex-charclass",
			pattern:     `[A-F0-9]{32}`,
			input:       strings.Repeat("z", 1024) + "5F4DCC3B5AA765D61D8327DEB882CF99",
			expectMatch: true,
		},
		{
			name:        "rtl-number-scan",
			pattern:     `\d+`,
			options:     RightToLeft,
			input:       strings.Repeat("abc ", 300) + "id=987654321",
			expectMatch: true,
		},
	}
}

func workloadFindIndicesCases() []findIndicesWorkload {
	return []findIndicesWorkload{
		{
			name:        "all-words",
			pattern:     `\b\w+\b`,
			input:       benchmarkWordCorpus(300),
			expectCount: 300,
		},
		{
			name:        "all-log-levels",
			pattern:     `(ERROR|INFO)`,
			input:       benchmarkLogCorpus(500),
			expectCount: 500,
		},
		{
			name:        "all-tokens-lookaround",
			pattern:     `(?<=token=)[A-Za-z0-9_]+(?=;)`,
			input:       strings.Repeat("a=1;token=abc123;z=9;", 120),
			expectCount: 120,
		},
	}
}

func workloadReplaceCases() []replaceWorkload {
	return []replaceWorkload{
		{
			name:    "redact-token",
			pattern: `(?<=token=)[A-Za-z0-9_]+(?=;)`,
			input:   strings.Repeat("a=1;token=session_ABC123;z=9;", 50),
			repl:    "REDACTED",
			want:    strings.Repeat("a=1;token=REDACTED;z=9;", 50),
		},
		{
			name:    "dedupe-duplicate-words",
			pattern: `\b(\w+)\s+\1\b`,
			input:   "go go stop stop wait",
			repl:    "$1",
			want:    "go stop wait",
		},
		{
			name:    "email-domain-rewrite",
			pattern: `([A-Za-z0-9._%+-]+)@([A-Za-z0-9.-]+\.[A-Za-z]{2,})`,
			input:   "a@example.com b@example.org",
			repl:    `$1@redacted.invalid`,
			want:    "a@redacted.invalid b@redacted.invalid",
		},
	}
}

func BenchmarkWorkloadMatchString(b *testing.B) {
	for _, tc := range workloadMatchCases() {
		tc := tc
		b.Run(tc.name, func(b *testing.B) {
			re := MustCompile(tc.pattern, tc.options)
			if ok, err := re.MatchString(tc.input); err != nil || ok != tc.expectMatch {
				b.Fatalf("warmup mismatch: match=%v err=%v", ok, err)
			}

			b.ReportAllocs()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				ok, err := re.MatchString(tc.input)
				if err != nil || ok != tc.expectMatch {
					b.Fatalf("match mismatch: match=%v err=%v", ok, err)
				}
			}
		})
	}
}

func BenchmarkWorkloadMatchRunes(b *testing.B) {
	for _, tc := range workloadMatchCases() {
		tc := tc
		b.Run(tc.name, func(b *testing.B) {
			re := MustCompile(tc.pattern, tc.options)
			input := []rune(tc.input)
			if ok, err := re.MatchRunes(input); err != nil || ok != tc.expectMatch {
				b.Fatalf("warmup mismatch: match=%v err=%v", ok, err)
			}

			b.ReportAllocs()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				ok, err := re.MatchRunes(input)
				if err != nil || ok != tc.expectMatch {
					b.Fatalf("match mismatch: match=%v err=%v", ok, err)
				}
			}
		})
	}
}

func BenchmarkWorkloadFindIndices(b *testing.B) {
	for _, tc := range workloadFindIndicesCases() {
		tc := tc
		b.Run(tc.name, func(b *testing.B) {
			re := MustCompile(tc.pattern, tc.options)
			if got, err := re.FindStringMatchIndices(tc.input); err != nil || len(got) != tc.expectCount {
				b.Fatalf("warmup mismatch: count=%d err=%v", len(got), err)
			}

			b.ReportAllocs()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				got, err := re.FindStringMatchIndices(tc.input)
				if err != nil || len(got) != tc.expectCount {
					b.Fatalf("count mismatch: count=%d err=%v", len(got), err)
				}
			}
		})
	}
}

func BenchmarkWorkloadReplace(b *testing.B) {
	for _, tc := range workloadReplaceCases() {
		tc := tc
		b.Run(tc.name, func(b *testing.B) {
			re := MustCompile(tc.pattern, tc.options)
			if got, err := re.Replace(tc.input, tc.repl, -1, -1); err != nil || got != tc.want {
				b.Fatalf("warmup mismatch: output=%q err=%v", got, err)
			}

			b.ReportAllocs()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				got, err := re.Replace(tc.input, tc.repl, -1, -1)
				if err != nil || got != tc.want {
					b.Fatalf("replace mismatch: output=%q err=%v", got, err)
				}
			}
		})
	}
}

func BenchmarkWorkloadCompile(b *testing.B) {
	patterns := []struct {
		name    string
		pattern string
		options RegexOptions
	}{
		{name: "literal", pattern: "ERROR"},
		{name: "alternation", pattern: "(ERROR|WARN|INFO|DEBUG|TRACE)"},
		{name: "lookaround", pattern: `(?<=token=)[A-Za-z0-9_]+(?=;)`},
		{name: "backref", pattern: `\b(\w+)\s+\1\b`},
		{name: "unicode", pattern: `\p{L}+\s+\p{L}+`},
	}

	for _, tc := range patterns {
		tc := tc
		b.Run(tc.name, func(b *testing.B) {
			b.ReportAllocs()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				re, err := Compile(tc.pattern, tc.options)
				if err != nil {
					b.Fatalf("compile failed: %v", err)
				}
				if re == nil {
					b.Fatal("compile returned nil regexp")
				}
			}
		})
	}
}

func BenchmarkWorkloadParallelMatchRunes(b *testing.B) {
	cases := workloadMatchCases()
	for i := range cases {
		tc := cases[i]
		if tc.name != "literal-log-scan" && tc.name != "hex-charclass" && tc.name != "backref-duplicate-word" {
			continue
		}

		b.Run(tc.name, func(b *testing.B) {
			re := MustCompile(tc.pattern, tc.options)
			input := []rune(tc.input)
			b.ReportAllocs()
			b.ResetTimer()
			b.RunParallel(func(pb *testing.PB) {
				for pb.Next() {
					ok, err := re.MatchRunes(input)
					if err != nil || ok != tc.expectMatch {
						b.Fatalf("match mismatch: match=%v err=%v", ok, err)
					}
				}
			})
		})
	}
}
