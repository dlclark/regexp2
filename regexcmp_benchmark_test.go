package regexp2

import (
	"os"
	stdregexp "regexp"
	"strconv"
	"strings"
	"testing"
)

// benchmarks from https://github.com/karust/regexcmp

const (
	regexcmpDefaultRepeats = 100
	regexcmpPayload        = "123@mail.co nümbr=+71112223334 SSN:123-45-6789 http://1.1.1.1 3FZbgi29cpjq2GjdwV8eyHuJJnkLtktZc5 Й"
)

type regexcmpCase struct {
	name    string
	pattern string
}

var regexcmpCases = []regexcmpCase{
	{
		name:    "email",
		pattern: `(?P<name>[-\w\d\.]+?)(?:\s+at\s+|\s*@\s*|\s*(?:[\[\]@]){3}\s*)(?P<host>[-\w\d\.]*?)\s*(?:dot|\.|(?:[\[\]dot\.]){3,5})\s*(?P<domain>\w+)`,
	},
	{
		name:    "bitcoin",
		pattern: `\b([13][a-km-zA-HJ-NP-Z1-9]{25,34}|bc1[ac-hj-np-zAC-HJ-NP-Z02-9]{11,71})`,
	},
	{
		name:    "ssn",
		pattern: `\d{3}-\d{2}-\d{4}`,
	},
	{
		name:    "uri",
		pattern: `[\w]+://[^/\s?#]+[^\s?#]+(?:\?[^\s#]*)?(?:#[^\s]*)?`,
	},
	{
		name:    "tel",
		pattern: `\+\d{1,4}?[-.\s]?\(?\d{1,3}?\)?[-.\s]?\d{1,4}[-.\s]?\d{1,4}[-.\s]?\d{1,9}`,
	},
}

func BenchmarkRegexcmp(b *testing.B) {
	repeats := regexcmpRepeats()
	inputString := strings.Repeat(regexcmpPayload, repeats)
	inputBytes := []byte(inputString)
	inputRunes := []rune(inputString)

	matching := regexcmpCases
	nonMatching := make([]regexcmpCase, 0, len(regexcmpCases))
	for _, tc := range regexcmpCases {
		nonMatching = append(nonMatching, regexcmpCase{
			name:    "non_matching_" + tc.name,
			pattern: tc.pattern + tc.name,
		})
	}
	grouped := []regexcmpCase{{
		name:    "grouped",
		pattern: regexcmpGroupedPattern(regexcmpCases),
	}}

	benchmarkRegexcmpSet(b, "matching", matching, repeats, inputString, inputBytes, inputRunes)
	benchmarkRegexcmpSet(b, "non_matching", nonMatching, 0, inputString, inputBytes, inputRunes)
	benchmarkRegexcmpSet(b, "grouped", grouped, len(regexcmpCases)*repeats, inputString, inputBytes, inputRunes)
}

func benchmarkRegexcmpSet(b *testing.B, setName string, cases []regexcmpCase, wantCount int, inputString string, inputBytes []byte, inputRunes []rune) {
	for _, tc := range cases {
		b.Run(setName+"/regexp/"+tc.name, func(b *testing.B) {
			re := stdregexp.MustCompile(tc.pattern)
			b.ReportAllocs()
			b.SetBytes(int64(len(inputBytes)))
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				if count := len(re.FindAllIndex(inputBytes, -1)); count != wantCount {
					b.Fatalf("count = %d, want %d", count, wantCount)
				}
			}
		})

		b.Run(setName+"/regexp2_string/"+tc.name, func(b *testing.B) {
			re := MustCompile(tc.pattern, RE2)
			b.ReportAllocs()
			b.SetBytes(int64(len(inputString)))
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				matches, err := re.FindAllStringIndex(inputString, -1)
				if err != nil {
					b.Fatal(err)
				}
				count := len(matches)
				if count != wantCount {
					b.Fatalf("count = %d, want %d", count, wantCount)
				}
			}
		})

		b.Run(setName+"/regexp2_runes/"+tc.name, func(b *testing.B) {
			re := MustCompile(tc.pattern, RE2)
			b.ReportAllocs()
			b.SetBytes(int64(len(inputRunes)))
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				matches, err := re.FindAllRunesIndex(inputRunes, -1)
				if err != nil {
					b.Fatal(err)
				}
				count := len(matches)
				if count != wantCount {
					b.Fatalf("count = %d, want %d", count, wantCount)
				}
			}
		})
	}
}

func regexcmpGroupedPattern(cases []regexcmpCase) string {
	var b strings.Builder
	for i, tc := range cases {
		if i > 0 {
			b.WriteByte('|')
		}
		b.WriteString("(?P<")
		b.WriteString(tc.name)
		b.WriteByte('>')
		b.WriteString(tc.pattern)
		b.WriteByte(')')
	}
	return b.String()
}

func regexcmpRepeats() int {
	raw := os.Getenv("REGEXCMP_REPEAT")
	if raw == "" {
		return regexcmpDefaultRepeats
	}
	n, err := strconv.Atoi(raw)
	if err != nil || n < 1 {
		return regexcmpDefaultRepeats
	}
	return n
}
