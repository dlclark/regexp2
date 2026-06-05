package regexp2

import (
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
)

// TestCorpusRustRegexToml runs the raw Rust regex TOML corpus.
//
// Corpus source:
//
//	https://github.com/rust-lang/regex/tree/master/testdata
//
// The raw TOML files are checked into testdata/corpus/rust-regex unchanged.
// This runner parses the source format directly and maps only the search modes
// exposed by regexp2. Skipped categories are documented in
// testdata/corpus/rust-regex/skipped.md.
func TestCorpusRustRegexToml(t *testing.T) {
	files, err := filepath.Glob("testdata/corpus/rust-regex/*.toml")
	if err != nil {
		t.Fatal(err)
	}
	for _, file := range files {
		file := file
		t.Run(filepath.Base(file), func(t *testing.T) {
			tests := parseRustRegexCorpusFile(t, file)
			for _, tc := range tests {
				tc := tc
				t.Run(tc.name, func(t *testing.T) {
					runRustRegexCorpusTest(t, tc)
				})
			}
		})
	}
}

type rustRegexCorpusTest struct {
	file       string
	name       string
	regexRaw   string
	regexes    []string
	haystack   string
	matchesRaw string
	matches    []rustRegexMatch
	options    map[string]string
}

type rustRegexMatch struct {
	span     []int
	captures [][]int
}

func runRustRegexCorpusTest(t *testing.T, tc rustRegexCorpusTest) {
	if reason := rustRegexSkipReason(tc); reason != "" {
		t.Skip(reason)
	}

	pattern := tc.regexes[0]
	if len(tc.regexes) > 1 {
		parts := make([]string, 0, len(tc.regexes))
		for _, regex := range tc.regexes {
			parts = append(parts, "(?:"+regex+")")
		}
		pattern = strings.Join(parts, "|")
	}

	options := rustRegexCompileOptions(tc)
	if tc.options["compiles"] == "false" {
		if _, err := Compile(pattern, options...); err == nil {
			t.Fatalf("Compile(%q) succeeded, want error", pattern)
		}
		return
	}

	re, err := Compile(pattern, options...)
	if err != nil {
		t.Fatalf("Compile(%q) failed: %v", pattern, err)
	}
	got, err := rustRegexAllMatches(re, tc.haystack, rustRegexMatchLimit(tc))
	if err != nil {
		t.Fatalf("matching failed: %v", err)
	}
	if !rustRegexMatchesEqual(got, tc.matches) {
		t.Fatalf("matches = %v, want %v", got, tc.matches)
	}
	assertCorpusOptimizedEquivalent(t, pattern, options, tc.haystack)
}

func rustRegexCompileOptions(tc rustRegexCorpusTest) []CompileOption {
	var options []CompileOption
	if rustRegexOption(tc, "case-insensitive") == "true" {
		options = append(options, IgnoreCase)
	}
	if rustRegexOption(tc, "unicode") == "false" ||
		strings.Contains(tc.regexRaw, "[:") ||
		strings.Contains(tc.regexRaw, "$") ||
		strings.Contains(tc.regexRaw, "(?P<") ||
		strings.Contains(tc.regexRaw, "(?P=") {
		options = append(options, RE2)
	}
	return options
}

func rustRegexSkipReason(tc rustRegexCorpusTest) string {
	if len(tc.regexes) == 0 {
		return "empty Rust regex set has no single-regexp alternation equivalent"
	}
	if rustRegexOption(tc, "utf8") == "false" {
		return "byte/invalid-UTF8 mode is not exposed by regexp2"
	}
	if rustRegexOption(tc, "unicode") == "false" {
		return "Rust ASCII-only behavior is not supported"
	}
	if filepath.Base(tc.file) == "regex-lite.toml" {
		return "Rust regex-lite ASCII-only behavior is not supported"
	}
	if rustRegexOption(tc, "anchored") == "true" {
		return "Rust anchored automata mode is not exposed by regexp2"
	}
	if rustRegexOption(tc, "bounds") != "" {
		return "bounded search end is not exposed by regexp2"
	}
	if rustRegexOption(tc, "line-terminator") != "" || strings.Contains(tc.regexRaw, "(?R") || strings.Contains(tc.regexRaw, "(?mR") {
		return "Rust CRLF/line-terminator mode is not supported"
	}
	if strings.Contains(tc.regexRaw, "(?U") || strings.Contains(tc.regexRaw, "(?-U") {
		return "Rust swap-greed mode is not supported"
	}
	if strings.Contains(tc.regexRaw, `\b{`) {
		return "Rust special word-boundary assertions are not supported"
	}
	if rustRegexOption(tc, "search-kind") == "overlapping" || rustRegexOption(tc, "search-kind") == "earliest" {
		return "Rust overlapping/earliest search mode is not exposed by regexp2"
	}
	if rustRegexOption(tc, "match-kind") == "all" {
		return "Rust all-match mode is not equivalent to normal leftmost-first regexp matching"
	}
	if len(tc.regexes) > 1 && strings.Contains(tc.matchesRaw, "id =") {
		return "Rust regex-set all/id reporting is not equivalent to a single alternation regexp"
	}
	return ""
}

func rustRegexOption(tc rustRegexCorpusTest, key string) string {
	value := strings.TrimSpace(tc.options[key])
	if value == "" {
		return ""
	}
	if strings.HasPrefix(value, "'") && strings.HasSuffix(value, "'") {
		return strings.Trim(value, "'")
	}
	if strings.HasPrefix(value, "\"") && strings.HasSuffix(value, "\"") {
		if unquoted, err := strconv.Unquote(value); err == nil {
			return unquoted
		}
	}
	return value
}

func rustRegexMatchLimit(tc rustRegexCorpusTest) int {
	if raw := tc.options["match-limit"]; raw != "" {
		n, err := strconv.Atoi(raw)
		if err == nil {
			return n
		}
	}
	return -1
}

func rustRegexAllMatches(re *Regexp, input string, limit int) ([]rustRegexMatch, error) {
	var out []rustRegexMatch
	prevEnd := -1
	m, err := re.FindStringMatch(input)
	for m != nil && err == nil && limit != 0 {
		if m.RuneLength != 0 || m.RuneIndex != prevEnd {
			out = append(out, rustRegexFromMatch(m))
			prevEnd = m.RuneIndex + m.RuneLength
			if limit > 0 {
				limit--
			}
		}
		m, err = re.FindNextMatch(m)
	}
	return out, err
}

func rustRegexFromMatch(m *Match) rustRegexMatch {
	idx, length := m.ByteRange()
	rm := rustRegexMatch{span: []int{idx, idx + length}}
	groups := m.Groups()
	if len(groups) > 1 {
		rm.captures = make([][]int, len(groups))
		for i := range groups {
			if len(groups[i].Captures) == 0 {
				rm.captures[i] = nil
				continue
			}
			start, groupLength := groups[i].ByteRange()
			rm.captures[i] = []int{start, start + groupLength}
		}
	}
	return rm
}

func parseRustRegexCorpusFile(t *testing.T, file string) []rustRegexCorpusTest {
	t.Helper()
	b, err := os.ReadFile(file)
	if err != nil {
		t.Fatal(err)
	}
	blocks := splitRustRegexTestBlocks(string(b))
	tests := make([]rustRegexCorpusTest, 0, len(blocks))
	for _, block := range blocks {
		values := parseRustRegexTomlBlock(block)
		tc := rustRegexCorpusTest{
			file:       file,
			name:       strings.Trim(parseRustRegexString(t, values["name"]), `"`),
			regexRaw:   values["regex"],
			haystack:   parseRustRegexString(t, values["haystack"]),
			matchesRaw: values["matches"],
			options:    values,
		}
		tc.regexes = parseRustRegexRegexes(t, values["regex"])
		tc.matches = parseRustRegexMatches(t, values["matches"])
		tests = append(tests, tc)
	}
	return tests
}

func splitRustRegexTestBlocks(raw string) []string {
	var blocks []string
	var current strings.Builder
	inBlock := false
	for _, line := range strings.Split(raw, "\n") {
		if strings.TrimSpace(line) == "[[test]]" {
			if inBlock {
				blocks = append(blocks, current.String())
				current.Reset()
			}
			inBlock = true
			continue
		}
		if inBlock {
			current.WriteString(line)
			current.WriteByte('\n')
		}
	}
	if inBlock {
		blocks = append(blocks, current.String())
	}
	return blocks
}

func parseRustRegexTomlBlock(block string) map[string]string {
	values := map[string]string{}
	lines := strings.Split(block, "\n")
	for i := 0; i < len(lines); i++ {
		line := strings.TrimSpace(lines[i])
		if line == "" || strings.HasPrefix(line, "#") || !strings.Contains(line, "=") {
			continue
		}
		key, value, _ := strings.Cut(line, "=")
		key = strings.TrimSpace(key)
		value = stripRustRegexComment(strings.TrimSpace(value))
		for !rustRegexValueBalanced(value) && i+1 < len(lines) {
			i++
			value += "\n" + stripRustRegexComment(strings.TrimSpace(lines[i]))
		}
		values[key] = strings.TrimSpace(value)
	}
	return values
}

func parseRustRegexRegexes(t *testing.T, raw string) []string {
	t.Helper()
	raw = strings.TrimSpace(raw)
	if strings.HasPrefix(raw, "[") {
		return parseRustRegexStringArray(t, raw)
	}
	return []string{parseRustRegexString(t, raw)}
}

func parseRustRegexStringArray(t *testing.T, raw string) []string {
	t.Helper()
	var out []string
	for i := 0; i < len(raw); {
		for i < len(raw) && !isRustRegexQuote(raw[i]) {
			i++
		}
		if i >= len(raw) {
			break
		}
		s, next := parseRustRegexStringAt(t, raw, i)
		out = append(out, s)
		i = next
	}
	return out
}

func parseRustRegexString(t *testing.T, raw string) string {
	t.Helper()
	s, next := parseRustRegexStringAt(t, strings.TrimSpace(raw), 0)
	if strings.TrimSpace(raw[next:]) != "" {
		t.Fatalf("trailing TOML string data in %q", raw)
	}
	return s
}

func parseRustRegexStringAt(t *testing.T, raw string, start int) (string, int) {
	t.Helper()
	if strings.HasPrefix(raw[start:], "'''") {
		end := strings.Index(raw[start+3:], "'''")
		if end < 0 {
			t.Fatalf("unterminated TOML literal string %q", raw[start:])
		}
		return raw[start+3 : start+3+end], start + 3 + end + 3
	}
	if raw[start] == '\'' {
		end := strings.IndexByte(raw[start+1:], '\'')
		if end < 0 {
			t.Fatalf("unterminated TOML literal string %q", raw[start:])
		}
		return raw[start+1 : start+1+end], start + 1 + end + 1
	}
	if raw[start] == '"' {
		for end := start + 1; end < len(raw); end++ {
			if raw[end] == '\\' {
				end++
				continue
			}
			if raw[end] == '"' {
				s, err := strconv.Unquote(raw[start : end+1])
				if err != nil {
					t.Fatalf("invalid TOML basic string %q: %v", raw[start:end+1], err)
				}
				return s, end + 1
			}
		}
	}
	t.Fatalf("expected TOML string at %q", raw[start:])
	return "", 0
}

func parseRustRegexMatches(t *testing.T, raw string) []rustRegexMatch {
	t.Helper()
	raw = strings.TrimSpace(raw)
	if raw == "" || raw == "[]" || strings.Contains(raw, "id =") {
		return nil
	}
	nums := parseRustRegexInts(raw)
	if len(nums)%2 != 0 {
		t.Fatalf("odd number of match coordinates in %q", raw)
	}
	depths := rustRegexNumberDepths(raw)
	captureMode := false
	for _, depth := range depths {
		if depth >= 3 {
			captureMode = true
			break
		}
	}
	if !captureMode {
		out := make([]rustRegexMatch, 0, len(nums)/2)
		for i := 0; i < len(nums); i += 2 {
			out = append(out, rustRegexMatch{span: []int{nums[i], nums[i+1]}})
		}
		return out
	}

	var out []rustRegexMatch
	for _, matchRaw := range splitRustRegexTopLevelArrays(raw) {
		captureRaws := splitRustRegexTopLevelArrays(matchRaw)
		captures := make([][]int, 0, len(captureRaws))
		for _, captureRaw := range captureRaws {
			matchNums := parseRustRegexInts(captureRaw)
			if len(matchNums) == 0 {
				captures = append(captures, nil)
				continue
			}
			if len(matchNums) != 2 {
				t.Fatalf("invalid capture coordinates in %q", captureRaw)
			}
			captures = append(captures, []int{matchNums[0], matchNums[1]})
		}
		rm := rustRegexMatch{captures: captures}
		if len(captures) > 0 {
			rm.span = captures[0]
		}
		out = append(out, rm)
	}
	return out
}

func parseRustRegexInts(raw string) []int {
	var out []int
	for i := 0; i < len(raw); i++ {
		if raw[i] < '0' || raw[i] > '9' {
			continue
		}
		start := i
		for i < len(raw) && raw[i] >= '0' && raw[i] <= '9' {
			i++
		}
		n, _ := strconv.Atoi(raw[start:i])
		out = append(out, n)
	}
	return out
}

func rustRegexNumberDepths(raw string) []int {
	var depths []int
	depth := 0
	for i := 0; i < len(raw); i++ {
		switch raw[i] {
		case '[':
			depth++
		case ']':
			depth--
		default:
			if raw[i] >= '0' && raw[i] <= '9' {
				depths = append(depths, depth)
				for i+1 < len(raw) && raw[i+1] >= '0' && raw[i+1] <= '9' {
					i++
				}
			}
		}
	}
	return depths
}

func splitRustRegexTopLevelArrays(raw string) []string {
	raw = strings.TrimSpace(raw)
	if len(raw) < 2 {
		return nil
	}
	raw = strings.TrimSpace(raw[1 : len(raw)-1])
	var out []string
	for i := 0; i < len(raw); i++ {
		if raw[i] != '[' {
			continue
		}
		start := i
		depth := 0
		for ; i < len(raw); i++ {
			if raw[i] == '[' {
				depth++
			} else if raw[i] == ']' {
				depth--
				if depth == 0 {
					out = append(out, raw[start:i+1])
					break
				}
			}
		}
	}
	return out
}

func rustRegexMatchesEqual(a, b []rustRegexMatch) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if !corpusIntSlicesEqual(a[i].span, b[i].span) {
			return false
		}
		if len(b[i].captures) > 0 {
			if len(a[i].captures) < len(b[i].captures) {
				return false
			}
			for j := range b[i].captures {
				if !corpusIntSlicesEqual(a[i].captures[j], b[i].captures[j]) {
					return false
				}
			}
		}
	}
	return true
}

func stripRustRegexComment(s string) string {
	if strings.HasPrefix(s, "'''") || strings.HasPrefix(s, `"""`) {
		return strings.TrimSpace(s)
	}
	in := byte(0)
	escaped := false
	for i := 0; i < len(s); i++ {
		c := s[i]
		if escaped {
			escaped = false
			continue
		}
		if in == '"' && c == '\\' {
			escaped = true
			continue
		}
		if in != 0 {
			if c == in {
				in = 0
			}
			continue
		}
		if c == '\'' || c == '"' {
			in = c
			continue
		}
		if c == '#' {
			return strings.TrimSpace(s[:i])
		}
	}
	return strings.TrimSpace(s)
}

func rustRegexValueBalanced(s string) bool {
	depth := 0
	in := byte(0)
	escaped := false
	for i := 0; i < len(s); i++ {
		c := s[i]
		if escaped {
			escaped = false
			continue
		}
		if in == '"' && c == '\\' {
			escaped = true
			continue
		}
		if in != 0 {
			if c == in {
				in = 0
			}
			continue
		}
		if c == '\'' || c == '"' {
			in = c
			continue
		}
		if c == '[' {
			depth++
		} else if c == ']' {
			depth--
		}
	}
	return depth <= 0
}

func isRustRegexQuote(c byte) bool {
	return c == '\'' || c == '"'
}
