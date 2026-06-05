package regexp2

import (
	"bufio"
	"os"
	"regexp"
	"strconv"
	"strings"
	"testing"
)

// TestCorpusRE2BasicDat runs the raw Go regexp basic.dat corpus.
//
// Corpus source:
//
//	https://github.com/golang/go/blob/master/src/regexp/testdata/basic.dat
//
// Go's regexp package is RE2-family syntax and semantics. The raw corpus is
// checked into testdata/corpus/re2/basic.dat unchanged; this runner only parses
// the raw fields and maps the supported modes onto regexp2's RE2 option.
func TestCorpusRE2BasicDat(t *testing.T) {
	file := "testdata/corpus/re2/basic.dat"
	f, err := os.Open(file)
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close() // nolint: errcheck

	scanner := bufio.NewScanner(f)
	lastPattern := ""
	for lineno := 1; scanner.Scan(); lineno++ {
		line := scanner.Text()
		if strings.TrimSpace(line) == "" || strings.HasPrefix(line, "#") {
			continue
		}

		fields := corpusRE2NotTab.FindAllString(line, -1)
		if len(fields) == 0 {
			continue
		}

		flag := fields[0]
		switch flag[0] {
		case '?', '&', '|', ';', '{', '}':
			flag = flag[1:]
			if flag == "" {
				continue
			}
		case ':':
			var ok bool
			if _, flag, ok = strings.Cut(flag[1:], ":"); !ok {
				continue
			}
		case 'C', 'N', 'T', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			continue
		}

		if len(fields) < 4 {
			t.Fatalf("%s:%d: too few fields: %q", file, lineno, line)
		}
		for i := range fields {
			switch fields[i] {
			case "NULL":
				fields[i] = ""
			case "NIL":
				t.Fatalf("%s:%d: unexpected NIL field in match corpus: %q", file, lineno, line)
			}
		}

		if strings.Contains(flag, "$") {
			fields[1] = corpusRE2UnquoteC(t, file, lineno, fields[1])
			fields[2] = corpusRE2UnquoteC(t, file, lineno, fields[2])
		}
		if fields[1] == "SAME" {
			fields[1] = lastPattern
		}
		lastPattern = fields[1]

		ok, shouldCompile, shouldMatch, want := corpusRE2ParseResult(fields[3])
		if !ok {
			t.Fatalf("%s:%d: cannot parse result %q", file, lineno, fields[3])
		}
		if !shouldCompile {
			continue
		}

		if !strings.ContainsAny(flag, "EL") {
			// POSIX BRE mode is not exposed by regexp2. See
			// testdata/corpus/re2/skipped.md.
			continue
		}

		for _, mode := range flag {
			if mode != 'E' && mode != 'L' {
				continue
			}
			pattern := fields[1]
			if mode == 'L' {
				pattern = Escape(pattern)
			}
			options := []CompileOption{RE2}
			if strings.ContainsRune(flag, 'i') {
				options = append(options, IgnoreCase)
			}
			if strings.ContainsRune(flag, 'j') {
				options = append(options, Singleline)
			}

			name := strconv.Itoa(lineno) + "/" + string(mode)
			t.Run(name, func(t *testing.T) {
				re, err := Compile(pattern, options...)
				if err != nil {
					t.Fatalf("Compile(%q) failed: %v", pattern, err)
				}
				gotMatch, got := corpusFindStringSubmatchIndex(t, re, fields[2])
				if gotMatch != shouldMatch {
					t.Fatalf("Match(%q, %q) = %v, want %v", pattern, fields[2], gotMatch, shouldMatch)
				}
				if !shouldMatch {
					return
				}
				if len(got) > len(want) {
					got = got[:len(want)]
				}
				if !corpusIntSlicesEqual(got, want) {
					t.Fatalf("FindStringSubmatchIndex(%q, %q) = %v, want %v", pattern, fields[2], got, want)
				}
				assertCorpusOptimizedEquivalent(t, pattern, options, fields[2])
			})
		}
	}
	if err := scanner.Err(); err != nil {
		t.Fatal(err)
	}
}

var corpusRE2NotTab = regexp.MustCompile(`[^\t]+`)

func corpusRE2UnquoteC(t *testing.T, file string, lineno int, s string) string {
	t.Helper()
	out, err := strconv.Unquote(`"` + s + `"`)
	if err != nil {
		t.Fatalf("%s:%d: cannot unquote %q: %v", file, lineno, s, err)
	}
	return out
}

func corpusRE2ParseResult(s string) (ok, compiled, matched bool, pos []int) {
	switch {
	case s == "":
		return true, true, true, nil
	case s == "NOMATCH":
		return true, true, false, nil
	case 'A' <= s[0] && s[0] <= 'Z':
		return true, false, false, nil
	}

	compiled = true
	var out []int
	for s != "" {
		end := byte(')')
		if len(out)%2 == 0 {
			if s[0] != '(' {
				return false, false, false, nil
			}
			s = s[1:]
			end = ','
		}
		i := strings.IndexByte(s, end)
		if i <= 0 {
			return false, false, false, nil
		}
		v := -1
		if s[:i] != "?" {
			n, err := strconv.Atoi(s[:i])
			if err != nil {
				return false, false, false, nil
			}
			v = n
		}
		out = append(out, v)
		s = s[i+1:]
	}
	if len(out)%2 != 0 {
		return false, false, false, nil
	}
	return true, true, true, out
}
