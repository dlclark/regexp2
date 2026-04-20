// Package compat provides a regexp2 adapter with regexp.Regexp-compatible
// matching method signatures.
package compat

import (
	"io"

	regexp2 "github.com/dlclark/regexp2/v2"
)

// Regexp adapts a regexp2.Regexp to the matching method signatures of
// regexp.Regexp.
//
// The wrapped regexp2 engine can return match-time errors, most commonly
// timeouts. Since regexp.Regexp matching methods do not return errors, this
// adapter panics if the wrapped regexp2 matcher returns one.
type Regexp struct {
	re *regexp2.Regexp
}

// Compile parses a regular expression and returns a compat adapter for it.
func Compile(expr string, options ...regexp2.CompileOption) (*Regexp, error) {
	re, err := regexp2.Compile(expr, options...)
	if err != nil {
		return nil, err
	}
	return Wrap(re), nil
}

// MustCompile is like Compile but panics if the expression cannot be parsed.
func MustCompile(str string, options ...regexp2.CompileOption) *Regexp {
	return Wrap(regexp2.MustCompile(str, options...))
}

// Wrap returns an adapter around re.
func Wrap(re *regexp2.Regexp) *Regexp {
	return &Regexp{re: re}
}

// Unwrap returns the wrapped regexp2 regular expression.
func (re *Regexp) Unwrap() *regexp2.Regexp {
	return re.re
}

// String returns the source text used to compile the regular expression.
func (re *Regexp) String() string {
	return re.re.String()
}

// Match reports whether the byte slice b contains any match of the regular expression.
func (re *Regexp) Match(b []byte) bool {
	return re.MatchString(string(b))
}

// MatchString reports whether the string s contains any match of the regular expression.
func (re *Regexp) MatchString(s string) bool {
	ok, err := re.re.MatchString(s)
	must(err)
	return ok
}

// MatchReader reports whether text returned by r contains any match of the regular expression.
func (re *Regexp) MatchReader(r io.RuneReader) bool {
	text, _, err := readRunes(r)
	must(err)
	ok, err := re.re.MatchRunes(text)
	must(err)
	return ok
}

// Find returns a slice holding the text of the leftmost match in b.
func (re *Regexp) Find(b []byte) []byte {
	loc := re.FindIndex(b)
	if loc == nil {
		return nil
	}
	return b[loc[0]:loc[1]]
}

// FindIndex returns a two-element slice defining the location of the leftmost
// match in b.
func (re *Regexp) FindIndex(b []byte) []int {
	m := re.findStringMatch(string(b))
	if m == nil {
		return nil
	}
	return captureIndex(&m.Group.Capture)
}

// FindString returns a string holding the text of the leftmost match in s.
func (re *Regexp) FindString(s string) string {
	m := re.findStringMatch(s)
	if m == nil {
		return ""
	}
	return m.String()
}

// FindStringIndex returns a two-element slice defining the location of the
// leftmost match in s.
func (re *Regexp) FindStringIndex(s string) []int {
	m := re.findStringMatch(s)
	if m == nil {
		return nil
	}
	return captureIndex(&m.Group.Capture)
}

// FindReaderIndex returns a two-element slice defining the byte location of the
// leftmost match in text read from r.
func (re *Regexp) FindReaderIndex(r io.RuneReader) []int {
	text, offsets, err := readRunes(r)
	must(err)
	m := re.findRunesMatch(text)
	if m == nil {
		return nil
	}
	return runeCaptureIndex(&m.Group.Capture, offsets)
}

// FindSubmatch returns a slice of byte slices holding the text of the leftmost
// match and its submatches.
func (re *Regexp) FindSubmatch(b []byte) [][]byte {
	loc := re.FindSubmatchIndex(b)
	if loc == nil {
		return nil
	}
	out := make([][]byte, len(loc)/2)
	for i := range out {
		start, end := loc[2*i], loc[2*i+1]
		if start >= 0 {
			out[i] = b[start:end]
		}
	}
	return out
}

// FindSubmatchIndex returns a slice holding the byte index pairs of the
// leftmost match and its submatches.
func (re *Regexp) FindSubmatchIndex(b []byte) []int {
	return re.FindStringSubmatchIndex(string(b))
}

// FindStringSubmatch returns a slice of strings holding the text of the
// leftmost match and its submatches.
func (re *Regexp) FindStringSubmatch(s string) []string {
	m := re.findStringMatch(s)
	if m == nil {
		return nil
	}
	return matchStrings(m)
}

// FindStringSubmatchIndex returns a slice holding the byte index pairs of the
// leftmost match and its submatches.
func (re *Regexp) FindStringSubmatchIndex(s string) []int {
	m := re.findStringMatch(s)
	if m == nil {
		return nil
	}
	return matchIndexes(m)
}

// FindReaderSubmatchIndex returns a slice holding the byte index pairs of the
// leftmost match and its submatches in text read from r.
func (re *Regexp) FindReaderSubmatchIndex(r io.RuneReader) []int {
	text, offsets, err := readRunes(r)
	must(err)
	m := re.findRunesMatch(text)
	if m == nil {
		return nil
	}
	return matchRuneIndexes(m, offsets)
}

// FindAll returns a slice of all successive matches in b.
func (re *Regexp) FindAll(b []byte, n int) [][]byte {
	locs := re.FindAllIndex(b, n)
	if locs == nil {
		return nil
	}
	out := make([][]byte, len(locs))
	for i, loc := range locs {
		out[i] = b[loc[0]:loc[1]]
	}
	return out
}

// FindAllIndex returns a slice of byte index pairs for all successive matches in b.
func (re *Regexp) FindAllIndex(b []byte, n int) [][]int {
	return re.FindAllStringIndex(string(b), n)
}

// FindAllString returns a slice of all successive matches in s.
func (re *Regexp) FindAllString(s string, n int) []string {
	if n == 0 {
		return nil
	}
	var out []string
	re.forEachStringMatch(s, n, func(m *regexp2.Match) {
		out = append(out, m.String())
	})
	return out
}

// FindAllStringIndex returns a slice of byte index pairs for all successive matches in s.
func (re *Regexp) FindAllStringIndex(s string, n int) [][]int {
	if n == 0 {
		return nil
	}
	var out [][]int
	re.forEachStringMatch(s, n, func(m *regexp2.Match) {
		out = append(out, captureIndex(&m.Group.Capture))
	})
	return out
}

// FindAllSubmatch returns a slice of all successive matches and their submatches in b.
func (re *Regexp) FindAllSubmatch(b []byte, n int) [][][]byte {
	locs := re.FindAllSubmatchIndex(b, n)
	if locs == nil {
		return nil
	}
	out := make([][][]byte, len(locs))
	for i, loc := range locs {
		out[i] = make([][]byte, len(loc)/2)
		for j := range out[i] {
			start, end := loc[2*j], loc[2*j+1]
			if start >= 0 {
				out[i][j] = b[start:end]
			}
		}
	}
	return out
}

// FindAllSubmatchIndex returns a slice of byte index pairs for all successive
// matches and their submatches in b.
func (re *Regexp) FindAllSubmatchIndex(b []byte, n int) [][]int {
	return re.FindAllStringSubmatchIndex(string(b), n)
}

// FindAllStringSubmatch returns a slice of all successive matches and their
// submatches in s.
func (re *Regexp) FindAllStringSubmatch(s string, n int) [][]string {
	if n == 0 {
		return nil
	}
	var out [][]string
	re.forEachStringMatch(s, n, func(m *regexp2.Match) {
		out = append(out, matchStrings(m))
	})
	return out
}

// FindAllStringSubmatchIndex returns a slice of byte index pairs for all
// successive matches and their submatches in s.
func (re *Regexp) FindAllStringSubmatchIndex(s string, n int) [][]int {
	if n == 0 {
		return nil
	}
	var out [][]int
	re.forEachStringMatch(s, n, func(m *regexp2.Match) {
		out = append(out, matchIndexes(m))
	})
	return out
}

func (re *Regexp) findStringMatch(s string) *regexp2.Match {
	m, err := re.re.FindStringMatch(s)
	must(err)
	return m
}

func (re *Regexp) findRunesMatch(r []rune) *regexp2.Match {
	m, err := re.re.FindRunesMatch(r)
	must(err)
	return m
}

func (re *Regexp) forEachStringMatch(s string, n int, f func(*regexp2.Match)) {
	m := re.findStringMatch(s)
	prevEnd := -1
	for m != nil && n != 0 {
		if !(m.RuneLength == 0 && m.RuneIndex == prevEnd) {
			f(m)
			prevEnd = m.RuneIndex + m.RuneLength
			if n > 0 {
				n--
				if n == 0 {
					break
				}
			}
		}
		next, err := re.re.FindNextMatch(m)
		must(err)
		m = next
	}
}

func matchStrings(m *regexp2.Match) []string {
	groups := m.Groups()
	out := make([]string, len(groups))
	for i := range groups {
		if len(groups[i].Captures) > 0 {
			out[i] = groups[i].String()
		}
	}
	return out
}

func matchIndexes(m *regexp2.Match) []int {
	groups := m.Groups()
	out := make([]int, 2*len(groups))
	for i := range groups {
		if len(groups[i].Captures) == 0 {
			out[2*i], out[2*i+1] = -1, -1
			continue
		}
		idx := captureIndex(&groups[i].Capture)
		out[2*i], out[2*i+1] = idx[0], idx[1]
	}
	return out
}

func matchRuneIndexes(m *regexp2.Match, offsets []int) []int {
	groups := m.Groups()
	out := make([]int, 2*len(groups))
	for i := range groups {
		if len(groups[i].Captures) == 0 {
			out[2*i], out[2*i+1] = -1, -1
			continue
		}
		idx := runeCaptureIndex(&groups[i].Capture, offsets)
		out[2*i], out[2*i+1] = idx[0], idx[1]
	}
	return out
}

func captureIndex(c *regexp2.Capture) []int {
	start, length := c.ByteRange()
	return []int{start, start + length}
}

func runeCaptureIndex(c *regexp2.Capture, offsets []int) []int {
	return []int{offsets[c.RuneIndex], offsets[c.RuneIndex+c.RuneLength]}
}

func readRunes(r io.RuneReader) ([]rune, []int, error) {
	var text []rune
	offsets := []int{0}
	for {
		ch, size, err := r.ReadRune()
		if err == io.EOF {
			return text, offsets, nil
		}
		if err != nil {
			return nil, nil, err
		}
		text = append(text, ch)
		offsets = append(offsets, offsets[len(offsets)-1]+size)
	}
}

func must(err error) {
	if err != nil {
		panic(err)
	}
}
