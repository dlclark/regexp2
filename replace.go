package regexp2

import (
	"bytes"
	"errors"
)

type replacer struct {
	rep     []string
	strings []string
	rules   []int
}

// Three very similar algorithms appear below: replace (pattern),
// replace (evaluator), and split.

// Replaces all occurrences of the regex in the string with the
// replacement pattern.
//
// Note that the special case of no matches is handled on its own:
// with no matches, the input string is returned unchanged.
// The right-to-left case is split out because StringBuilder
// doesn't handle right-to-left string building directly very well.
func replace(regex Regexp, input string, count, startat int) (string, error) {
	if count < -1 {
		return "", errors.New("Count too small")
	}
	if startat < 0 || startat > len(input) {
		return "", errors.New("Begin index must not be negative and less than the length")
	}

	if count == 0 {
		return "", nil
	}

	m, err := regex.FindStringMatch(input)

	if err != nil {
		return "", err
	}
	if m == nil {
		return input, nil
	}

	buf := &bytes.Buffer{}
	text := m.text

	if !regex.RightToLeft() {
		prevat := 0
		for m != nil {
			if m.Index != prevat {
				buf.WriteString(string(text[prevat:m.Index]))
			}
			prevat = m.Index + m.Length
			replacementImpl(buf, m)
			count--
			if count == 0 {
				break
			}
			m, err = regex.FindNextMatch(m)
			if err != nil {
				return "", nil
			}
		}

		if prevat < len(text) {
			buf.WriteString(string(text[prevat:]))
		}
	} else {
		prevat := len(text)
		var al []string

		for m != nil {
			if m.Index+m.Length != prevat {
				al = append(al, string(text[m.Index+m.Length:prevat]))
			}
			prevat = m.Index
			replacementImplRTL(&al, m)
			count--
			if count == 0 {
				break
			}
			m, err = regex.FindNextMatch(m)
			if err != nil {
				return "", nil
			}
		}

		if prevat > 0 {
			buf.WriteString(string(text[:prevat]))
		}

		for i := len(al) - 1; i >= 0; i-- {
			buf.WriteString(al[i])
		}
	}

	return buf.String(), nil
}

func replacementImpl(buf *bytes.Buffer, m *Match) {

}

func replacementImplRTL(al *[]string, m *Match) {

}
