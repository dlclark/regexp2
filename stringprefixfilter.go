package regexp2

import (
	"errors"
	"strings"
	"unicode/utf8"

	"github.com/dlclark/regexp2/v2/syntax"
)

var (
	errStringStartAtTooLarge        = errors.New("startAt must be less than the length of the input string")
	errStringStartAtNotRuneBoundary = errors.New("startAt must align to the start of a valid rune in the input string")
)

// StringPrefixFilter optionally searches string input before the engine decodes it
// to runes. It returns a byte index for a candidate match start, or ok=false if
// the regex cannot match. The filter must be conservative: false positives are
// allowed, false negatives are not.
type StringPrefixFilter func(input string, startAt int) (candidateByteIndex int, ok bool)

func newStringPrefixFilter(code *syntax.Code) StringPrefixFilter {
	if code == nil || code.RightToLeft || code.FindOptimizations == nil {
		return nil
	}

	opts := code.FindOptimizations
	minRequiredLength := opts.MinRequiredLength

	switch opts.FindMode {
	case syntax.LeadingString_LeftToRight:
		return stringIndexPrefixFilter(opts.LeadingPrefix, false, minRequiredLength)
	case syntax.LeadingString_OrdinalIgnoreCase_LeftToRight:
		return stringIndexPrefixFilter(opts.LeadingPrefix, true, minRequiredLength)
	case syntax.LeadingStrings_LeftToRight:
		return stringIndexPrefixesFilter(opts.LeadingPrefixes, false, minRequiredLength)
	case syntax.LeadingStrings_OrdinalIgnoreCase_LeftToRight:
		return stringIndexPrefixesFilter(opts.LeadingPrefixes, true, minRequiredLength)
	default:
		return nil
	}
}

func stringIndexPrefixFilter(prefix string, ignoreCase bool, minRequiredLength int) StringPrefixFilter {
	if prefix == "" {
		return nil
	}
	if ignoreCase && !isASCIIString(prefix) {
		return nil
	}

	return func(input string, startAt int) (candidateByteIndex int, ok bool) {
		if !hasMinRequiredBytes(input, startAt, minRequiredLength) {
			return 0, false
		}

		var offset int
		if ignoreCase {
			offset = indexASCIIIgnoreCase(input[startAt:], prefix)
		} else {
			offset = strings.Index(input[startAt:], prefix)
		}
		if offset < 0 {
			return 0, false
		}
		return startAt + offset, true
	}
}

func stringIndexPrefixesFilter(prefixes []string, ignoreCase bool, minRequiredLength int) StringPrefixFilter {
	if len(prefixes) == 0 {
		return nil
	}
	if ignoreCase {
		for _, prefix := range prefixes {
			if !isASCIIString(prefix) {
				return nil
			}
		}
	}

	prefixes = append([]string(nil), prefixes...)
	return func(input string, startAt int) (candidateByteIndex int, ok bool) {
		if !hasMinRequiredBytes(input, startAt, minRequiredLength) {
			return 0, false
		}

		best := -1
		remaining := input[startAt:]
		for _, prefix := range prefixes {
			var offset int
			if ignoreCase {
				offset = indexASCIIIgnoreCase(remaining, prefix)
			} else {
				offset = strings.Index(remaining, prefix)
			}
			if offset >= 0 && (best < 0 || offset < best) {
				best = offset
			}
		}
		if best < 0 {
			return 0, false
		}
		return startAt + best, true
	}
}

func (re *Regexp) findStringPrefixCandidate(input string, startAt int) (candidateByteIndex int, ok bool) {
	if re.stringPrefixFilter == nil || re.RightToLeft() {
		return startAt, true
	}
	candidateByteIndex, ok = re.stringPrefixFilter(input, startAt)
	if !ok {
		return 0, false
	}
	if candidateByteIndex < startAt || candidateByteIndex > len(input) || !isStringRuneBoundary(input, candidateByteIndex) {
		return startAt, true
	}
	return candidateByteIndex, true
}

func (re *Regexp) findStringMatchStart(input string, startAt int) (candidateByteIndex int, ok bool, err error) {
	if startAt > len(input) {
		return 0, false, errStringStartAtTooLarge
	}
	if startAt >= 0 && !isStringRuneBoundary(input, startAt) {
		return 0, false, errStringStartAtNotRuneBoundary
	}

	if startAt < 0 {
		if re.RightToLeft() {
			startAt = len(input)
		} else {
			startAt = 0
		}
	}

	candidateByteIndex, ok = re.findStringPrefixCandidate(input, startAt)
	return candidateByteIndex, ok, nil
}

func hasMinRequiredBytes(input string, startAt, minRequiredLength int) bool {
	if startAt < 0 || startAt > len(input) {
		return false
	}
	return minRequiredLength <= 0 || len(input)-startAt >= minRequiredLength
}

func isStringRuneBoundary(s string, index int) bool {
	if index == 0 || index == len(s) {
		return true
	}
	if index < 0 || index > len(s) {
		return false
	}
	for strIdx := range s {
		if strIdx == index {
			return true
		}
		if strIdx > index {
			return false
		}
	}
	return false
}

func isASCIIString(s string) bool {
	for i := 0; i < len(s); i++ {
		if s[i] >= utf8.RuneSelf {
			return false
		}
	}
	return true
}

func indexASCIIIgnoreCase(s, prefix string) int {
	if len(prefix) == 0 {
		return 0
	}
	end := len(s) - len(prefix)
	for i := 0; i <= end; i++ {
		if equalASCIIIgnoreCase(s[i:i+len(prefix)], prefix) {
			return i
		}
	}
	return -1
}

func equalASCIIIgnoreCase(s, prefix string) bool {
	for i := 0; i < len(prefix); i++ {
		if foldASCII(s[i]) != foldASCII(prefix[i]) {
			return false
		}
	}
	return true
}

func foldASCII(c byte) byte {
	if 'A' <= c && c <= 'Z' {
		return c + ('a' - 'A')
	}
	return c
}
