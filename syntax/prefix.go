package syntax

import (
	"bytes"
	"unicode"
)

type Prefix struct {
	PrefixStr       []rune
	CaseInsensitive bool
}

// It takes a RegexTree and computes the set of chars that can start it.
func getFirstCharsPrefix(tree *RegexTree) (*Prefix, error) {
	return nil, nil
	//TODO: this
	//panic("not implemented")
}

// This is a related computation: it takes a RegexTree and computes the
// leading substring if it sees one. It's quite trivial and gives up easily.
func getPrefix(tree *RegexTree) *Prefix {
	var concatNode *regexNode
	nextChild := 0

	curNode := tree.root

	for {
		switch curNode.t {
		case ntConcatenate:
			if len(curNode.children) > 0 {
				concatNode = curNode
				nextChild = 0
			}

		case ntGreedy, ntCapture:
			curNode = curNode.children[0]
			concatNode = nil
			continue

		case ntOneloop, ntOnelazy:
			if curNode.m > 0 {
				return &Prefix{
					PrefixStr:       repeat(curNode.ch, curNode.m),
					CaseInsensitive: (curNode.options & IgnoreCase) != 0,
				}
			}
			return nil

		case ntOne:
			return &Prefix{
				PrefixStr:       []rune{curNode.ch},
				CaseInsensitive: (curNode.options & IgnoreCase) != 0,
			}

		case ntMulti:
			return &Prefix{
				PrefixStr:       curNode.str,
				CaseInsensitive: (curNode.options & IgnoreCase) != 0,
			}

		case ntBol, ntEol, ntBoundary, ntECMABoundary, ntBeginning, ntStart,
			ntEndZ, ntEnd, ntEmpty, ntRequire, ntPrevent:

		default:
			return nil
		}

		if concatNode == nil || nextChild >= len(concatNode.children) {
			return nil
		}

		curNode = concatNode.children[nextChild]
		nextChild++
	}
}

func repeat(r rune, c int) []rune {
	ret := make([]rune, c)
	for i := 0; i < c; i++ {
		ret[i] = r
	}
	return ret
}

// BmPrefix precomputes the Boyer-Moore
// tables for fast string scanning. These tables allow
// you to scan for the first occurrence of a string within
// a large body of text without examining every character.
// The performance of the heuristic depends on the actual
// string and the text being searched, but usually, the longer
// the string that is being searched for, the fewer characters
// need to be examined.
type BmPrefix struct {
	positive        []int
	negativeASCII   []int
	negativeUnicode [][]int
	pattern         []rune
	lowASCII        rune
	highASCII       rune
	rightToLeft     bool
	caseInsensitive bool
}

func newBmPrefix(pattern []rune, caseInsensitive, rightToLeft bool) *BmPrefix {

	b := &BmPrefix{
		rightToLeft:     rightToLeft,
		caseInsensitive: caseInsensitive,
		pattern:         pattern,
	}

	if caseInsensitive {
		for i := 0; i < len(b.pattern); i++ {
			// We do the ToLower character by character for consistency.  With surrogate chars, doing
			// a ToLower on the entire string could actually change the surrogate pair.  This is more correct
			// linguistically, but since Regex doesn't support surrogates, it's more important to be
			// consistent.

			b.pattern[i] = unicode.ToLower(b.pattern[i])
		}
	}

	var beforefirst, last, bump int
	var scan, match int

	if !rightToLeft {
		beforefirst = -1
		last = len(b.pattern) - 1
		bump = 1
	} else {
		beforefirst = len(b.pattern)
		last = 0
		bump = -1
	}

	// PART I - the good-suffix shift table
	//
	// compute the positive requirement:
	// if char "i" is the first one from the right that doesn't match,
	// then we know the matcher can advance by _positive[i].
	//
	// This algorithm is a simplified variant of the standard
	// Boyer-Moore good suffix calculation.

	b.positive = make([]int, len(b.pattern))

	examine := last
	ch := b.pattern[examine]
	b.positive[examine] = bump
	examine -= bump

Outerloop:
	for {
		// find an internal char (examine) that matches the tail

		for {
			if examine == beforefirst {
				break Outerloop
			}
			if b.pattern[examine] == ch {
				break
			}
			examine -= bump
		}

		match = last
		scan = examine

		// find the length of the match

		for {
			if scan == beforefirst || b.pattern[match] != b.pattern[scan] {
				// at the end of the match, note the difference in _positive
				// this is not the length of the match, but the distance from the internal match
				// to the tail suffix.
				if b.positive[match] == 0 {
					b.positive[match] = match - scan
				}

				// System.Diagnostics.Debug.WriteLine("Set positive[" + match + "] to " + (match - scan));

				break
			}

			scan -= bump
			match -= bump
		}

		examine -= bump
	}

	match = last - bump

	// scan for the chars for which there are no shifts that yield a different candidate

	// The inside of the if statement used to say
	// "_positive[match] = last - beforefirst;"
	// This is slightly less aggressive in how much we skip, but at worst it
	// should mean a little more work rather than skipping a potential match.
	for match != beforefirst {
		if b.positive[match] == 0 {
			b.positive[match] = bump
		}

		match -= bump
	}

	// PART II - the bad-character shift table
	//
	// compute the negative requirement:
	// if char "ch" is the reject character when testing position "i",
	// we can slide up by _negative[ch];
	// (_negative[ch] = str.Length - 1 - str.LastIndexOf(ch))
	//
	// the lookup table is divided into ASCII and Unicode portions;
	// only those parts of the Unicode 16-bit code set that actually
	// appear in the string are in the table. (Maximum size with
	// Unicode is 65K; ASCII only case is 512 bytes.)

	b.negativeASCII = make([]int, 128)

	for i := 0; i < 128; i++ {
		b.negativeASCII[i] = last - beforefirst
	}

	b.lowASCII = 127
	b.highASCII = 0

	for examine = last; examine != beforefirst; examine -= bump {
		ch = b.pattern[examine]

		if ch < 128 {
			if b.lowASCII > ch {
				b.lowASCII = ch
			}

			if b.highASCII < ch {
				b.highASCII = ch
			}

			if b.negativeASCII[ch] == last-beforefirst {
				b.negativeASCII[ch] = last - examine
			}
		} else {
			i, j := ch>>8, ch&0xFF

			if b.negativeUnicode == nil {
				b.negativeUnicode = make([][]int, 256)
			}

			if b.negativeUnicode[i] == nil {
				newarray := make([]int, 256)

				for k := 0; k < 256; k++ {
					newarray[k] = last - beforefirst
				}

				if i == 0 {
					copy(newarray, b.negativeASCII)
					//TODO: this line needed?
					b.negativeASCII = newarray
				}

				b.negativeUnicode[i] = newarray
			}

			if b.negativeUnicode[i][j] == last-beforefirst {
				b.negativeUnicode[i][j] = last - examine
			}
		}
	}

	return b
}

func (b *BmPrefix) String() string {
	return string(b.pattern)
}

func (b *BmPrefix) Dump(indent string) string {
	return ""
}

// Scan uses the Boyer-Moore algorithm to find the first occurrence
// of the specified string within text, beginning at index, and
// constrained within beglimit and endlimit.
//
// The direction and case-sensitivity of the match is determined
// by the arguments to the RegexBoyerMoore constructor.
func (b *BmPrefix) Scan(text []rune, index, beglimit, endlimit int) int {
	var (
		defadv, test, test2         int
		match, startmatch, endmatch int
		bump, advance               int
		chTest                      rune
		unicodeLookup               []int
	)

	if !b.rightToLeft {
		defadv = len(b.pattern)
		startmatch = len(b.pattern) - 1
		endmatch = 0
		test = index + defadv - 1
		bump = 1
	} else {
		defadv = -len(b.pattern)
		startmatch = 0
		endmatch = -defadv - 1
		test = index + defadv
		bump = -1
	}

	chMatch := b.pattern[startmatch]

	for {
		if test >= endlimit || test < beglimit {
			return -1
		}

		chTest = text[test]

		if b.caseInsensitive {
			chTest = unicode.ToLower(chTest)
		}

		if chTest != chMatch {
			if chTest < 128 {
				advance = b.negativeASCII[chTest]
			} else if nil != b.negativeUnicode {
				unicodeLookup = b.negativeUnicode[chTest>>8]
				if unicodeLookup != nil {
					advance = unicodeLookup[chTest&0xFF]
				} else {
					advance = defadv
				}
			} else {
				advance = defadv
			}

			test += advance
		} else { // if (chTest == chMatch)
			test2 = test
			match = startmatch

			for {
				if match == endmatch {
					if b.rightToLeft {
						return test2 + 1
					} else {
						return test2
					}
				}

				match -= bump
				test2 -= bump

				chTest = text[test2]

				if b.caseInsensitive {
					chTest = unicode.ToLower(chTest)
				}

				if chTest != b.pattern[match] {
					advance = b.positive[match]
					if (chTest & 0xFF80) == 0 {
						test2 = (match - startmatch) + b.negativeASCII[chTest]
					} else if nil != b.negativeUnicode {
						unicodeLookup = b.negativeUnicode[chTest>>8]
						if unicodeLookup != nil {
							test2 = (match - startmatch) + unicodeLookup[chTest&0xFF]
						} else {
							test += advance
							break
						}
					} else {
						test += advance
						break
					}

					if b.rightToLeft {
						if test2 < advance {
							advance = test2
						}
					} else if test2 > advance {
						advance = test2
					}

					test += advance
					break
				}
			}
		}
	}
}

// When a regex is anchored, we can do a quick IsMatch test instead of a Scan
func (b *BmPrefix) IsMatch(text []rune, index, beglimit, endlimit int) bool {
	if !b.rightToLeft {
		if index < beglimit || endlimit-index < len(b.pattern) {
			return false
		}

		return b.matchPattern(text, index)
	} else {
		if index > endlimit || index-beglimit < len(b.pattern) {
			return false
		}

		return b.matchPattern(text, index-len(b.pattern))
	}
}

func (b *BmPrefix) matchPattern(text []rune, index int) bool {
	if len(text)-index < len(b.pattern) {
		return false
	}

	if b.caseInsensitive {
		for i := 0; i < len(b.pattern); i++ {
			//Debug.Assert(textinfo.ToLower(_pattern[i]) == _pattern[i], "pattern should be converted to lower case in constructor!");
			if unicode.ToLower(text[index+i]) != b.pattern[i] {
				return false
			}
		}
		return true
	} else {
		for i := 0; i < len(b.pattern); i++ {
			if text[index+i] != b.pattern[i] {
				return false
			}
		}
		return true
	}
}

type AnchorLoc int16

// where the regex can be pegged
const (
	AnchorBeginning    AnchorLoc = 0x0001
	AnchorBol                    = 0x0002
	AnchorStart                  = 0x0004
	AnchorEol                    = 0x0008
	AnchorEndZ                   = 0x0010
	AnchorEnd                    = 0x0020
	AnchorBoundary               = 0x0040
	AnchorECMABoundary           = 0x0080
)

func getAnchors(tree *RegexTree) AnchorLoc {

	var concatNode *regexNode
	nextChild, result := 0, AnchorLoc(0)

	curNode := tree.root

	for {
		switch curNode.t {
		case ntConcatenate:
			if len(curNode.children) > 0 {
				concatNode = curNode
				nextChild = 0
			}

		case ntGreedy, ntCapture:
			curNode = curNode.children[0]
			concatNode = nil
			continue

		case ntBol, ntEol, ntBoundary, ntECMABoundary, ntBeginning,
			ntStart, ntEndZ, ntEnd:
			return result | anchorFromType(curNode.t)

		case ntEmpty, ntRequire, ntPrevent:

		default:
			return result
		}

		if concatNode == nil || nextChild >= len(concatNode.children) {
			return result
		}

		curNode = concatNode.children[nextChild]
		nextChild++
	}
}

func anchorFromType(t nodeType) AnchorLoc {
	switch t {
	case ntBol:
		return AnchorBol
	case ntEol:
		return AnchorEol
	case ntBoundary:
		return AnchorBoundary
	case ntECMABoundary:
		return AnchorECMABoundary
	case ntBeginning:
		return AnchorBeginning
	case ntStart:
		return AnchorStart
	case ntEndZ:
		return AnchorEndZ
	case ntEnd:
		return AnchorEnd
	default:
		return 0
	}
}

// anchorDescription returns a human-readable description of the anchors
func (anchors AnchorLoc) String() string {
	buf := &bytes.Buffer{}

	if 0 != (anchors & AnchorBeginning) {
		buf.WriteString(", Beginning")
	}
	if 0 != (anchors & AnchorStart) {
		buf.WriteString(", Start")
	}
	if 0 != (anchors & AnchorBol) {
		buf.WriteString(", Bol")
	}
	if 0 != (anchors & AnchorBoundary) {
		buf.WriteString(", Boundary")
	}
	if 0 != (anchors & AnchorECMABoundary) {
		buf.WriteString(", ECMABoundary")
	}
	if 0 != (anchors & AnchorEol) {
		buf.WriteString(", Eol")
	}
	if 0 != (anchors & AnchorEnd) {
		buf.WriteString(", End")
	}
	if 0 != (anchors & AnchorEndZ) {
		buf.WriteString(", EndZ")
	}

	// trim off comma
	if buf.Len() >= 2 {
		return buf.String()[2:]
	}
	return "None"
}
