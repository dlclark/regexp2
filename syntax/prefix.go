package syntax

import (
	"bytes"
	"strings"
)

type Prefix struct {
	Prefix          string
	CaseInsensitive bool
}

// It takes a RegexTree and computes the set of chars that can start it.
func getFirstCharsPrefix(tree *RegexTree) (*Prefix, error) {
	return nil, nil
	//TODO: this
	//panic("not implemented")
}

// This is a related computation: it takes a RegexTree and computes the
// leading substring if it see one. It's quite trivial and gives up easily.
func getPrefix(tree *RegexTree) Prefix {
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

		case ntOneloop, ntOnelazy:
			if curNode.m > 0 {
				return Prefix{
					Prefix:          strings.Repeat(string(curNode.ch), curNode.m),
					CaseInsensitive: (curNode.options & IgnoreCase) != 0,
				}
			}
			return Prefix{}

		case ntOne:
			return Prefix{
				Prefix:          string(curNode.ch),
				CaseInsensitive: (curNode.options & IgnoreCase) != 0,
			}

		case ntMulti:
			return Prefix{
				Prefix:          curNode.str,
				CaseInsensitive: (curNode.options & IgnoreCase) != 0,
			}

		case ntBol, ntEol, ntBoundary, ntECMABoundary, ntBeginning, ntStart,
			ntEndZ, ntEnd, ntEmpty, ntRequire, ntPrevent:

		default:
			return Prefix{}
		}

		if concatNode == nil || nextChild >= len(concatNode.children) {
			return Prefix{}
		}

		curNode = concatNode.children[nextChild]
		nextChild++
	}
}

// bmPrefix precomputes the Boyer-Moore
// tables for fast string scanning. These tables allow
// you to scan for the first occurrence of a string within
// a large body of text without examining every character.
// The performance of the heuristic depends on the actual
// string and the text being searched, but usually, the longer
// the string that is being searched for, the fewer characters
// need to be examined.
type BmPrefix struct {
}

func (b *BmPrefix) String() string {
	return ""
}

func (b *BmPrefix) Dump(indent string) string {
	return ""
}

func (b *BmPrefix) Scan(text []rune, pos int) int {
	panic("not implemented")
}

func (b *BmPrefix) IsMatch(text []rune, pos int) bool {
	panic("not implemented")
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
func anchorDescription(anchors AnchorLoc) string {
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
