package helpers

import (
	"unicode"
)

type AsciiSearchValues struct {
	// 7 bytes to cover all 128 values for ascii
	set [7]uint32
}

func NewAsciiSearchValues(vals string) AsciiSearchValues {
	// pre-calc ascii table stuff to make this go faster
	sv := AsciiSearchValues{}
	for i := 0; i < len(vals); i++ {
		c := vals[i]
		if c > unicode.MaxASCII {
			// a bug got us here. that's bad
			continue
		}
		idx := c / 32
		rem := c % 32
		sv.set[idx] |= 1 << rem
	}

	return sv
}

// return the first index of our original vals values within the slice given
func (s AsciiSearchValues) IndexOfAny(chars []rune) int {
	for i := 0; i < len(chars); i++ {
		c := chars[i]
		if c > unicode.MaxASCII {
			continue
		}
		if s.set[c/32]&(1<<(c%32)) != 0 {
			return i
		}
	}
	return -1
}

// return the first index of our original vals values within the slice given
func (s AsciiSearchValues) IndexOfAnyExcept(chars []rune) int {
	//TODO: this
	return -1
}

// return the last index of our original vals values within the slice given
func (s AsciiSearchValues) LastIndexOfAny(chars []rune) int {
	//TODO: this
	return -1
}

// return the last index of our original vals values within the slice given
func (s AsciiSearchValues) LastIndexOfAnyExcept(chars []rune) int {
	//TODO: this
	return -1
}

type RuneSearchValues struct {
	vals []rune
}

func NewRuneSearchValues(vals string) RuneSearchValues {
	//TODO: pre-calc the stuff we need to make each IndexOf go faster
	return RuneSearchValues{vals: []rune(vals)}
}

// return the first index of our original vals values within the slice given
func (s RuneSearchValues) IndexOfAny(chars []rune) int {
	//naive implementation
	//TODO: this
	return IndexOfAny(chars, s.vals)
}

// return the first index of our original vals values within the slice given
func (s RuneSearchValues) IndexOfAnyExcept(chars []rune) int {
	//TODO: this
	return IndexOfAnyExcept(chars, s.vals)
}

// return the last index of our original vals values within the slice given
func (s RuneSearchValues) LastIndexOfAny(chars []rune) int {
	//TODO: this
	return -1
}

// return the last index of our original vals values within the slice given
func (s RuneSearchValues) LastIndexOfAnyExcept(chars []rune) int {
	//TODO: this
	return -1
}

type StringSearchValues struct {
}

func NewStringSearchValues(vals []string, ignoreCase bool) StringSearchValues {
	//TODO: this
	return StringSearchValues{}
}

// return the first index of our original vals values within the chars
// slice, starting at startPos
func (s StringSearchValues) IndexOfAny(chars []rune, startPos int) int {
	//TODO: this
	return -1
}
