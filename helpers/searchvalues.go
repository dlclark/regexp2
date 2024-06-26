package helpers

import (
	"fmt"
	"unicode"
)

type AsciiSearchValues struct {
	// each ascii byte is represented by a bit in this array
	// there are 128bits here and ascii has 128 possible chars
	set [2]uint64
}

func NewAsciiSearchValues(vals string) AsciiSearchValues {
	// pre-calc ascii table stuff to make this go faster
	sv := AsciiSearchValues{}
	for i := 0; i < len(vals); i++ {
		c := vals[i]
		if c > unicode.MaxASCII {
			// a bug got us here. that's bad.
			panic(fmt.Errorf("non-ascii value found in ascii search values: %s", vals))
		}
		idx := c / 64
		shift := c % 64
		sv.set[idx] |= 1 << shift
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
		idx := c / 64
		shift := c % 64
		if s.set[idx]&(1<<shift) != 0 {
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
	vals       [][]rune
	ignoreCase bool
}

func NewStringSearchValues(vals [][]rune, ignoreCase bool) StringSearchValues {
	//TODO: this
	return StringSearchValues{vals, ignoreCase}
}

func (s StringSearchValues) StartsWith(chars []rune) int {
	//TODO: this
	return -1
}

func (s StringSearchValues) StartsWithIgnoreCase(chars []rune) int {
	//TODO: this
	return -1
}

func (s StringSearchValues) IndexOfAny(in []rune) int {
	//TODO: this
	return -1
}
