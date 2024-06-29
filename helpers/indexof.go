package helpers

import (
	"bytes"
	"slices"
	"unicode"
	"unsafe"

	"github.com/dlclark/regexp2/syntax"
)

func IndexOfAny(in []rune, find []rune) int {
	// special case
	if len(find) == 0 {
		return -1
	}
	// naive version
	for i, c := range in {
		if slices.Contains(find, c) {
			return i
		}
	}
	return -1
}

func IndexOfAny1(in []rune, find rune) int {
	//TODO: bytes optimization?
	return slices.Index(in, find)
}

func IndexOfAny2(in []rune, find1, find2 rune) int {
	for i, c := range in {
		if c == find1 || c == find2 {
			return i
		}
	}

	return -1
}

func IndexOfAny3(in []rune, find1, find2, find3 rune) int {
	for i, c := range in {
		if c == find1 || c == find2 || c == find3 {
			return i
		}
	}

	return -1
}

func IndexOfAnyInRange(in []rune, first, last rune) int {
	for i, c := range in {
		if c >= first && c <= last {
			return i
		}
	}
	return -1
}

func IndexOfAnyExcept(in []rune, bad []rune) int {
	for i, c := range in {
		found := false
		for _, b := range bad {
			if b == c {
				found = true
				break
			}
		}
		if !found {
			return i
		}
	}
	return -1
}

func IndexOfAnyExcept1(in []rune, bad rune) int {
	for i, c := range in {
		if c != bad {
			return i
		}
	}
	return -1
}

func IndexOfAnyExcept2(in []rune, bad1, bad2 rune) int {
	for i, c := range in {
		if c != bad1 && c != bad2 {
			return i
		}
	}

	return -1
}

func IndexOfAnyExcept3(in []rune, bad1, bad2, bad3 rune) int {
	for i, c := range in {
		if c != bad1 && c != bad2 && c != bad3 {
			return i
		}
	}

	return -1
}

func IndexOfAnyExceptInRange(in []rune, first, last rune) int {
	for i, c := range in {
		if c > last {
			return i
		}
		if c < first {
			return i
		}
	}
	return -1
}

func IndexFunc(in []rune, f func(ch rune) bool) int {
	for i := range in {
		if f(in[i]) {
			return i
		}
	}
	return -1
}

func IndexOfAnyExceptInSet(in []rune, set syntax.CharSet) int {
	//TODO: this
	return -1
}

func LastIndexOf(in []rune, find []rune) int {
	end := len(in) - len(find)
	first := find[0]
	lastOffset := len(find) - 1
	last := find[lastOffset]
	for i := end; i >= 0; i-- {
		//TODO: check 2 chars needed?
		// match start and end...check the middle
		if in[i] == first && in[i+lastOffset] == last {
			// found our first char
			// check if the rest are equal
			if bytesEqual(in[i:i+len(find)], find) {
				return i
			}
		}
	}

	//not found
	return -1
}

func LastIndexOfAnyExcept1(in []rune, not rune) int {
	for i := len(in) - 1; i >= 0; i-- {
		if in[i] != not {
			return i
		}
	}
	return -1
}

func LastIndexOfAny1(in []rune, find rune) int {
	for i := len(in) - 1; i >= 0; i-- {
		if in[i] == find {
			// found our char
			return i
		}
	}

	//not found
	return -1
}

func LastIndexOfAnyInRange(in []rune, first, last rune) int {
	for i := len(in) - 1; i >= 0; i-- {
		if in[i] >= first && in[i] <= last {
			return i
		}
	}
	return -1
}

//TODO: LastIndexOf methods
//IndexOfAnyInRange
//LastIndexOfAnyInRange
//LastIndexOfAnyExceptInRange

func IndexOfIgnoreCase(in []rune, find []rune) int {
	if len(find) == 0 {
		return -1
	}
	// search the in slice for the "find" slice, ignoring case in the comparisons
	//TODO: this
	return 0
}

func IndexOf(in []rune, find []rune) int {
	/*
		Since we auto-gen the find code this shouldn't happen
		if len(find) == 0 {
			//special case
			return -1
		}*/
	end := len(in) - len(find)
	first := find[0]
	lastOffset := len(find) - 1
	last := find[lastOffset]
	for i := 0; i < end; i++ {
		//TODO: check 2 chars needed?
		// match start and end...check the middle
		if in[i] == first && in[i+lastOffset] == last {
			// found our first char
			// check if the rest are equal
			if bytesEqual(in[i:i+len(find)], find) {
				return i
			}
			/*if slices.Equal(in[i:i+len(find)], find) {
				return i
			}*/
		}
	}

	//not found
	return -1
}

func StartsWith(in []rune, find []rune) bool {
	// if text is less than our "begin" then can't find it
	if len(in) < len(find) {
		return false
	}

	return bytesEqual(in[:len(find)], find)

	/*for i := 0; i < len(find); i++ {
		if in[i] != find[i] {
			return false
		}
	}

	return true*/
}

//StartsWithIgnoreCaseAscii would be faster

// find should always be sent in lower-case
func StartsWithIgnoreCase(in []rune, find []rune) bool {
	// if text is less than our "begin" then can't find it
	if len(in) < len(find) {
		return false
	}

	for i := 0; i < len(find); i++ {
		if in[i] == find[i] {
			// if we match the char exactly then we're good
			continue
		}
		// if the to-lower still doesn't match then it's not a match
		if unicode.ToLower(in[i]) != find[i] {
			return false
		}
	}

	return true
}

// internal function, assumes the bounds are already set right on the slices for equality
// casts the rune slices to bytes to use framework fast []byte comparison
func bytesEqual(a, b []rune) bool {
	bytesA := unsafe.Slice((*byte)(unsafe.Pointer(&a[0])), len(a)*4)
	bytesB := unsafe.Slice((*byte)(unsafe.Pointer(&b[0])), len(b)*4)
	return bytes.Equal(bytesA, bytesB)
}

func Equals(in []rune, start int, length int, find []rune) bool {
	return bytesEqual(in[start:start+length], find)
}
