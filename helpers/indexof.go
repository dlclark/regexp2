package helpers

import "slices"

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

//TODO: LastIndexOf methods
//IndexOfAnyInRange
//IndexOfAnyExceptInRange
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
	if len(find) == 0 {
		//special case
		return -1
	}
	end := len(in) - len(find)
	first := find[0]
	for i := 0; i < end; i++ {
		if in[i] == first {
			// found our first char
			// check if the rest are equal
			if slices.Equal(in[i:i+len(find)], find) {
				return i
			}
		}
	}

	//not found
	return -1
}

func StartsWith(in []rune, find []rune) bool {
	//TODO: this
	if len(find) == 0 {
		return true
	}

	// if text is less than our "begin" then can't find it
	if len(in) < len(find) {
		return false
	}

	for i := 0; i < len(find); i++ {
		if in[i] != find[i] {
			return false
		}
	}

	return true
}

func StartsWithIgnoreCase(in []rune, find []rune) bool {
	//TODO: this
	if len(find) == 0 {
		return true
	}
	return false
}
