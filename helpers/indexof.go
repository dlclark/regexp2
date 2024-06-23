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

//TODO: IndexOfAnyExcept methods

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
	return false
}
