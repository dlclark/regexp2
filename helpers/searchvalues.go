package helpers

type RuneSearchValues struct {
}

func NewRuneSearchValues(vals []rune, ignoreCase bool) *RuneSearchValues {
	//TODO: this
	return &RuneSearchValues{}
}

// return the first index of our original vals values within the chars
// slice, starting at startPos
func (s *RuneSearchValues) IndexOfAny(chars []rune, startPos int) int {
	//TODO: this
	return -1
}

type StringSearchValues struct {
}

func NewStringSearchValues(vals []string, ignoreCase bool) *StringSearchValues {
	//TODO: this
	return &StringSearchValues{}
}

// return the first index of our original vals values within the chars
// slice, starting at startPos
func (s *StringSearchValues) IndexOfAny(chars []rune, startPos int) int {
	//TODO: this
	return -1
}
