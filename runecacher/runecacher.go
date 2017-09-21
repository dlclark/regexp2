package runecacher

import (
	"unicode/utf8"
)

const cachePrimeSize = 10

// RuneCacher allows the consumer to read runes from a string or byte slice
// and caches the results so that backtracking is cheap
type RuneCacher struct {
	runes  []rune
	inpStr string

	// start of uncached position in our input
	inpUncachedPos int
	// lengh of the input, maximum case scenario of the length of runes for optimizations
	inpLen int

	// length of our eventual runes set once the cached is filled
	runesLen int
}

// TODO: Right-to-left ?

func NewFromRunes(runes []rune) *RuneCacher {
	return &RuneCacher{
		runes:          runes,
		runesLen:       len(runes),
		inpLen:         len(runes),
		inpUncachedPos: len(runes) + 1,
	}
}

func NewFromString(str string) *RuneCacher {
	r := &RuneCacher{
		runes:    make([]rune, 0, len(str)),
		inpStr:   str,
		inpLen:   len(str),
		runesLen: len(str), // utf8.RuneCountInString(str),
	}
	// prime cache with some runes
	r.cachedNext(cachePrimeSize)
	return r
}

func (r *RuneCacher) Len() int {
	return r.runesLen
}

func (r *RuneCacher) String() string {
	if r.inpStr != "" {
		return r.inpStr
	}

	return string(r.runes)
}

// CachedRunesAt returns only the cached runes based on index and length
// Only safe to use if you're sure the runes are cached at those indices.
func (r *RuneCacher) CachedRunesAt(textPos, length int) []rune {
	return r.runes[textPos : textPos+length]
}

func (r *RuneCacher) CachedRunesFromTo(textPos, textEnd int) []rune {
	return r.runes[textPos:textEnd]
}

func (r *RuneCacher) CachedRunesFrom(textPos int) []rune {
	return r.runes[textPos:]
}

func (r *RuneCacher) CachedRunesTo(textEnd int) []rune {
	return r.runes[:textEnd]
}

func (r *RuneCacher) RuneAt(textPos int) rune {
	if textPos < len(r.runes) {
		return r.runes[textPos]
	}
	// not in our cache - populate cache
	want := textPos - len(r.runes) + 1
	r.cachedNext(want)

	return r.runes[textPos]
}

// RunesFrom returns all remaining runes from the input starting at a specific rune index
func (r *RuneCacher) RunesFrom(textPos int) []rune {
	if r.hasUncached() {
		// make sure our cache is fully populated
		r.cachedNext(r.inpLen)
	}
	return r.CachedRunesFrom(textPos)
}

func (r *RuneCacher) hasUncached() bool {
	// if we're not passed the end then we have more to cache
	return r.inpUncachedPos < r.inpLen
}

func (r *RuneCacher) cachedNext(count int) {
	// calculate our next runes and pre-populate the cache
	// stop if we've cached everything OR if we're
	for r.hasUncached() && count > 0 {
		// decode bytes
		newRune, newLen := utf8.DecodeRuneInString(r.inpStr[r.inpUncachedPos:])
		r.runes = append(r.runes, newRune)
		r.inpUncachedPos += newLen
		count--
	}
}
