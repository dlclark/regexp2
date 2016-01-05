package syntax

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"unicode"
)

// CharSet combines start-end rune ranges and unicode categories representing a set of characters
type CharSet struct {
	negate     bool
	ranges     []singleRange
	categories []category
	sub        *CharSet //optional subtractor
}

type category struct {
	negate bool
	cat    string
}

type singleRange struct {
	first rune
	last  rune
}

const (
	spaceCategoryText = " "
	wordCategoryText  = "W"
)

var (
	AnyClass          = getCharSetFromOldString("\x00", false) // &CharSet{ranges: []singleRange{singleRange{first: '\u0000', last: '\uFFFF'}}}
	ECMAWordClass     = getCharSetFromOldString("\u0030\u003A\u0041\u005B\u005F\u0060\u0061\u007B\u0130\u0131", false)
	NotECMAWordClass  = getCharSetFromOldString("\u0030\u003A\u0041\u005B\u005F\u0060\u0061\u007B\u0130\u0131", true)
	ECMASpaceClass    = getCharSetFromOldString("\u0009\u000E\u0020\u0021", false)
	NotECMASpaceClass = getCharSetFromOldString("\u0009\u000E\u0020\u0021", true)
	ECMADigitClass    = getCharSetFromOldString("\u0030\u003A", false)
	NotECMADigitClass = getCharSetFromOldString("\u0030\u003A", true)

	WordClass     = getCharSetFromCategoryString(false, "L", "Mn", "Nd", "Pc")
	NotWordClass  = getCharSetFromCategoryString(true, "L", "Mn", "Nd", "Pc")
	SpaceClass    = getCharSetFromCategoryString(false, spaceCategoryText)
	NotSpaceClass = getCharSetFromCategoryString(true, spaceCategoryText)
	DigitClass    = getCharSetFromCategoryString(false, "Nd")
	NotDigitClass = getCharSetFromCategoryString(true, "Nd")
)

func getCharSetFromCategoryString(negate bool, cats ...string) *CharSet {
	c := &CharSet{negate: negate}

	c.categories = make([]category, len(cats))
	for i, cat := range cats {
		c.categories[i] = category{cat: cat}
	}
	return c
}

func getCharSetFromOldString(setText string, negate bool) *CharSet {
	c := &CharSet{negate: negate}

	if len(setText)%2 == 0 {
		c.ranges = make([]singleRange, len(setText)/2)
	} else {
		c.ranges = make([]singleRange, len(setText)/2+1)
	}

	i := 0
	first := true
	for _, r := range setText {
		if first {
			// lower bound in a new range
			c.ranges[i] = singleRange{first: r}
			first = false
		} else {
			c.ranges[i].last = r - 1
			i++
			first = true
		}
	}
	if !first {
		c.ranges[i].last = '\uFFFF'
	}

	return c
}

// gets a human-readable description for a set string
func (set CharSet) String() string {
	buf := &bytes.Buffer{}
	buf.WriteRune('[')

	if set.IsNegated() {
		buf.WriteRune('^')
	}

	for _, r := range set.ranges {

		buf.WriteString(CharDescription(r.first))
		if r.first != r.last {
			buf.WriteRune('-')
			buf.WriteString(CharDescription(r.last))
		}
	}

	for _, c := range set.categories {
		buf.WriteString(c.String())
	}

	if set.sub != nil {
		buf.WriteRune('-')
		buf.WriteString(set.sub.String())
	}

	buf.WriteRune(']')

	return buf.String()
}

// mapHashFill converts a charset into a buffer for use in maps
func (s CharSet) mapHashFill(buf *bytes.Buffer) {
	if s.negate {
		buf.WriteByte(0)
	} else {
		buf.WriteByte(1)
	}

	binary.Write(buf, binary.LittleEndian, len(s.ranges))
	binary.Write(buf, binary.LittleEndian, len(s.categories))
	for _, r := range s.ranges {
		buf.WriteRune(r.first)
		buf.WriteRune(r.last)
	}
	for _, c := range s.categories {
		buf.WriteString(c.cat)
		if c.negate {
			buf.WriteByte(1)
		} else {
			buf.WriteByte(0)
		}
	}

	if s.sub != nil {
		s.sub.mapHashFill(buf)
	}
}

// CharIn returns true if the rune is in our character set (either ranges or categories).
// It handles negations and subtracted sub-charsets.
func (s CharSet) CharIn(ch rune) bool {
	val := false
	// in s && !s.subtracted

	//check ranges
	for _, r := range s.ranges {
		if ch < r.first {
			continue
		}
		if ch <= r.last {
			val = true
			break
		}
	}

	//check categories if we haven't already found a range
	if !val && len(s.categories) > 0 {
		for _, c := range s.categories {
			// special categories...then unicode
			if c.cat == spaceCategoryText {
				if unicode.IsSpace(ch) {
					// we found a space so we're done
					// negate means this is a "bad" thing
					val = !c.negate
					break
				}
			} else if unicode.Is(unicode.Categories[c.cat], ch) {
				// if we're in this unicode category then we're done
				// if negate=true on this category then we "failed" our test
				// otherwise we're good that we found it
				val = !c.negate
				break
			}
		}
	}

	// negate the whole char set
	if s.negate {
		val = !val
	}

	// get subtracted recurse
	if val && s.sub != nil {
		val = !s.sub.CharIn(ch)
	}

	return val
}

func (c category) String() string {
	switch c.cat {
	case spaceCategoryText:
		if c.negate {
			return "\\S"
		}
		return "\\s"
	case wordCategoryText:
		if c.negate {
			return "\\W"
		}
		return "\\w"
	}
	if _, ok := unicode.Categories[c.cat]; ok {

		if c.negate {
			return "\\P{" + c.cat + "}"
		}
		return "\\p{" + c.cat + "}"
	}
	return "Unknown category: " + c.cat
}

// CharDescription Produces a human-readable description for a single character.
func CharDescription(ch rune) string {
	if ch == '\\' {
		return "\\\\"
	}

	if ch >= ' ' && ch <= '~' {
		return string(ch)
	}

	return fmt.Sprintf("%U", ch)
}

// According to UTS#18 Unicode Regular Expressions (http://www.unicode.org/reports/tr18/)
// RL 1.4 Simple Word Boundaries  The class of <word_character> includes all Alphabetic
// values from the Unicode character database, from UnicodeData.txt [UData], plus the U+200C
// ZERO WIDTH NON-JOINER and U+200D ZERO WIDTH JOINER.
func IsWordChar(r rune) bool {
	//TODO: unicode IsWordChar
	return 'A' <= r && r <= 'Z' || 'a' <= r && r <= 'z' || '0' <= r && r <= '9' || r == '_'
}

func IsECMAWordChar(r rune) bool {
	//TODO: unicode IsWordChar
	return 'A' <= r && r <= 'Z' || 'a' <= r && r <= 'z' || '0' <= r && r <= '9' || r == '_'
}

// SingletonChar will return the char from the first range without validation.
// It assumes you have checked for IsSingleton or IsSingletonInverse and will panic given bad input
func (set CharSet) SingletonChar() rune {
	return set.ranges[0].first
}

func (set CharSet) IsSingleton() bool {
	return !set.negate && //negated is multiple chars
		len(set.categories) == 0 && len(set.ranges) == 1 && // multiple ranges and unicode classes represent multiple chars
		set.sub == nil && // subtraction means we've got multiple chars
		set.ranges[0].first == set.ranges[0].last // first and last equal means we're just 1 char
}

func (set CharSet) IsSingletonInverse() bool {
	return set.negate && //same as above, but requires negated
		len(set.categories) == 0 && len(set.ranges) == 1 && // multiple ranges and unicode classes represent multiple chars
		set.sub == nil && // subtraction means we've got multiple chars
		set.ranges[0].first == set.ranges[0].last // first and last equal means we're just 1 char
}

func (set CharSet) IsMergeable() bool {
	return !set.IsNegated() && !set.HasSubtraction()
}

func (set CharSet) IsNegated() bool {
	return set.negate
}

func (set CharSet) HasSubtraction() bool {
	return set.sub != nil
}

func (c *CharSet) addChar(ch rune) {
	panic("not implemented")
}

func (c *CharSet) addSet(set *CharSet) {
	panic("not implemented")
}

func (c *CharSet) addCategory(categoryName string, invert, caseInsensitive bool, pattern string) {

	if _, ok := unicode.Categories[categoryName]; ok {
		if caseInsensitive && (categoryName == "Ll" || categoryName == "Lu" || categoryName == "Lt") {
			// when RegexOptions.IgnoreCase is specified then {Ll} {Lu} and {Lt} cases should all match
			c.categories = append(c.categories,
				category{cat: "Ll", negate: invert},
				category{cat: "Lu", negate: invert},
				category{cat: "Lt", negate: invert})
		}

		c.categories = append(c.categories, category{cat: categoryName, negate: invert})
	} else {
		c.addSet(setFromProperty(categoryName, invert, pattern))
	}
}

// Adds to the class any lowercase versions of characters already
// in the class. Used for case-insensitivity.
func (c *CharSet) addLowercase() {
	for i := 0; i < len(c.ranges); i++ {
		r := c.ranges[i]
		if r.first == r.last {
			lower := unicode.ToLower(r.first)
			c.ranges[i] = singleRange{first: lower, last: lower}
		} else {
			c.addLowercaseRange(r.first, r.last)
		}
	}
}

func (c *CharSet) addLowercaseRange(chMin, chMax rune) {
	panic("not implemented")
}

func setFromProperty(capname string, invert bool, pattern string) *CharSet {
	panic("not impelemented")
}
