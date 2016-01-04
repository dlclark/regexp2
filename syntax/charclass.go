package syntax

import (
	"bytes"
	"fmt"
	"strings"
	"unicode"
)

const (
	SpaceClass    = "\u0000\u0000\u0001\u0064"
	NotSpaceClass = "\u0001\u0000\u0001\u0064"
	WordClass     = "\u0000\u0000\u000A\u0000\u0002\u0004\u0005\u0003\u0001\u0006\u0009\u0013\u0000"
	NotWordClass  = "\u0001\u0000\u000A\u0000\u0002\u0004\u0005\u0003\u0001\u0006\u0009\u0013\u0000"
	DigitClass    = "\u0000\u0000\u0001\u0009"
	NotDigitClass = "\u0000\u0000\u0001\uFFF7"

	ECMASpaceSet    = "\u0009\u000E\u0020\u0021"
	NotECMASpaceSet = "\x00\u0009\u000E\u0020\u0021"
	ECMAWordSet     = "\u0030\u003A\u0041\u005B\u005F\u0060\u0061\u007B\u0130\u0131"
	NotECMAWordSet  = "\x00\u0030\u003A\u0041\u005B\u005F\u0060\u0061\u007B\u0130\u0131"
	ECMADigitSet    = "\u0030\u003A"
	NotECMADigitSet = "\x00\u0030\u003A"

	ECMASpaceClass    = "\x00\x04\x00" + ECMASpaceSet
	NotECMASpaceClass = "\x01\x04\x00" + ECMASpaceSet
	ECMAWordClass     = "\x00\x0A\x00" + ECMAWordSet
	NotECMAWordClass  = "\x01\x0A\x00" + ECMAWordSet
	ECMADigitClass    = "\x00\x02\x00" + ECMADigitSet
	NotECMADigitClass = "\x01\x02\x00" + ECMADigitSet

	//AnyClass represents any character class
	AnyClass   = "\x00\x01\x00\x00"
	EmptyClass = "\x00\x00\x00"
)

const (
	nullChar = '\x00'
	//LastChar = '\uFFFF'
	lastChar      = '\xFF' // sets as strings or []rune?
	spaceConst    = 100
	notSpaceConst = -100

	groupChar = '\u0000'
)

const (
	internalRegexIgnoreCase = "__InternalRegexIgnoreCase__"
	space                   = "\x64"
	notSpace                = "\uFF9C"
	word                    = "\u0000\u0002\u0004\u0005\u0003\u0001\u0006\u0009\u0013\u0000"
	notWord                 = "\u0000\uFFFE\uFFFC\uFFFB\uFFFD\uFFFF\uFFFA\uFFF7\uFFED\u0000"
)

const (
	flags          = 0
	setLength      = 1
	categoryLength = 2
	setStart       = 3
)

// go doesn't have constant slices
var categories = []string{"Lu", "Ll", "Lt", "Lm", "Lo", internalRegexIgnoreCase,
	"Mn", "Mc", "Me",
	"Nd", "Nl", "No",
	"Zs", "Zl", "Zp",
	"Cc", "Cf", "Cs", "Co",
	"Pc", "Pd", "Ps", "Pe", "Pi", "Pf", "Po",
	"Sm", "Sc", "Sk", "So",
	"Cn"}

// go doesn't have constant maps ... so we roll with a var, don't change it
var definedCategories = map[string]string{
	// Others
	"Cc": "\u000F", // UnicodeCategory.Control + 1
	"Cf": "\u0010", // UnicodeCategory.Format + 1
	"Cn": "\u001E", // UnicodeCategory.OtherNotAssigned + 1
	"Co": "\u0012", // UnicodeCategory.PrivateUse + 1
	"Cs": "\u0011", // UnicodeCategory.Surrogate + 1
	"C":  "\u0000\u000F\u0010\u001E\u0012\u0011\u0000",

	// Letters
	"Ll": "\u0002", // UnicodeCategory.LowercaseLetter + 1
	"Lm": "\u0004", // UnicodeCategory.ModifierLetter + 1
	"Lo": "\u0005", // UnicodeCategory.OtherLetter + 1
	"Lt": "\u0003", // UnicodeCategory.TitlecaseLetter + 1
	"Lu": "\u0001", // UnicodeCategory.UppercaseLetter + 1
	"L":  "\u0000\u0002\u0004\u0005\u0003\u0001\u0000",

	// InternalRegexIgnoreCase = LowercaseLetter} OR TitlecaseLetter} OR UppercaseLetter}
	// !!!This category should only ever be used in conjunction with RegexOptions.IgnoreCase code paths!!!
	"__InternalRegexIgnoreCase__": "\u0000\u0002\u0003\u0001\u0000",

	// Marks
	"Mc": "\u0007", // UnicodeCategory.SpacingCombiningMark + 1
	"Me": "\u0008", // UnicodeCategory.EnclosingMark + 1
	"Mn": "\u0006", // UnicodeCategory.NonSpacingMark + 1
	"M":  "\u0000\u0007\u0008\u0006\u0000",

	// Numbers
	"Nd": "\u0009", // UnicodeCategory.DecimalDigitNumber + 1
	"Nl": "\u000A", // UnicodeCategory.LetterNumber + 1
	"No": "\u000B", // UnicodeCategory.OtherNumber + 1
	"N":  "\u0000\u0009\u000A\u000B\u0000",

	// Punctuation
	"Pc": "\u0013", // UnicodeCategory.ConnectorPunctuation + 1
	"Pd": "\u0014", // UnicodeCategory.DashPunctuation + 1
	"Pe": "\u0016", // UnicodeCategory.ClosePunctuation + 1
	"Po": "\u0019", // UnicodeCategory.OtherPunctuation + 1
	"Ps": "\u0015", // UnicodeCategory.OpenPunctuation + 1
	"Pf": "\u0018", // UnicodeCategory.FinalQuotePunctuation + 1
	"Pi": "\u0017", // UnicodeCategory.InitialQuotePunctuation + 1
	"P":  "\u0000\u0013\u0014\u0016\u0019\u0015\u0018\u0017\u0000",

	// Symbols
	"Sc": "\u001B", // UnicodeCategory.CurrencySymbol + 1
	"Sk": "\u001C", // UnicodeCategory.ModifierSymbol + 1
	"Sm": "\u001A", // UnicodeCategory.MathSymbol + 1
	"So": "\u001D", // UnicodeCategory.OtherSymbol + 1
	"S":  "\u0000\u001B\u001C\u001A\u001D\u0000",

	// Separators
	"Zl": "\u000D", // UnicodeCategory.LineSeparator + 1
	"Zp": "\u000E", // UnicodeCategory.ParagraphSeparator + 1
	"Zs": "\u000C", // UnicodeCategory.SpaceSeparator + 1
	"Z":  "\u0000\u000D\u000E\u000C\u0000",
}

// setDescription gets a human-readable description for a set string
func setDescription(set string) string {
	mySetLength := int(set[setLength])
	myCategoryLength := int(set[categoryLength])
	myEndPosition := setStart + mySetLength + myCategoryLength

	buf := &bytes.Buffer{}
	buf.WriteRune('[')

	index := setStart

	if IsNegated(set) {
		buf.WriteRune('^')
	}

	for index < setStart+int(set[setLength]) {
		ch1 := rune(set[index])
		var ch2 rune
		if index+1 < len(set) {
			ch2 = rune(set[index+1] - 1)
		} else {
			ch2 = lastChar
		}

		buf.WriteString(CharDescription(ch1))

		if ch2 != ch1 {
			if ch1+1 != ch2 {
				buf.WriteRune('-')
			}
			buf.WriteString(CharDescription(ch2))
		}
		index += 2
	}

	for index < setStart+mySetLength+myCategoryLength {
		ch1 := rune(set[index])
		if ch1 == 0 {
			found := false

			lastindex := strings.IndexByte(set[index+1:], groupChar)
			group := set[index : lastindex+1]

			for k, v := range definedCategories {
				if group == v {
					if int16(set[index+1]) > 0 {
						buf.WriteString("\\p{")
					} else {
						buf.WriteString("\\P{")
					}

					buf.WriteString(k)
					buf.WriteRune('}')

					found = true
					break
				}
			}

			if !found {
				if group == word {
					buf.WriteString("\\w")
				} else if group == notWord {
					buf.WriteString("\\W")
				} else {
					buf.WriteString("Couldn't find a group to match '" + group + "'")
				}
			}

			index = lastindex
		} else {
			buf.WriteString(categoryDescription(ch1))
		}

		index++
	}

	if len(set) > myEndPosition {
		buf.WriteRune('-')
		buf.WriteString(setDescription(set[myEndPosition:]))
	}

	buf.WriteRune(']')

	return buf.String()
}

func categoryDescription(ch rune) string {
	if ch == spaceConst {
		return "\\s"
	} else if ch == notSpaceConst {
		return "\\S"
	} else if int16(ch) < 0 {
		return "\\P{" + categories[(-(int16(ch))-1)] + "}"
	} else {
		return "\\p{" + categories[(ch-1)] + "}"
	}
}

// Produces a human-readable description for a single character.
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

// this whole stack is ripe for re-writing with a real data structure -- the string as a struct thing is super hacky
func CharInClass(ch rune, set string) bool {
	return CharInClassRecursive(ch, set, 0)
}

func CharInClassRecursive(ch rune, set string, start int) bool {
	mySetLength := int(set[start+setLength])
	myCategoryLength := int(set[start+categoryLength])
	myEndPosition := start + setStart + mySetLength + myCategoryLength

	subtracted := false

	if len(set) > myEndPosition {
		subtracted = CharInClassRecursive(ch, set, myEndPosition)
	}

	b := charInClassInternal(ch, set, start, mySetLength, myCategoryLength)

	// Note that we apply the negation *before* performing the subtraction.  This is because
	// the negation only applies to the first char class, not the entire subtraction.
	if set[start+flags] == 1 {
		b = !b
	}

	return b && !subtracted
}

// Determines a character's membership in a character class (via the
// string representation of the class).
func charInClassInternal(ch rune, set string, start, mySetLength, myCategoryLength int) bool {
	var mid int
	min := start + setStart
	max := min + mySetLength

	for min != max {
		mid = (min + max) / 2
		if ch < rune(set[mid]) {
			max = mid
		} else {
			min = mid + 1
		}
	}

	// The starting position of the set within the character class determines
	// whether what an odd or even ending position means.  If the start is odd,
	// an *even* ending position means the character was in the set.  With recursive
	// subtractions in the mix, the starting position = start+setStart.  Since we know that
	// setStart is odd, we can simplify it out of the equation.  But if it changes we need to
	// reverse this check.
	//Debug.Assert((setStart & 0x1) == 1, "If setStart is not odd, the calculation below this will be reversed");
	if (min & 0x1) == (start & 0x1) {
		return true
	}
	if myCategoryLength == 0 {
		return false
	}
	return charInCategory(ch, set, start, mySetLength, myCategoryLength)
}

//TODO: this is a hack -- fix this whole string-as-struct thing
type UnicodeCategory int16

func getUnicodeCategory(r rune) UnicodeCategory {
	if unicode.IsUpper(r) {
		return 0
	} else if unicode.IsLower(r) {
		return 1
	} else if unicode.IsTitle(r) {
		return 2
		//etc
	}

	return 29
}

func charInCategory(ch rune, set string, start, mySetLength, myCategoryLength int) bool {
	//this code seems odd since it only allows each char to be in one category...
	chcategory := getUnicodeCategory(ch)

	i := start + setStart + mySetLength
	end := i + myCategoryLength
	for i < end {
		curcat := int16(set[i])

		if curcat == 0 {
			// zero is our marker for a group of categories - treated as a unit
			if charInCategoryGroup(ch, chcategory, set, &i) {
				return true
			}
		} else if curcat > 0 {
			// greater than zero is a positive case

			if curcat == spaceConst {
				if unicode.IsSpace(ch) {
					return true
				}
				i++
				continue
			}
			curcat--
			if chcategory == UnicodeCategory(curcat) {
				return true
			}
		} else {
			// less than zero is a negative case
			if curcat == notSpaceConst {
				if !unicode.IsSpace(ch) {
					return true
				}
				i++
				continue
			}

			//curcat = -curcat;
			//--curcat;
			curcat = -1 - curcat

			if chcategory != UnicodeCategory(curcat) {
				return true
			}
		}
		i++
	}
	return false
}

// This is used for categories which are composed of other categories - L, N, Z, W...
// These groups need special treatment when they are negated
func charInCategoryGroup(ch rune, chcategory UnicodeCategory, category string, i *int) bool {
	*i++

	curcat := int16(category[*i])
	if curcat > 0 {
		// positive case - the character must be in ANY of the categories in the group
		answer := false

		for curcat != 0 {
			if !answer {
				curcat--
				if chcategory == UnicodeCategory(curcat) {
					answer = true
				}
			}
			*i++
			curcat = int16(category[*i])
		}
		return answer
	}

	// negative case - the character must be in NONE of the categories in the group
	answer := true

	for curcat != 0 {
		if answer {
			//curcat = -curcat;
			//--curcat;
			curcat = -1 - curcat
			if chcategory == UnicodeCategory(curcat) {
				answer = false
			}
		}
		*i++
		curcat = int16(category[*i])
	}
	return answer
}

func SingletonChar(set string) rune {
	//      Debug.Assert(IsSingleton(set) || IsSingletonInverse(set), "Tried to get the singleton char out of a non singleton character class");
	return rune(set[setStart])
}

func IsSingleton(set string) bool {
	if set[flags] == 0 && set[categoryLength] == 0 && set[setLength] == 2 && !IsSubtraction(set) &&
		(set[setStart] == lastChar || set[setStart]+1 == set[setStart+1]) {
		return true
	}
	return false
}

func IsSingletonInverse(set string) bool {
	if set[flags] == 1 && set[categoryLength] == 0 && set[setLength] == 2 && !IsSubtraction(set) &&
		(set[setStart] == lastChar || set[setStart]+1 == set[setStart+1]) {
		return true
	}
	return false
}

func IsEmpty(charClass string) bool {
	if charClass[categoryLength] == 0 && charClass[flags] == 0 && charClass[setLength] == 0 && !IsSubtraction(charClass) {
		return true
	} else {
		return false
	}
}

func IsMergeable(charClass string) bool {
	return (!IsNegated(charClass) && !IsSubtraction(charClass))
}

func IsNegated(set string) bool {
	return (set != "" && set[flags] == 1)
}

func IsSubtraction(charClass string) bool {
	//TODO: set as strings?
	return len(charClass) > setStart+int(charClass[setLength])+int(charClass[categoryLength])
}

type charClass struct {
	rangeList  []singleRange
	categories *bytes.Buffer
	canonical  bool
	negate     bool
	subtractor *charClass
}
type singleRange struct {
	first rune
	last  rune
}

func newCharClass() charClass {
	return charClass{
		canonical:  true,
		categories: &bytes.Buffer{},
		rangeList:  make([]singleRange, 0, 6),
	}
}
func parseCharClass(charClass string) charClass {
	panic("not implemented")
	//return ParseRecursive(charClass, 0)
}

// Constructs the string representation of the class.
func (c *charClass) toStringClass() string {
	//TODO: this
	panic("not implemented")
}

func (c *charClass) addChar(ch rune) {
	panic("not implemented")
}

func (c *charClass) addSet(set string) {
	panic("not implemented")
}

func (c *charClass) addCharClass(cl charClass) {
	panic("not implemented")
}

func (c *charClass) addCategoryFromName(categoryName string, invert, caseInsensitive bool, pattern string) {

	if category, ok := definedCategories[categoryName]; ok && categoryName != internalRegexIgnoreCase {
		if caseInsensitive {
			if categoryName == "Ll" || categoryName == "Lu" || categoryName == "Lt" {
				// when RegexOptions.IgnoreCase is specified then {Ll, {Lu, and {Lt} cases should all match
				category = definedCategories[internalRegexIgnoreCase]
			}
		}

		if invert {
			category = negateCategory(category) // negate the category
		}

		c.categories.WriteString(category)
	} else {
		c.addSet(setFromProperty(categoryName, invert, pattern))
	}
}

func (c *charClass) addCategory(category string) {
	c.categories.WriteString(category)
}

// Adds to the class any lowercase versions of characters already
// in the class. Used for case-insensitivity.
func (c *charClass) addLowercase() {
	c.canonical = false

	for i := 0; i < len(c.rangeList); i++ {
		r := c.rangeList[i]
		if r.first == r.last {
			lower := unicode.ToLower(r.first)
			c.rangeList[i] = singleRange{first: lower, last: lower}
		} else {
			c.addLowercaseRange(r.first, r.last)
		}
	}
}

func (c *charClass) addLowercaseRange(chMin, chMax rune) {
	panic("not implemented")
}

func negateCategory(category string) string {
	panic("not implemented")
}

func setFromProperty(capname string, invert bool, pattern string) string {
	panic("not impelemented")
}
