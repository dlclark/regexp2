package syntax

import "fmt"

const (
	//AnyClass represents any character class
	AnyClass = "\x00\x01\x00\x00"
)
const (
	flags          = 0
	setLength      = 1
	categoryLength = 2
	setStart       = 3
)

// setDescription gets a human-readable description for a set string
func setDescription(set string) string {
	//TODO: setDescription
	return "TODO: setDescription"
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

func SingletonChar(set string) rune {
	//      Debug.Assert(IsSingleton(set) || IsSingletonInverse(set), "Tried to get the singleton char out of a non singleton character class");
	panic("not implemented -- sets as strings in question")
	//return rune(set[setStart])
}

func IsSingleton(set string) bool {
	panic("not implemented -- sets as strings in question")
}

func CharInClass(char rune, set string) bool {
	panic("not implemented -- sets as strings in question")
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
	canonical bool
}

func newCharClass() charClass {
	return charClass{canonical: true}
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

func (c *charClass) addCharClass(cl charClass) {
	panic("not implemented")
}
