package syntax

import (
	"bytes"
	"fmt"
	"math"
	"strconv"
	"strings"
	"unicode"

	"slices"
)

// Arbitrary number of repetitions of the same character when we'd prefer to represent that as a repeater of that character rather than a string.
const MultiVsRepeaterLimit = 64

type RegexTree struct {
	Root              *RegexNode
	Caps              map[int]int
	Capnumlist        []int
	Captop            int
	Capnames          map[string]int
	Caplist           []string
	Options           RegexOptions
	FindOptimizations *FindOptimizations
}

// It is built into a parsed tree for a regular expression.

// Implementation notes:
//
// Since the node tree is a temporary data structure only used
// during compilation of the regexp to integer codes, it's
// designed for clarity and convenience rather than
// space efficiency.
//
// RegexNodes are built into a tree, linked by the n.children list.
// Each node also has a n.parent and n.ichild member indicating
// its parent and which child # it is in its parent's list.
//
// RegexNodes come in as many types as there are constructs in
// a regular expression, for example, "concatenate", "alternate",
// "one", "rept", "group". There are also node types for basic
// peephole optimizations, e.g., "onerep", "notsetrep", etc.
//
// Because perl 5 allows "lookback" groups that scan backwards,
// each node also gets a "direction". Normally the value of
// boolean n.backward = false.
//
// During parsing, top-level nodes are also stacked onto a parse
// stack (a stack of trees). For this purpose we have a n.next
// pointer. [Note that to save a few bytes, we could overload the
// n.parent pointer instead.]
//
// On the parse stack, each tree has a "role" - basically, the
// nonterminal in the grammar that the parser has currently
// assigned to the tree. That code is stored in n.role.
//
// Finally, some of the different kinds of nodes have data.
// Two integers (for the looping constructs) are stored in
// n.operands, an an object (either a string or a set)
// is stored in n.data
type RegexNode struct {
	T        NodeType
	Children []*RegexNode
	Str      []rune
	Set      *CharSet
	Ch       rune
	M        int
	N        int
	Options  RegexOptions
	Next     *RegexNode
}

type NodeType int32

const (
	// The following are leaves, and correspond to primitive operations
	NtUnknown NodeType = -1
	//NtOnerep      NodeType = 0  // lef,back char,min,max    a {n}
	//NtNotonerep   NodeType = 1  // lef,back char,min,max    .{n}
	//NtSetrep      NodeType = 2  // lef,back set,min,max     [\d]{n}
	NtOneloop     NodeType = 3  // lef,back char,min,max    a {,n}
	NtNotoneloop  NodeType = 4  // lef,back char,min,max    .{,n}
	NtSetloop     NodeType = 5  // lef,back set,min,max     [\d]{,n}
	NtOnelazy     NodeType = 6  // lef,back char,min,max    a {,n}?
	NtNotonelazy  NodeType = 7  // lef,back char,min,max    .{,n}?
	NtSetlazy     NodeType = 8  // lef,back set,min,max     [\d]{,n}?
	NtOne         NodeType = 9  // lef      char            a
	NtNotone      NodeType = 10 // lef      char            [^a]
	NtSet         NodeType = 11 // lef      set             [a-z\s]  \w \s \d
	NtMulti       NodeType = 12 // lef      string          abcd
	NtRef         NodeType = 13 // lef      group           \#
	NtBol         NodeType = 14 //                          ^
	NtEol         NodeType = 15 //                          $
	NtBoundary    NodeType = 16 //                          \b
	NtNonboundary NodeType = 17 //                          \B
	NtBeginning   NodeType = 18 //                          \A
	NtStart       NodeType = 19 //                          \G
	NtEndZ        NodeType = 20 //                          \Z
	NtEnd         NodeType = 21 //                          \Z

	// Interior nodes do not correspond to primitive operations, but
	// control structures compositing other operations

	// Concat and alternate take n children, and can run forward or backwards

	NtNothing     NodeType = 22 //          []
	NtEmpty       NodeType = 23 //          ()
	NtAlternate   NodeType = 24 //          a|b
	NtConcatenate NodeType = 25 //          ab
	NtLoop        NodeType = 26 // m,x      * + ? {,}
	NtLazyloop    NodeType = 27 // m,x      *? +? ?? {,}?
	NtCapture     NodeType = 28 // n        ()
	NtGroup       NodeType = 29 //          (?:)
	NtPosLook     NodeType = 30 //          (?=) (?<=)
	NtNegLook     NodeType = 31 //          (?!) (?<!)
	NtAtomic      NodeType = 32 //          (?>) (?<)
	NtBackRefCond NodeType = 33 //          (?(n) | )
	NtExprCond    NodeType = 34 //          (?(...) | )

	NtECMABoundary    NodeType = 41 //                          \b
	NtNonECMABoundary NodeType = 42 //                          \B

	NtUpdateBumpalong NodeType = 43
)

func newRegexNode(t NodeType, opt RegexOptions) *RegexNode {
	return &RegexNode{
		T:       t,
		Options: opt,
	}
}

func newRegexNodeCh(t NodeType, opt RegexOptions, ch rune) *RegexNode {
	return nodeWithCaseConversion(&RegexNode{
		T:       t,
		Options: opt,
		Ch:      ch,
	})
}

func newRegexNodeStr(t NodeType, opt RegexOptions, str []rune) *RegexNode {
	return &RegexNode{
		T:       t,
		Options: opt,
		Str:     str,
	}
}

func newRegexNodeSet(t NodeType, opt RegexOptions, set *CharSet) *RegexNode {
	return nodeWithCaseConversion(&RegexNode{
		T:       t,
		Options: opt,
		Set:     set,
	})
}

func newRegexNodeM(t NodeType, opt RegexOptions, m int) *RegexNode {
	return &RegexNode{
		T:       t,
		Options: opt,
		M:       m,
	}
}
func newRegexNodeMN(t NodeType, opt RegexOptions, m, n int) *RegexNode {
	return &RegexNode{
		T:       t,
		Options: opt,
		M:       m,
		N:       n,
	}
}

func nodeWithCaseConversion(n *RegexNode) *RegexNode {
	// if opts are ignore case and our rune is impacted by casing
	// then we need to switch our type to the set version
	// NtOne = NtSet
	if n.Options&IgnoreCase == 0 {
		return n
	}

	if n.Ch > 0 {
		ch := n.Ch
		if isLow, isUp := unicode.IsLower(ch), unicode.IsUpper(ch); isLow || isUp {
			var upper, lower rune
			// it's a capitalizable char
			if isUp {
				upper = ch
				lower = unicode.ToLower(ch)
			} else {
				lower = ch
				upper = unicode.ToUpper(ch)
			}
			set := &CharSet{}
			set.addChar(upper)
			set.addChar(lower)
			t := NtSet

			if n.T == NtOneloop || n.T == NtNotoneloop {
				t = NtSetloop
			} else if n.T == NtOnelazy || n.T == NtNotonelazy {
				t = NtSetlazy
			}
			set.negate = n.IsNotoneFamily()

			return &RegexNode{
				T:       t,
				Options: n.Options & ^IgnoreCase,
				Set:     set,
			}
		}
	} else if n.Set != nil {
		// just to be safe we don't modify the original set pointer
		// just in case it's used in a case-sensitive area
		s := n.Set.Copy()
		s.addCaseEquivalences()
		n.Set = &s
		n.Options &= ^IgnoreCase
	}

	return n
}

func (n *RegexNode) IsSetFamily() bool {
	return n.T == NtSet || n.T == NtSetloop || n.T == NtSetlazy /*|| n.T == NtSetloopatomic*/
}
func (n *RegexNode) IsOneFamily() bool {
	return n.T == NtOne || n.T == NtOneloop || n.T == NtOnelazy /*|| n.T == NtOneloopatomic*/
}
func (n *RegexNode) IsNotoneFamily() bool {
	return n.T == NtNotone || n.T == NtNotoneloop || n.T == NtNotonelazy /*|| n.T == NtNotoneloopatomic*/
}

func (n *RegexNode) writeStrToBuf(buf *bytes.Buffer) {
	for i := 0; i < len(n.Str); i++ {
		buf.WriteRune(n.Str[i])
	}
}

func (n *RegexNode) addChild(child *RegexNode) {
	reduced := child.reduce()
	n.Children = append(n.Children, reduced)
	reduced.Next = n
}

func (n *RegexNode) insertChildren(afterIndex int, nodes []*RegexNode) {
	newChildren := make([]*RegexNode, 0, len(n.Children)+len(nodes))
	n.Children = append(append(append(newChildren, n.Children[:afterIndex]...), nodes...), n.Children[afterIndex:]...)
}

// removes children including the start but not the end index
func (n *RegexNode) removeChildren(startIndex, endIndex int) {
	n.Children = append(n.Children[:startIndex], n.Children[endIndex:]...)
}

func (n *RegexNode) ReplaceChild(index int, newChild *RegexNode) {
	//newChild.Parent = n // so that the child can see its parent while being reduced
	newChild = newChild.reduce()
	//newChild.Parent = n // in case Reduce returns a different node that needs to be reparented

	n.Children[index] = newChild
}

// Pass type as OneLazy or OneLoop
func (n *RegexNode) makeRep(t NodeType, min, max int) {
	n.T += (t - NtOne)
	n.M = min
	n.N = max
}

func (n *RegexNode) reduce() *RegexNode {
	// Remove IgnoreCase option from everything except a Backreference
	if n.T != NtRef {
		n.Options &= ^IgnoreCase
	}
	switch n.T {
	case NtAlternate:
		return n.reduceAlternation()

	case NtConcatenate:
		return n.reduceConcatenation()

	case NtLoop, NtLazyloop:
		return n.reduceRep()

	case NtGroup:
		return n.reduceGroup()

	case NtSet, NtSetloop:
		return n.reduceSet()

	default:
		return n
	}
}

// Basic optimization. Single-letter alternations can be replaced
// by faster set specifications, and nested alternations with no
// intervening operators can be flattened:
//
// a|b|c|def|g|h -> [a-c]|def|[gh]
// apple|(?:orange|pear)|grape -> apple|orange|pear|grape
func (n *RegexNode) reduceAlternation() *RegexNode {
	if len(n.Children) == 0 {
		return newRegexNode(NtNothing, n.Options)
	}

	wasLastSet := false
	lastNodeCannotMerge := false
	var optionsLast RegexOptions
	var i, j int

	for i, j = 0, 0; i < len(n.Children); i, j = i+1, j+1 {
		at := n.Children[i]

		if j < i {
			n.Children[j] = at
		}

		for {
			if at.T == NtAlternate {
				for k := 0; k < len(at.Children); k++ {
					at.Children[k].Next = n
				}
				n.insertChildren(i+1, at.Children)

				j--
			} else if at.T == NtSet || at.T == NtOne {
				// Cannot merge sets if L or I options differ, or if either are negated.
				optionsAt := at.Options & (RightToLeft | IgnoreCase)

				if at.T == NtSet {
					if !wasLastSet || optionsLast != optionsAt || lastNodeCannotMerge || !at.Set.IsMergeable() {
						wasLastSet = true
						lastNodeCannotMerge = !at.Set.IsMergeable()
						optionsLast = optionsAt
						break
					}
				} else if !wasLastSet || optionsLast != optionsAt || lastNodeCannotMerge {
					wasLastSet = true
					lastNodeCannotMerge = false
					optionsLast = optionsAt
					break
				}

				// The last node was a Set or a One, we're a Set or One and our options are the same.
				// Merge the two nodes.
				j--
				prev := n.Children[j]

				var prevCharClass *CharSet
				if prev.T == NtOne {
					prevCharClass = &CharSet{}
					prevCharClass.addChar(prev.Ch)
				} else {
					prevCharClass = prev.Set
				}

				if at.T == NtOne {
					prevCharClass.addChar(at.Ch)
				} else {
					prevCharClass.addSet(*at.Set)
				}

				prev.T = NtSet
				prev.Set = prevCharClass
			} else if at.T == NtNothing {
				j--
			} else {
				wasLastSet = false
				lastNodeCannotMerge = false
			}
			break
		}
	}

	if j < i {
		n.removeChildren(j, i)
	}

	return n.stripEnation(NtNothing)
}

// Basic optimization. Adjacent strings can be concatenated.
//
// (?:abc)(?:def) -> abcdef
func (n *RegexNode) reduceConcatenation() *RegexNode {
	// Eliminate empties and concat adjacent strings/chars

	var optionsLast RegexOptions
	var optionsAt RegexOptions
	var i, j int

	if len(n.Children) == 0 {
		return newRegexNode(NtEmpty, n.Options)
	}

	wasLastString := false

	for i, j = 0, 0; i < len(n.Children); i, j = i+1, j+1 {
		var at, prev *RegexNode

		at = n.Children[i]

		if j < i {
			n.Children[j] = at
		}

		if at.T == NtConcatenate &&
			((at.Options & RightToLeft) == (n.Options & RightToLeft)) {
			for k := 0; k < len(at.Children); k++ {
				at.Children[k].Next = n
			}

			//insert at.children at i+1 index in n.children
			n.insertChildren(i+1, at.Children)

			j--
		} else if at.T == NtMulti || at.T == NtOne {
			// Cannot merge strings if L or I options differ
			optionsAt = at.Options & (RightToLeft | IgnoreCase)

			if !wasLastString || optionsLast != optionsAt {
				wasLastString = true
				optionsLast = optionsAt
				continue
			}

			j--
			prev = n.Children[j]

			if prev.T == NtOne {
				prev.T = NtMulti
				prev.Str = []rune{prev.Ch}
			}

			if (optionsAt & RightToLeft) == 0 {
				if at.T == NtOne {
					prev.Str = append(prev.Str, at.Ch)
				} else {
					prev.Str = append(prev.Str, at.Str...)
				}
			} else {
				if at.T == NtOne {
					// insert at the front by expanding our slice, copying the data over, and then setting the value
					prev.Str = append(prev.Str, 0)
					copy(prev.Str[1:], prev.Str)
					prev.Str[0] = at.Ch
				} else {
					//insert at the front...this one we'll make a new slice and copy both into it
					merge := make([]rune, len(prev.Str)+len(at.Str))
					copy(merge, at.Str)
					copy(merge[len(at.Str):], prev.Str)
					prev.Str = merge
				}
			}
		} else if at.T == NtEmpty {
			j--
		} else {
			wasLastString = false
		}
	}

	if j < i {
		// remove indices j through i from the children
		n.removeChildren(j, i)
	}

	return n.stripEnation(NtEmpty)
}

// Nested repeaters just get multiplied with each other if they're not
// too lumpy
func (n *RegexNode) reduceRep() *RegexNode {

	u := n
	t := n.T
	min := n.M
	max := n.N

	for {
		if len(u.Children) == 0 {
			break
		}

		child := u.Children[0]

		// multiply reps of the same type only
		if child.T != t {
			childType := child.T

			if !(childType >= NtOneloop && childType <= NtSetloop && t == NtLoop ||
				childType >= NtOnelazy && childType <= NtSetlazy && t == NtLazyloop) {
				break
			}
		}

		// child can be too lumpy to blur, e.g., (a {100,105}) {3} or (a {2,})?
		// [but things like (a {2,})+ are not too lumpy...]
		if u.M == 0 && child.M > 1 || child.N < child.M*2 {
			break
		}

		u = child
		if u.M > 0 {
			if (math.MaxInt32-1)/u.M < min {
				u.M = math.MaxInt32
			} else {
				u.M = u.M * min
			}
		}
		if u.N > 0 {
			if (math.MaxInt32-1)/u.N < max {
				u.N = math.MaxInt32
			} else {
				u.N = u.N * max
			}
		}
	}

	if math.MaxInt32 == min {
		return newRegexNode(NtNothing, n.Options)
	}
	return u

}

// Simple optimization. If a concatenation or alternation has only
// one child strip out the intermediate node. If it has zero children,
// turn it into an empty.
func (n *RegexNode) stripEnation(emptyType NodeType) *RegexNode {
	switch len(n.Children) {
	case 0:
		return newRegexNode(emptyType, n.Options)
	case 1:
		return n.Children[0]
	default:
		return n
	}
}

func (n *RegexNode) reduceGroup() *RegexNode {
	u := n

	for u.T == NtGroup {
		u = u.Children[0]
	}

	return u
}

// Simple optimization. If a set is a singleton, an inverse singleton,
// or empty, it's transformed accordingly.
func (n *RegexNode) reduceSet() *RegexNode {
	// Extract empty-set, one and not-one case as special

	if n.Set == nil {
		n.T = NtNothing
	} else if n.Set.IsSingleton() {
		n.Ch = n.Set.SingletonChar()
		n.Set = nil
		n.T += (NtOne - NtSet)
	} else if n.Set.IsSingletonInverse() {
		n.Ch = n.Set.SingletonChar()
		n.Set = nil
		n.T += (NtNotone - NtSet)
	}

	return n
}

func (n *RegexNode) reverseLeft() *RegexNode {
	if n.Options&RightToLeft != 0 && n.T == NtConcatenate && len(n.Children) > 0 {
		//reverse children order
		for left, right := 0, len(n.Children)-1; left < right; left, right = left+1, right-1 {
			n.Children[left], n.Children[right] = n.Children[right], n.Children[left]
		}
	}

	return n
}

func (n *RegexNode) makeQuantifier(lazy bool, min, max int) *RegexNode {
	if min == 0 && max == 0 {
		return newRegexNode(NtEmpty, n.Options)
	}

	if min == 1 && max == 1 {
		return n
	}

	switch n.T {
	case NtOne, NtNotone, NtSet:
		if lazy {
			n.makeRep(NtOnelazy, min, max)
		} else {
			n.makeRep(NtOneloop, min, max)
		}
		return n

	default:
		var t NodeType
		if lazy {
			t = NtLazyloop
		} else {
			t = NtLoop
		}
		result := newRegexNodeMN(t, n.Options, min, max)
		result.addChild(n)
		return result
	}
}

// Computes a min bound on the required length of any string that could possibly match.
// If the result is 0, there is no minimum we can enforce.
func (n *RegexNode) ComputeMinLength() int {
	switch n.T {
	case NtOne, NtNotone, NtSet:
		// single char
		return 1
	case NtMulti:
		// Every character in the string needs to match.
		return len(n.Str)
	case NtNotonelazy, NtNotoneloop, //NtNotoneloopatomic,
		NtOnelazy, NtOneloop /*NtOneloopatomic,*/, NtSetlazy, NtSetloop /*,NtSetloopatomics*/ :
		// One character repeated at least M times.
		return n.M
	case NtLazyloop, NtLoop:
		// A node graph repeated at least M times.
		return n.M * n.Children[0].ComputeMinLength()
	case NtAlternate:
		// The minimum required length for any of the alternation's branches.
		childCount := len(n.Children)
		min := n.Children[0].ComputeMinLength()
		for i := 1; i < childCount && min > 0; i++ {
			newMin := n.Children[i].ComputeMinLength()
			if newMin < min {
				min = newMin
			}
		}
		return min
	case NtBackRefCond:
		// Minimum of its yes and no branches.  The backreference doesn't add to the length.
		b1 := n.Children[0].ComputeMinLength()
		if len(n.Children) == 1 {
			return b1
		}
		b2 := n.Children[1].ComputeMinLength()
		if b1 < b2 {
			return b1
		}
		return b2
	case NtExprCond:
		// Minimum of its yes and no branches.  The condition is a zero-width assertion.
		if len(n.Children) == 2 {
			return n.Children[1].ComputeMinLength()
		}
		b1 := n.Children[1].ComputeMinLength()
		b2 := n.Children[2].ComputeMinLength()
		if b1 < b2 {
			return b1
		}
		return b2
	case NtConcatenate:
		// The sum of all of the concatenation's children.
		sum := 0
		for i := 0; i < len(n.Children); i++ {
			sum += n.Children[i].ComputeMinLength()
		}
		return sum
	case NtAtomic, NtCapture, NtGroup:
		// For groups, we just delegate to the sole child.
		return n.Children[0].ComputeMinLength()
	case NtEmpty, NtNothing,
		NtBeginning, NtBol, NtBoundary, NtECMABoundary, NtEnd, NtEndZ, NtEol,
		NtNonboundary, NtNonECMABoundary, NtStart, NtNegLook, NtPosLook, NtRef:
		// Nothing to match. In the future, we could potentially use Nothing to say that the min length
		// is infinite, but that would require a different structure, as that would only apply if the
		// Nothing match is required in all cases (rather than, say, as one branch of an alternation).
	}
	return 0
}

// Computes a maximum length of any string that could possibly match.
// or -1 if the length may not always be the same.
func (n *RegexNode) computeMaxLength() int {
	switch n.T {
	case NtOne, NtNotone, NtSet:
		return 1
	case NtMulti:
		return len(n.Str)
	case NtNotonelazy, NtNotoneloop, //NtNotoneloopatomic,
		NtOnelazy, NtOneloop /*NtOneloopatomic,*/, NtSetlazy, NtSetloop /*,NtSetloopatomics*/ :
		// Return the max number of iterations if there's an upper bound, or null if it's infinite
		if n.N == math.MaxInt32 {
			return -1
		}
		return n.N
	case NtLazyloop, NtLoop:
		if n.N == math.MaxInt32 {
			return -1
		}
		// A node graph repeated a fixed number of times
		if c := n.Children[0].computeMaxLength(); c >= 0 {
			maxLen := n.N * c
			if maxLen >= math.MaxInt32 {
				return -1
			}
			return maxLen
		}
	case NtAlternate:
		// The maximum length of any child branch, as long as they all have one.
		c := n.Children[0].computeMaxLength()

		if c < 0 {
			return -1
		}
		for i := 1; i < len(n.Children); i++ {
			c2 := n.Children[0].computeMaxLength()
			if c2 < 0 {
				return -1
			}
			if c2 > c {
				c = c2
			}
		}
		return c
	case NtBackRefCond:
		// The maximum length of either child branch, as long as they both have one.
		b1 := n.Children[0].computeMaxLength()
		if b1 < 0 {
			return -1
		}
		b2 := n.Children[1].computeMaxLength()
		if b2 < 0 {
			return b2
		}
		if b1 > b2 {
			return b1
		}
		return b2
	case NtExprCond:
		// The condition for an expression conditional is a zero-width assertion.
		b1 := n.Children[1].computeMaxLength()
		if b1 < 0 {
			return -1
		}
		b2 := n.Children[2].computeMaxLength()
		if b2 < 0 {
			return b2
		}
		if b1 > b2 {
			return b1
		}
		return b2
	case NtConcatenate:
		// The sum of all of the concatenation's children's max lengths, as long as they all have one.
		sum := 0
		for i := 0; i < len(n.Children); i++ {
			c := n.Children[i].computeMaxLength()
			if c < 0 {
				return -1
			}
			sum += c
		}
		return sum

	case NtAtomic, NtCapture:
		// For groups, we just delegate to the sole child.
		return n.Children[0].computeMaxLength()
	case NtEmpty, NtNothing,
		NtBeginning, NtBol, NtBoundary, NtECMABoundary, NtEnd, NtEndZ, NtEol,
		NtNonboundary, NtNonECMABoundary, NtStart, NtNegLook, NtPosLook:
		//zero-width
		return 0

	case NtRef:
		// Requires matching data available only at run-time.  In the future, we could choose to find
		// and follow the capture group this aligns with, while being careful not to end up in an
		// infinite cycle.
		return -1
	}

	return -1
}

// debug functions

var typeStr = []string{
	"Onerep", "Notonerep", "Setrep",
	"Oneloop", "Notoneloop", "Setloop",
	"Onelazy", "Notonelazy", "Setlazy",
	"One", "Notone", "Set",
	"Multi", "Ref",
	"Bol", "Eol", "Boundary", "Nonboundary",
	"Beginning", "Start", "EndZ", "End",
	"Nothing", "Empty",
	"Alternate", "Concatenate",
	"Loop", "Lazyloop",
	"Capture", "Group", "Require", "Prevent", "Greedy",
	"Testref", "Testgroup",
	"Unknown", "Unknown", "Unknown",
	"Unknown", "Unknown", "Unknown",
	"ECMABoundary", "NonECMABoundary",
	"Bumpalong",
}

func (n *RegexNode) Description() string {
	buf := &bytes.Buffer{}

	buf.WriteString(typeStr[n.T])

	if (n.Options & ExplicitCapture) != 0 {
		buf.WriteString("-C")
	}
	if (n.Options & IgnoreCase) != 0 {
		buf.WriteString("-I")
	}
	if (n.Options & RightToLeft) != 0 {
		buf.WriteString("-L")
	}
	if (n.Options & Multiline) != 0 {
		buf.WriteString("-M")
	}
	if (n.Options & Singleline) != 0 {
		buf.WriteString("-S")
	}
	if (n.Options & IgnorePatternWhitespace) != 0 {
		buf.WriteString("-X")
	}
	if (n.Options & ECMAScript) != 0 {
		buf.WriteString("-E")
	}

	switch n.T {
	case NtOneloop, NtNotoneloop, NtOnelazy, NtNotonelazy, NtOne, NtNotone:
		buf.WriteString("(Ch = " + CharDescription(n.Ch) + ")")
	case NtCapture:
		buf.WriteString("(index = " + strconv.Itoa(n.M) + ", unindex = " + strconv.Itoa(n.N) + ")")
	case NtRef, NtBackRefCond:
		buf.WriteString("(index = " + strconv.Itoa(n.M) + ")")
	case NtMulti:
		fmt.Fprintf(buf, "(String = %s)", string(n.Str))
	case NtSet, NtSetloop, NtSetlazy:
		buf.WriteString("(Set = " + n.Set.String() + ")")
	}

	switch n.T {
	case NtOneloop, NtNotoneloop, NtOnelazy, NtNotonelazy, NtSetloop, NtSetlazy, NtLoop, NtLazyloop:
		buf.WriteString("(Min = ")
		buf.WriteString(strconv.Itoa(n.M))
		buf.WriteString(", Max = ")
		if n.N == math.MaxInt32 {
			buf.WriteString("inf")
		} else {
			buf.WriteString(strconv.Itoa(n.N))
		}
		buf.WriteString(")")
	}

	return buf.String()
}

var padSpace = []byte("                                ")

func (t *RegexTree) Dump() string {
	return t.Root.dump()
}

func (n *RegexNode) dump() string {
	var stack []int
	CurNode := n
	CurChild := 0

	buf := bytes.NewBufferString(CurNode.Description())
	buf.WriteRune('\n')

	for {
		if CurNode.Children != nil && CurChild < len(CurNode.Children) {
			stack = append(stack, CurChild+1)
			CurNode = CurNode.Children[CurChild]
			CurChild = 0

			Depth := len(stack)
			if Depth > 32 {
				Depth = 32
			}
			buf.Write(padSpace[:Depth])
			buf.WriteString(CurNode.Description())
			buf.WriteRune('\n')
		} else {
			if len(stack) == 0 {
				break
			}

			CurChild = stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			CurNode = CurNode.Next
		}
	}
	return buf.String()
}

// Determines whether the specified child index of a concatenation begins a sequence whose values
// should be used to perform an ordinal case-insensitive comparison.
//
// When consumeZeroWidthNodes is false, the consumer needs the semantics of matching the produced string to fully represent
// the semantics of all the consumed nodes, which means nodes can be consumed iff they produce text that's represented
// by the resulting string. When true, the resulting string needs to fully represent all valid matches at that position,
// but it can have false positives, which means the resulting string doesn't need to fully represent all zero-width nodes
// consumed. true is only valid when used as part of a search to determine where to try a full match, not as part of
// actual matching logic.
// consumeZeroWidthNodes = false
func (n *RegexNode) TryGetOrdinalCaseInsensitiveString(childIndex int, exclusiveChildBound int, consumeZeroWidthNodes bool) (success bool, nodesConsumed int, caseInsensitiveString string) {
	vsb := &strings.Builder{}

	// We're looking in particular for sets of ASCII characters, so we focus only on sets with two characters in them, e.g. [Aa].
	twoChars := make([]rune, 0, 2)

	// Iterate from the child index to the exclusive upper bound.
	var i int
	for i = childIndex; i < exclusiveChildBound; i++ {
		child := n.Children[i]

		if child.T == NtOne {
			// We only want to include ASCII characters, and only if they don't participate in case conversion
			// such that they only case to themselves and nothing other cases to them.  Otherwise, including
			// them would potentially cause us to match against things not allowed by the pattern.
			if child.Ch >= unicode.MaxASCII || participatesInCaseConversion(child.Ch) {
				break
			}

			vsb.WriteRune(child.Ch)
		} else if child.T == NtMulti {
			// As with NtOne, the string needs to be composed solely of ASCII characters that
			// don't participate in case conversion.
			hasNonAscii := slices.ContainsFunc(child.Str, func(ch rune) bool { return ch > unicode.MaxASCII })
			if hasNonAscii || anyParticipatesInCaseConversion(string(child.Str)) {
				break
			}

			vsb.WriteString(string(child.Str))
		} else if child.T == NtSet ||
			((child.T == NtSetloop || child.T == NtSetlazy /*|| child.T == NtSetloopatomic*/) && child.M == child.N) {
			// In particular we want to look for sets that contain only the upper and lowercase variant
			// of the same ASCII letter.
			ok, twoChars := child.Set.containsAsciiIgnoreCaseCharacter(twoChars)
			if !ok {
				break
			}

			count := child.M
			if child.T == NtSet {
				count = 1
			}
			vsb.WriteString(strings.Repeat(string(twoChars[0]|0x20), count))
		} else if child.T == NtEmpty {
			// Skip over empty nodes, as they're pure nops. They would ideally have been optimized away,
			// but can still remain in some situations.
		} else if consumeZeroWidthNodes &&
			// anchors
			(child.T == NtBeginning || child.T == NtBol || child.T == NtStart ||
				// boundaries
				child.T == NtBoundary || child.T == NtECMABoundary || child.T == NtNonboundary || child.T == NtNonECMABoundary ||
				// lookarounds
				child.T == NtNegLook || child.T == NtPosLook ||
				// logic
				child.T == NtUpdateBumpalong) {
			// Skip over zero-width nodes that might be reasonable at the beginning of or within a substring.
			// We can only do these if consumeZeroWidthNodes is true, as otherwise we'd be producing a string that
			// may not fully represent the semantics of this portion of the pattern.
		} else {
			break
		}
	}

	// If we found at least two characters, consider it a sequence found.  It's possible
	// they all came from the same node, so this could be a sequence of just one node.
	if vsb.Len() >= 2 {
		return true, i - childIndex, vsb.String()
	}

	// No sequence found.
	return false, 0, ""
}

func (child *RegexNode) canJoinLengthCheck() bool {
	return child.T == NtOne || child.T == NtNotone || child.T == NtSet ||
		child.T == NtMulti ||
		(child.M == child.N &&
			(child.T == NtOneloop || child.T == NtOnelazy ||
				child.T == NtNotoneloop || child.T == NtNotonelazy ||
				child.T == NtSetloop || child.T == NtSetlazy))
}

// Determine whether the specified child node is the beginning of a sequence that can
// trivially have length checks combined in order to avoid bounds checks.
// requiredLength is The sum of all the fixed lengths for the nodes in the sequence.</param>
// exclusiveEnd is The index of the node just after the last one in the sequence.</param>
// returns true if more than one node can have their length checks combined; otherwise, false.</returns>
//
// There are additional node types for which we can prove a fixed length, e.g. examining all branches
// of an alternation and returning true if all their lengths are equal.  However, the primary purpose
// of this method is to avoid bounds checks by consolidating length checks that guard accesses to
// strings/spans for which the JIT can see a fixed index within bounds, and alternations employ
// patterns that defeat that (e.g. reassigning the span in question).  As such, the implementation
// remains focused on only a core subset of nodes that are a) likely to be used in concatenations and
// b) employ simple patterns of checks.
func (n *RegexNode) TryGetJoinableLengthCheckChildRange(childIndex int, requiredLength *int, exclusiveEnd *int) bool {

	child := n.Children[childIndex]
	if child.canJoinLengthCheck() {
		*requiredLength = child.ComputeMinLength()

		for *exclusiveEnd = childIndex + 1; *exclusiveEnd < len(n.Children); *exclusiveEnd++ {
			child = n.Children[*exclusiveEnd]
			if !child.canJoinLengthCheck() {
				break
			}

			*requiredLength += child.ComputeMinLength()
		}

		if *exclusiveEnd-childIndex > 1 {
			return true
		}
	}

	*requiredLength = 0
	*exclusiveEnd = 0
	return false
}

// Finds the guaranteed beginning literal(s) of the node, or null if none exists.
// allowZeroWidth = true
func (n *RegexNode) FindStartingLiteralNode(allowZeroWidth bool) *RegexNode {
	node := n
	for {
		if node != nil && node.Options&RightToLeft == 0 {
			switch node.T {
			case NtOne, NtNotone, NtMulti, NtSet:
				return node

			case NtOneloop /*NtOneloopatomic,*/, NtOnelazy,
				NtNotoneloop /*NtNotoneloopatomic,*/, NtNotonelazy,
				NtSetloop /*NtSetloopatomic,*/, NtSetlazy:
				if node.M > 0 {
					return node
				}

			case NtAtomic, NtConcatenate, NtCapture, NtGroup:
				node = node.Children[0]
				continue
			case NtLoop, NtLazyloop:
				node = node.Children[0]
				continue
			case NtPosLook:
				if allowZeroWidth {
					node = node.Children[0]
					continue
				}
			}
		}

		return nil
	}
}

// Gets the character that begins a One or Multi.
func (n *RegexNode) FirstCharOfOneOrMulti() rune {
	if n.IsOneFamily() {
		return n.Ch
	}
	return n.Str[0]
}
