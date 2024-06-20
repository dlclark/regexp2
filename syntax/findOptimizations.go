package syntax

type FindOptimizations struct {
	rightToLeft  bool
	asciiLookups [][]uint

	FindMode          FindNextStartingPositionMode
	LeadingAnchor     NodeType
	TrailingAnchor    NodeType
	MinRequiredLength int
	MaxPossibleLength int
	LeadingPrefix     string
	LeadingPrefixes   []string
}

type FindNextStartingPositionMode int

const (
	NoSearch FindNextStartingPositionMode = iota
	// A "beginning" anchor at the beginning of the pattern.
	LeadingAnchor_LeftToRight_Beginning
	// A "start" anchor at the beginning of the pattern.
	LeadingAnchor_LeftToRight_Start
	// An "endz" anchor at the beginning of the pattern.  This is rare.
	LeadingAnchor_LeftToRight_EndZ
	// An "end" anchor at the beginning of the pattern.  This is rare.
	LeadingAnchor_LeftToRight_End
	// A "beginning" anchor at the beginning of the right-to-left pattern.
	LeadingAnchor_RightToLeft_Beginning
	// A "start" anchor at the beginning of the right-to-left pattern.
	LeadingAnchor_RightToLeft_Start
	// An "endz" anchor at the beginning of the right-to-left pattern.  This is rare.
	LeadingAnchor_RightToLeft_EndZ
	// An "end" anchor at the beginning of the right-to-left pattern.  This is rare.
	LeadingAnchor_RightToLeft_End
	// An "end" anchor at the end of the pattern, with the pattern always matching a fixed-length expression.
	TrailingAnchor_FixedLength_LeftToRight_End
	// An "endz" anchor at the end of the pattern, with the pattern always matching a fixed-length expression.
	TrailingAnchor_FixedLength_LeftToRight_EndZ
	// A multi-character substring at the beginning of the pattern.
	LeadingString_LeftToRight
	// A multi-character substring at the beginning of the right-to-left pattern.
	LeadingString_RightToLeft
	// A multi-character ordinal case-insensitive substring at the beginning of the pattern.
	LeadingString_OrdinalIgnoreCase_LeftToRight
	// Multiple leading prefix strings
	LeadingStrings_LeftToRight
	// Multiple leading ordinal case-insensitive prefix strings
	LeadingStrings_OrdinalIgnoreCase_LeftToRight

	// A set starting the pattern.
	LeadingSet_LeftToRight
	// A set starting the right-to-left pattern.
	LeadingSet_RightToLeft

	// A single character at the start of the right-to-left pattern.
	LeadingChar_RightToLeft

	// A single character at a fixed distance from the start of the pattern.
	FixedDistanceChar_LeftToRight
	// A multi-character case-sensitive string at a fixed distance from the start of the pattern.
	FixedDistanceString_LeftToRight

	// One or more sets at a fixed distance from the start of the pattern.
	FixedDistanceSets_LeftToRight

	// A literal (single character, multi-char string, or set with small number of characters) after a non-overlapping set loop at the start of the pattern.
	LiteralAfterLoop_LeftToRight
)

func newFindOptimizations(tree *RegexTree, opt RegexOptions) *FindOptimizations {
	f := &FindOptimizations{
		rightToLeft:       opt&RightToLeft != 0,
		MinRequiredLength: tree.Root.computeMinLength(),
		LeadingAnchor:     findLeadingOrTrailingAnchor(tree.Root, true),
	}

	if f.rightToLeft && f.LeadingAnchor == NtBol {
		// Filter out Bol for RightToLeft, as we don't currently optimize for it.
		f.LeadingAnchor = NtUnknown
	}

	f.FindMode = getFindMode(f.rightToLeft, f.LeadingAnchor)
	if f.FindMode != NoSearch {
		return f
	}

	// Compute any anchor trailing the expression.  If there is one, and we can also compute a fixed length
	// for the whole expression, we can use that to quickly jump to the right location in the input.
	if !f.rightToLeft {
		f.TrailingAnchor = findLeadingOrTrailingAnchor(tree.Root, false)
		if f.TrailingAnchor == NtEnd || f.TrailingAnchor == NtEndZ {
			f.MaxPossibleLength = tree.Root.computeMaxLength()
			if f.MinRequiredLength == f.MaxPossibleLength {
				if f.TrailingAnchor == NtEnd {
					f.FindMode = TrailingAnchor_FixedLength_LeftToRight_End
				} else {
					f.FindMode = TrailingAnchor_FixedLength_LeftToRight_EndZ
				}
				return f
			}
		}
	}

	// If there's a leading substring, just use IndexOf and inherit all of its optimizations.
	//TODO: RegexPrefixAnalyzer.FindPrefix
	var prefix = getFirstCharsPrefix(tree)
	//var prefix = findPrefix(tree)
	if prefix != nil && len(prefix.PrefixStr) > 1 {
		f.LeadingPrefix = string(prefix.PrefixStr)
		if f.rightToLeft {
			f.FindMode = LeadingString_RightToLeft
		} else {
			f.FindMode = LeadingString_LeftToRight
		}
		return f
	}

	// At this point there are no fast-searchable anchors or case-sensitive prefixes. We can now analyze the
	// pattern for sets and then use any found sets to determine what kind of search to perform.

	//TODO: this
	// RegexFindOptimizations.cs: 89

	return f
}

func getFindMode(rtl bool, t NodeType) FindNextStartingPositionMode {
	if rtl {
		switch t {
		case NtBeginning:
			return LeadingAnchor_RightToLeft_Beginning
		case NtStart:
			return LeadingAnchor_RightToLeft_Start
		case NtEnd:
			return LeadingAnchor_RightToLeft_End
		case NtEndZ:
			return LeadingAnchor_RightToLeft_EndZ
		}
	} else {
		switch t {
		case NtBeginning:
			return LeadingAnchor_LeftToRight_Beginning
		case NtStart:
			return LeadingAnchor_LeftToRight_Start
		case NtEnd:
			return LeadingAnchor_LeftToRight_End
		case NtEndZ:
			return LeadingAnchor_LeftToRight_EndZ
		}
	}

	return NoSearch
}
