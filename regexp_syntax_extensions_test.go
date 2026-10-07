package regexp2

import (
	"testing"

	"github.com/dlclark/regexp2/v2/syntax"
)

func requireFullMatch(t *testing.T, pattern, input string, options ...CompileOption) *Match {
	t.Helper()
	re := MustCompile(`\A(?:`+pattern+`)\z`, options...)
	m, err := re.FindStringMatch(input)
	if err != nil {
		t.Fatalf("matching %q against %q: %v", pattern, input, err)
	}
	if m == nil {
		t.Fatalf("%q did not match %q", pattern, input)
	}
	return m
}

func requireNoFullMatch(t *testing.T, pattern, input string, options ...CompileOption) {
	t.Helper()
	re := MustCompile(`\A(?:`+pattern+`)\z`, options...)
	m, err := re.FindStringMatch(input)
	if err != nil {
		t.Fatalf("matching %q against %q: %v", pattern, input, err)
	}
	if m != nil {
		t.Fatalf("%q unexpectedly matched %q", pattern, input)
	}
}

func TestPossessiveQuantifiers(t *testing.T) {
	for _, options := range [][]CompileOption{nil, {RE2}} {
		requireNoFullMatch(t, `a++a`, "aa", options...)
		requireFullMatch(t, `a+a`, "aa", options...)
		requireNoFullMatch(t, `a*+a`, "a", options...)
		requireNoFullMatch(t, `a?+a`, "a", options...)
		requireNoFullMatch(t, `a{1,2}+a`, "aa", options...)
		requireNoFullMatch(t, `(ab|a)++b`, "ab", options...)
	}

	for _, pattern := range []string{`a++`, `a*+`, `a?+`, `a{1,2}+`} {
		if _, err := Compile(pattern, ECMAScript); err == nil {
			t.Errorf("ECMAScript Compile(%q) succeeded; want an invalid nested quantifier", pattern)
		}
	}
}

func TestLiteralQuoting(t *testing.T) {
	for _, options := range [][]CompileOption{nil, {RE2}} {
		requireFullMatch(t, `\Qfoo.bar[0]+\E`, `foo.bar[0]+`, options...)
		re := MustCompile(`\A\Qfoo.bar`, options...)
		m, err := re.FindStringMatch(`foo.bar`)
		if err != nil || m == nil || m.String() != `foo.bar` {
			t.Fatalf("unterminated literal quote match = (%v, %v); want foo.bar", m, err)
		}
		requireFullMatch(t, `\Qab\E+`, `abbb`, options...)
		requireFullMatch(t, `a\Q\E+`, `aaaa`, options...)
		requireFullMatch(t, `(?x)\Q a # b \E`, ` a # b `, options...)
		requireFullMatch(t, `[\Qa-z]\E]+`, `]-az`, options...)
		requireFullMatch(t, `[a-\Qz\E]+`, `azm`, options...)
	}

	// ECMAScript retains its existing identity-escape behavior: Q and E are
	// literals, while the text between them is still regular expression syntax.
	requireFullMatch(t, `\Q.\E`, `QxE`, ECMAScript)
	requireNoFullMatch(t, `\Q.\E`, `.`, ECMAScript)
}

func TestUnicodeNewlineEscape(t *testing.T) {
	for _, input := range []string{"\r\n", "\n", "\v", "\f", "\r", "\u0085", "\u2028", "\u2029"} {
		m := requireFullMatch(t, `\R`, input)
		if got, want := m.RuneLength, len([]rune(input)); got != want {
			t.Errorf("\\R matched %q with rune length %d; want %d", input, got, want)
		}
	}
	requireNoFullMatch(t, `\R`, "x")
	requireFullMatch(t, `\R{2}`, "\r\n\n")
	requireFullMatch(t, `\R`, "R", RE2)
	requireFullMatch(t, `\R`, "R", ECMAScript)

	re := MustCompile(`\R`, RightToLeft)
	m, err := re.FindStringMatch("x\r\ny")
	if err != nil {
		t.Fatal(err)
	}
	if m == nil || m.String() != "\r\n" {
		t.Fatalf("right-to-left \\R match = %v; want CRLF", m)
	}
}

func TestExtendedGraphemeClusterEscape(t *testing.T) {
	tests := []struct {
		name  string
		input string
	}{
		{"combining mark", "a\u0301"},
		{"CRLF", "\r\n"},
		{"Hangul Jamo", "\u1100\u1161\u11A8"},
		{"regional indicators", "\U0001F1FA\U0001F1F8"},
		{"emoji ZWJ sequence", "👩‍👩‍👧‍👦"},
		{"prepend", "\u0600a"},
		{"spacing mark", "क\u093E"},
		{"Indic conjunct", "क्ष"},
		{"control", "\u0001"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			requireFullMatch(t, `\X`, tt.input)
		})
	}

	requireFullMatch(t, `\X{3}`, "a\u0301b\r\n")
	requireNoFullMatch(t, `\X\p{GCB=Extend}`, "a\u0301")
	requireFullMatch(t, `\X`, "X", RE2)
	requireFullMatch(t, `\X`, "X", ECMAScript)
}

func TestExtendedGraphemeClusterRightToLeft(t *testing.T) {
	re := MustCompile(`\X`, RightToLeft)
	m, err := re.FindStringMatch("a\u0301b")
	if err != nil {
		t.Fatal(err)
	}
	if m == nil || m.String() != "b" {
		t.Fatalf("right-to-left first \\X match = %v; want b", m)
	}
	m, err = re.FindNextMatch(m)
	if err != nil {
		t.Fatal(err)
	}
	if m == nil || m.String() != "a\u0301" {
		t.Fatalf("right-to-left second \\X match = %v; want a+combining acute", m)
	}

	// RI pairs are formed from the start of a run, so an odd run ends in a
	// singleton even when matching it from right to left.
	re = MustCompile(`\X`, RightToLeft)
	m, err = re.FindStringMatch("🇺🇸🇨")
	if err != nil {
		t.Fatal(err)
	}
	if m == nil || m.String() != "🇨" {
		t.Fatalf("right-to-left odd RI match = %v; want final singleton", m)
	}
}

func TestGraphemeEscapeUsesDedicatedInstruction(t *testing.T) {
	for _, tt := range []struct {
		pattern string
		op      syntax.InstOp
	}{
		{`\X`, syntax.Grapheme},
	} {
		for _, options := range [][]CompileOption{nil, {OptionIsCodeGen()}} {
			re := MustCompile(tt.pattern, options...)
			found := false
			for pos := 0; pos < len(re.code.Codes); {
				op := syntax.InstOp(re.code.Codes[pos]) & syntax.Mask
				if op == tt.op {
					found = true
					break
				}
				pos += testOpcodeSize(op)
			}
			if !found {
				t.Errorf("%s did not compile to its dedicated instruction", tt.pattern)
			}
		}
	}
}

func TestUnicodeNewlineUsesGeneralInstructions(t *testing.T) {
	for _, options := range [][]CompileOption{nil, {RightToLeft}, {OptionIsCodeGen()}} {
		re := MustCompile(`\R`, options...)
		if containsTestOpcode(re, syntax.Setjump) || containsTestOpcode(re, syntax.Forejump) {
			t.Errorf("\\R retained unnecessary atomic-group bookkeeping:\n%s", re.code.Dump())
		}
		if !containsTestOpcode(re, syntax.Dispatch) {
			t.Errorf("\\R did not use disjoint-alternation dispatch:\n%s", re.code.Dump())
		}
	}
}

func TestDisjointAtomicAlternationOptimization(t *testing.T) {
	optimized := MustCompile(`(?>a[0-9]|b[0-9])e`)
	if containsTestOpcode(optimized, syntax.Setjump) || containsTestOpcode(optimized, syntax.Forejump) {
		t.Errorf("disjoint intrinsically-atomic branches retained atomic bookkeeping:\n%s", optimized.code.Dump())
	}
	if !containsTestOpcode(optimized, syntax.Dispatch) {
		t.Errorf("disjoint intrinsically-atomic branches did not use dispatch:\n%s", optimized.code.Dump())
	}
	requireFullMatch(t, `(?>a[0-9]|b[0-9])e`, "a1e")
	requireFullMatch(t, `(?>a[0-9]|b[0-9])e`, "b2e")
	requireNoFullMatch(t, `(?>a[0-9]|b[0-9])e`, "a1f")
	requireFullMatch(t, `(ab|cd)e`, "abe")
	requireFullMatch(t, `(ab|cd)e`, "cde")
	requireNoFullMatch(t, `(ab|cd)e`, "abf")
	requireFullMatch(t, `(?:éx|λy)`, "λy")

	quick := MustCompile(`\A(aX|cY)\z`)
	if quick.quickCode == nil {
		t.Fatal("expected capture-eliding quick code")
	}
	if got, err := quick.MatchString("cY"); err != nil || !got {
		t.Fatalf("quick dispatch match = (%v, %v); want true, nil", got, err)
	}

	rtl := MustCompile(`ab|cd`, RightToLeft)
	if !containsTestOpcode(rtl, syntax.Dispatch) {
		t.Errorf("right-to-left disjoint branches did not use dispatch:\n%s", rtl.code.Dump())
	}
	m, err := rtl.FindStringMatch("xxcd")
	if err != nil || m == nil || m.String() != "cd" {
		t.Fatalf("right-to-left disjoint alternation match = (%v, %v); want cd", m, err)
	}

	// Disjoint starting characters are not enough if a selected branch can
	// itself backtrack to a different successful length.
	requireNoFullMatch(t, `(?>a(?:(b)|(bc))|x)c`, "abcc")
	notOptimized := MustCompile(`(?>a(?:(b)|(bc))|x)c`)
	if !containsTestOpcode(notOptimized, syntax.Setjump) {
		t.Errorf("atomic alternation with an internally backtracking branch lost its atomic bookkeeping:\n%s", notOptimized.code.Dump())
	}
}

func TestDispatchUsesSingleInstructionForManyBranches(t *testing.T) {
	re := MustCompile(`AX|CY|EZ|GA|IB|KC|MD|OE|QF|SG|UH|WI`)
	if got := countTestOpcode(re, syntax.Dispatch); got != 1 {
		t.Fatalf("Dispatch count = %d; want 1:\n%s", got, re.code.Dump())
	}
}

func containsTestOpcode(re *Regexp, want syntax.InstOp) bool {
	return countTestOpcode(re, want) != 0
}

func countTestOpcode(re *Regexp, want syntax.InstOp) int {
	count := 0
	for pos := 0; pos < len(re.code.Codes); {
		op := syntax.InstOp(re.code.Codes[pos]) & syntax.Mask
		if op == want {
			count++
		}
		pos += testOpcodeSize(op)
	}
	return count
}

func testOpcodeSize(op syntax.InstOp) int {
	switch op {
	case syntax.Nothing, syntax.Bol, syntax.Eol, syntax.Boundary, syntax.Nonboundary,
		syntax.ECMABoundary, syntax.NonECMABoundary, syntax.Beginning, syntax.Start,
		syntax.EndZ, syntax.End, syntax.Nullmark, syntax.Setmark, syntax.Getmark,
		syntax.Setjump, syntax.Backjump, syntax.Forejump, syntax.Stop,
		syntax.UpdateBumpalong, syntax.Grapheme:
		return 1
	case syntax.One, syntax.Notone, syntax.Multi, syntax.Ref, syntax.Testref,
		syntax.Goto, syntax.Nullcount, syntax.Setcount, syntax.Lazybranch,
		syntax.Branchmark, syntax.Lazybranchmark, syntax.Prune, syntax.Set, syntax.Dispatch, syntax.ResetCapture:
		return 2
	default:
		return 3
	}
}
