package regexp2

import (
	"encoding/json"
	"fmt"
	"reflect"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/dlclark/regexp2/v2/syntax"
)

func TestBacktrack_CatastrophicTimeout(t *testing.T) {
	r, err := Compile("(.+)*\\?")
	if err != nil {
		t.Fatal(err)
	}
	t.Logf("code dump: %v", r.code.Dump())
	const subject = "Do you think you found the problem string!"

	const earlyAllowance = 10 * time.Millisecond
	var lateAllowance = clockPeriod + 500*time.Millisecond // Large allowance in case machine is slow

	for _, timeout := range []time.Duration{
		-1 * time.Millisecond,
		0 * time.Millisecond,
		1 * time.Millisecond,
		10 * time.Millisecond,
		100 * time.Millisecond,
		500 * time.Millisecond,
		1000 * time.Millisecond,
	} {
		t.Run(fmt.Sprint(timeout), func(t *testing.T) {
			r.MatchTimeout = timeout
			start := time.Now()
			m, err := r.FindStringMatch(subject)
			elapsed := time.Since(start)
			if err == nil {
				t.Errorf("expected timeout err")
			}
			if m != nil {
				t.Errorf("Expected no match")
			}
			t.Logf("timeed out after %v", elapsed)
			if elapsed < timeout-earlyAllowance {
				t.Errorf("Match timed out too quickly (%v instead of expected %v)", elapsed, timeout-earlyAllowance)
			}
			if elapsed > timeout+lateAllowance {
				t.Errorf("Match timed out too late (%v instead of expected %v)", elapsed, timeout+lateAllowance)
			}
		})
	}
}

func TestSetPrefix(t *testing.T) {
	r := MustCompile(`^\s*-TEST`)
	if r.code.FcPrefix == nil {
		t.Fatalf("Expected prefix set [-\\s] but was nil")
	}
	if r.code.FcPrefix.PrefixSet.String() != "[-\\s]" {
		t.Fatalf("Expected prefix set [\\s-] but was %v", r.code.FcPrefix.PrefixSet.String())
	}
}

func TestSetInCode(t *testing.T) {
	r := MustCompile(`(?<body>\s*(?<name>.+))`)
	t.Logf("code dump: %v", r.code.Dump())
	if want, got := 1, len(r.code.Sets); want != got {
		t.Fatalf("r.code.Sets wanted %v, got %v", want, got)
	}
	if want, got := "[\\s]", r.code.Sets[0].String(); want != got {
		t.Fatalf("first set wanted %v, got %v", want, got)
	}
}

func TestRegexp_QuantifiedStartAnchor(t *testing.T) {
	r, err := Compile(`^*`)
	if err != nil {
		t.Fatalf("Compile(^*): %v", err)
	}
	r.MatchTimeout = time.Second

	for _, input := range []string{"", "abc"} {
		t.Run(fmt.Sprintf("input=%q", input), func(t *testing.T) {
			m, err := r.FindStringMatch(input)
			if err != nil {
				t.Fatalf("FindStringMatch(%q): %v", input, err)
			}
			if m == nil {
				t.Fatal("expected an empty match at the start of the input")
			}
			if m.RuneIndex != 0 || m.RuneLength != 0 {
				t.Fatalf("match at %d with length %d, want index 0 and length 0", m.RuneIndex, m.RuneLength)
			}
		})
	}
}

func TestRegexp_QuantifiedWordBoundary(t *testing.T) {
	r, err := Compile(`\b{2}`)
	if err != nil {
		t.Fatalf("Compile(\\b{2}): %v", err)
	}
	r.MatchTimeout = time.Second

	for _, tc := range []struct {
		input string
		index int // -1 means no match.
	}{
		{"", -1},
		{"   ", -1},
		{"abc", 0},
		{" abc", 1},
	} {
		t.Run(fmt.Sprintf("input=%q", tc.input), func(t *testing.T) {
			m, err := r.FindStringMatch(tc.input)
			if err != nil {
				t.Fatalf("FindStringMatch(%q): %v", tc.input, err)
			}
			if tc.index == -1 {
				if m != nil {
					t.Fatalf("expected no match, got a match at %d", m.RuneIndex)
				}
				return
			}
			if m == nil {
				t.Fatalf("expected an empty match at index %d", tc.index)
			}
			if m.RuneIndex != tc.index || m.RuneLength != 0 {
				t.Fatalf("match at %d with length %d, want index %d and length 0", m.RuneIndex, m.RuneLength, tc.index)
			}
		})
	}
}

func TestRegexp_Basic(t *testing.T) {
	r, err := Compile("test(?<named>ing)?")
	//t.Logf("code dump: %v", r.code.Dump())

	if err != nil {
		t.Errorf("unexpected compile err: %v", err)
	}
	m, err := r.FindStringMatch("this is a testing stuff")
	if err != nil {
		t.Errorf("unexpected match err: %v", err)
	}
	if m == nil {
		t.Error("Nil match, expected success")
	}
}

// check all our functions and properties around basic capture groups and referential for Group 0
func TestCapture_Basic(t *testing.T) {
	r := MustCompile(`.*\B(SUCCESS)\B.*`)
	m, err := r.FindStringMatch("adfadsfSUCCESSadsfadsf")
	if err != nil {
		t.Fatalf("Unexpected match error: %v", err)
	}

	if m == nil {
		t.Fatalf("Should have matched")
	}
	if want, got := "adfadsfSUCCESSadsfadsf", m.String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := 0, m.RuneIndex; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := 22, m.RuneLength; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := 1, len(m.Captures); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}

	if want, got := m.String(), m.Captures[0].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := 0, m.Captures[0].RuneIndex; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := 22, m.Captures[0].RuneLength; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}

	g := m.Groups()
	if want, got := 2, len(g); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	// group 0 is always the match
	if want, got := m.String(), g[0].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := 1, len(g[0].Captures); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	// group 0's capture is always the match
	if want, got := m.Captures[0].String(), g[0].Captures[0].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}

	// group 1 is our first explicit group (unnamed)
	if want, got := 7, g[1].RuneIndex; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := 7, g[1].RuneLength; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := "SUCCESS", g[1].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
}

func TestCapture_ByteOffsets(t *testing.T) {
	re := MustCompile(`(猫)(b🙂)`)
	m, err := re.FindStringMatch("πa猫b🙂c")
	if err != nil {
		t.Fatalf("Unexpected match error: %v", err)
	}
	if m == nil {
		t.Fatal("Should have matched")
	}

	if want, got := 2, m.RuneIndex; want != got {
		t.Fatalf("Match RuneIndex wanted %v got %v", want, got)
	}
	if want, got := 3, m.RuneLength; want != got {
		t.Fatalf("Match RuneLength wanted %v got %v", want, got)
	}
	assertByteRange(t, "Match", m, 3, 8)
	assertByteRange(t, "Root capture", &m.Captures[0], 3, 8)

	groups := m.Groups()
	if want, got := 3, len(groups); want != got {
		t.Fatalf("Group count wanted %v got %v", want, got)
	}
	assertByteRange(t, "Group 1", &groups[1], 3, 3)
	assertByteRange(t, "Group 2", &groups[2], 6, 5)
	assertByteRange(t, "Group 2 capture", &groups[2].Captures[0], 6, 5)
}

func TestCapture_ByteOffsetsFindNextMatch(t *testing.T) {
	re := MustCompile(`é.`)
	m, err := re.FindStringMatch("éxéy")
	if err != nil {
		t.Fatalf("Unexpected match error: %v", err)
	}
	if m == nil {
		t.Fatal("Expected first match")
	}
	assertByteRange(t, "First match", m, 0, 3)

	m, err = re.FindNextMatch(m)
	if err != nil {
		t.Fatalf("Unexpected next match error: %v", err)
	}
	if m == nil {
		t.Fatal("Expected second match")
	}
	if want, got := 2, m.RuneIndex; want != got {
		t.Fatalf("Second match RuneIndex wanted %v got %v", want, got)
	}
	assertByteRange(t, "Second match", m, 3, 3)
}

func TestCapture_ByteOffsetsStartingAt(t *testing.T) {
	type startCase struct {
		name, pattern, input string
		start, want, length  int
	}
	cases := []startCase{
		{"unicode", `漢`, "aé漢b", 1, 2, 1},
	}
	for _, tc := range []struct{ start, want int }{{0, 79}, {79, 79}, {80, 84}, {83, 84}, {84, 84}, {85, -1}, {88, -1}} {
		cases = append(cases, startCase{fmt.Sprintf("alternative_%d", tc.start), `aaba|aaca|bada`, "界" + strings.Repeat("a", 80) + "ca?aaba", tc.start, tc.want, 4})
	}
	for _, tc := range []struct{ start, want int }{{0, 2}, {1, 2}, {2, 2}, {3, -1}, {5, -1}} {
		cases = append(cases, startCase{fmt.Sprintf("end_anchor_%d", tc.start), `(?<head>a)(?<tail>a[ \n])$`, "界aaa\n", tc.start, tc.want, 3})
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			re := MustCompile(tc.pattern)
			offsets := []int{}
			for i := range tc.input {
				offsets = append(offsets, i)
			}
			offsets = append(offsets, len(tc.input))
			for _, mode := range []string{"string", "runes"} {
				var m *Match
				var err error
				if mode == "string" {
					m, err = re.FindStringMatchStartingAt(tc.input, offsets[tc.start])
				} else {
					m, err = re.FindRunesMatchStartingAt([]rune(tc.input), tc.start)
				}
				if err != nil {
					t.Fatal(err)
				}
				if tc.want < 0 {
					if m != nil {
						t.Fatalf("unexpected match %v", m)
					}
					continue
				}
				if m == nil || m.RuneIndex != tc.want || m.RuneLength != tc.length {
					t.Fatalf("%s: got %v; want span [%d,%d)", mode, m, tc.want, tc.want+tc.length)
				}
				assertByteRange(t, mode, m, offsets[tc.want], offsets[tc.want+tc.length]-offsets[tc.want])
			}
			for i := 0; i <= len(tc.input)+1; i++ {
				if slices.Contains(offsets, i) {
					continue
				}
				if _, err := re.FindStringMatchStartingAt(tc.input, i); err == nil {
					t.Fatalf("expected invalid starting-position error at byte %d", i)
				}
			}
		})
	}
}

func TestCapture_ByteOffsetsRightToLeft(t *testing.T) {
	re := MustCompile(`.`, RightToLeft)
	m, err := re.FindStringMatch("aéb")
	if err != nil {
		t.Fatalf("Unexpected match error: %v", err)
	}
	if m == nil {
		t.Fatal("Should have matched")
	}
	if want, got := 2, m.RuneIndex; want != got {
		t.Fatalf("RuneIndex wanted %v got %v", want, got)
	}
	assertByteRange(t, "Match", m, 3, 1)
}

func TestCapture_NamedGroupsAfterSlice(t *testing.T) {
	type groupWant struct {
		name, text string
		start      int
		count      int
	}
	for _, tc := range []struct {
		name, pattern, input, text string
		start                      int
		groups                     []groupWant
	}{
		{"unicode_prefix", `(?<left>nee)(?<right>dle)`, strings.Repeat("漢", 12) + "needle-extra", "needle", 12, []groupWant{{"left", "nee", 12, 1}, {"right", "dle", 15, 1}}},
		{"rejected_alternative_captures", `(?:(?<first>aaba)|(?<second>aaca)|(?<third>bada))(?<suffix>!)`, strings.Repeat("a", 80) + "ca?bada!", "bada!", 83, []groupWant{{"first", "", 0, 0}, {"second", "", 0, 0}, {"third", "bada", 83, 1}, {"suffix", "!", 87, 1}}},
		{"rejected_end_candidate_captures", `(?<head>a)(?<tail>a[ \n])$`, "界aaa\n", "aa\n", 2, []groupWant{{"head", "a", 2, 1}, {"tail", "a\n", 3, 1}}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			re := MustCompile(tc.pattern)
			offsets := []int{}
			for i := range tc.input {
				offsets = append(offsets, i)
			}
			offsets = append(offsets, len(tc.input))
			for _, mode := range []string{"string", "runes"} {
				var m *Match
				var err error
				if mode == "string" {
					m, err = re.FindStringMatch(tc.input)
				} else {
					m, err = re.FindRunesMatch([]rune(tc.input))
				}
				if err != nil || m == nil {
					t.Fatalf("%s: missing match: %v", mode, err)
				}
				if m.String() != tc.text || m.RuneIndex != tc.start || m.RuneLength != len([]rune(tc.text)) {
					t.Fatalf("%s: unexpected match %q at %d", mode, m.String(), m.RuneIndex)
				}
				assertByteRange(t, "Match", m, offsets[tc.start], len(tc.text))
				if len(m.Groups()) != len(tc.groups)+1 {
					t.Fatalf("unexpected group count %d", len(m.Groups()))
				}
				for _, want := range tc.groups {
					g := m.GroupByName(want.name)
					if g == nil || g.String() != want.text || len(g.Captures) != want.count {
						t.Fatalf("%s: unexpected group %q: %v", mode, want.name, g)
					}
					if want.count > 0 {
						if g.RuneIndex != want.start || g.RuneLength != len([]rune(want.text)) {
							t.Fatalf("unexpected span for group %q", want.name)
						}
						assertByteRange(t, want.name, g, offsets[want.start], len(want.text))
					}
				}
			}
		})
	}
}

func TestCapture_ByteOffsetsRunesInput(t *testing.T) {
	re := MustCompile(`é漢`)
	m, err := re.FindRunesMatch([]rune("aé漢"))
	if err != nil {
		t.Fatalf("Unexpected match error: %v", err)
	}
	if m == nil {
		t.Fatal("Should have matched")
	}
	if want, got := 1, m.RuneIndex; want != got {
		t.Fatalf("RuneIndex wanted %v got %v", want, got)
	}
	assertByteRange(t, "Match", m, 1, 5)
}

type byteRanger interface {
	ByteRange() (int, int)
}

func assertByteRange(t *testing.T, name string, c byteRanger, wantIndex, wantLength int) {
	t.Helper()
	gotIndex, gotLength := c.ByteRange()
	if gotIndex != wantIndex || gotLength != wantLength {
		t.Fatalf("%s ByteRange wanted (%v, %v) got (%v, %v)", name, wantIndex, wantLength, gotIndex, gotLength)
	}
}

func TestEscapeUnescape_Basic(t *testing.T) {
	s1 := "#$^*+(){}<>\\|. "
	s2 := Escape(s1)
	s3, err := Unescape(s2)
	if err != nil {
		t.Fatalf("Unexpected error during unescape: %v", err)
	}

	//confirm one way
	if want, got := `\#\$\^\*\+\(\)\{\}<>\\\|\.\ `, s2; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}

	//confirm round-trip
	if want, got := s1, s3; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}

}

func TestGroups_Basic(t *testing.T) {
	type d struct {
		p    string
		s    string
		name []string
		num  []int
		strs []string
	}
	data := []d{
		{"(?<first_name>\\S+)\\s(?<last_name>\\S+)", // example
			"Ryan Byington",
			[]string{"0", "first_name", "last_name"},
			[]int{0, 1, 2},
			[]string{"Ryan Byington", "Ryan", "Byington"}},
		{"((?<One>abc)\\d+)?(?<Two>xyz)(.*)", // example
			"abc208923xyzanqnakl",
			[]string{"0", "1", "2", "One", "Two"},
			[]int{0, 1, 2, 3, 4},
			[]string{"abc208923xyzanqnakl", "abc208923", "anqnakl", "abc", "xyz"}},
		{"((?<256>abc)\\d+)?(?<16>xyz)(.*)", // numeric names
			"0272saasdabc8978xyz][]12_+-",
			[]string{"0", "1", "2", "16", "256"},
			[]int{0, 1, 2, 16, 256},
			[]string{"abc8978xyz][]12_+-", "abc8978", "][]12_+-", "xyz", "abc"}},
		{"((?<4>abc)(?<digits>\\d+))?(?<2>xyz)(?<everything_else>.*)", // mix numeric and string names
			"0272saasdabc8978xyz][]12_+-",
			[]string{"0", "1", "2", "digits", "4", "everything_else"},
			[]int{0, 1, 2, 3, 4, 5},
			[]string{"abc8978xyz][]12_+-", "abc8978", "xyz", "8978", "abc", "][]12_+-"}},
		{"(?<first_name>\\S+)\\s(?<first_name>\\S+)", // dupe string names
			"Ryan Byington",
			[]string{"0", "first_name"},
			[]int{0, 1},
			[]string{"Ryan Byington", "Byington"}},
		{"(?<15>\\S+)\\s(?<15>\\S+)", // dupe numeric names
			"Ryan Byington",
			[]string{"0", "15"},
			[]int{0, 15},
			[]string{"Ryan Byington", "Byington"}},
		// *** repeated from above, but with alt cap syntax ***
		{"(?'first_name'\\S+)\\s(?'last_name'\\S+)", //example
			"Ryan Byington",
			[]string{"0", "first_name", "last_name"},
			[]int{0, 1, 2},
			[]string{"Ryan Byington", "Ryan", "Byington"}},
		{"((?'One'abc)\\d+)?(?'Two'xyz)(.*)", // example
			"abc208923xyzanqnakl",
			[]string{"0", "1", "2", "One", "Two"},
			[]int{0, 1, 2, 3, 4},
			[]string{"abc208923xyzanqnakl", "abc208923", "anqnakl", "abc", "xyz"}},
		{"((?'256'abc)\\d+)?(?'16'xyz)(.*)", // numeric names
			"0272saasdabc8978xyz][]12_+-",
			[]string{"0", "1", "2", "16", "256"},
			[]int{0, 1, 2, 16, 256},
			[]string{"abc8978xyz][]12_+-", "abc8978", "][]12_+-", "xyz", "abc"}},
		{"((?'4'abc)(?'digits'\\d+))?(?'2'xyz)(?'everything_else'.*)", // mix numeric and string names
			"0272saasdabc8978xyz][]12_+-",
			[]string{"0", "1", "2", "digits", "4", "everything_else"},
			[]int{0, 1, 2, 3, 4, 5},
			[]string{"abc8978xyz][]12_+-", "abc8978", "xyz", "8978", "abc", "][]12_+-"}},
		{"(?'first_name'\\S+)\\s(?'first_name'\\S+)", // dupe string names
			"Ryan Byington",
			[]string{"0", "first_name"},
			[]int{0, 1},
			[]string{"Ryan Byington", "Byington"}},
		{"(?'15'\\S+)\\s(?'15'\\S+)", // dupe numeric names
			"Ryan Byington",
			[]string{"0", "15"},
			[]int{0, 15},
			[]string{"Ryan Byington", "Byington"}},
	}

	fatalf := func(re *Regexp, v d, format string, args ...any) {
		args = append(args, v, re.code.Dump())

		t.Fatalf(format+" using test data: %#v\ndump:%v", args...)
	}

	validateGroupNamesNumbers := func(re *Regexp, v d) {
		if len(v.name) != len(v.num) {
			fatalf(re, v, "Invalid data, group name count and number count must match")
		}

		groupNames := re.GetGroupNames()
		if !reflect.DeepEqual(groupNames, v.name) {
			fatalf(re, v, "group names expected: %v, actual: %v", v.name, groupNames)
		}
		groupNums := re.GetGroupNumbers()
		if !reflect.DeepEqual(groupNums, v.num) {
			fatalf(re, v, "group numbers expected: %v, actual: %v", v.num, groupNums)
		}
		// make sure we can freely get names and numbers from eachother
		for i := range groupNums {
			if want, got := groupNums[i], re.GroupNumberFromName(groupNames[i]); want != got {
				fatalf(re, v, "group num from name Wanted '%v'\nGot '%v'", want, got)
			}
			if want, got := groupNames[i], re.GroupNameFromNumber(groupNums[i]); want != got {
				fatalf(re, v, "group name from num Wanted '%v'\nGot '%v'", want, got)
			}
		}
	}

	for _, v := range data {
		// compile the regex
		re := MustCompile(v.p)

		// validate our group name/num info before execute
		validateGroupNamesNumbers(re, v)

		m, err := re.FindStringMatch(v.s)
		if err != nil {
			fatalf(re, v, "Unexpected error in match: %v", err)
		}
		if m == nil {
			fatalf(re, v, "Match is nil")
		}
		if want, got := len(v.strs), m.GroupCount(); want != got {
			fatalf(re, v, "GroupCount() Wanted '%v'\nGot '%v'", want, got)
		}
		g := m.Groups()
		if want, got := len(v.strs), len(g); want != got {
			fatalf(re, v, "len(m.Groups()) Wanted '%v'\nGot '%v'", want, got)
		}
		// validate each group's value from the execute
		for i := range v.name {
			grp1 := m.GroupByName(v.name[i])
			grp2 := m.GroupByNumber(v.num[i])
			// should be identical reference
			if grp1 != grp2 {
				fatalf(re, v, "Expected GroupByName and GroupByNumber to return same result for %v, %v", v.name[i], v.num[i])
			}
			if want, got := v.strs[i], grp1.String(); want != got {
				fatalf(re, v, "Value[%v] Wanted '%v'\nGot '%v'", i, want, got)
			}
		}

		// validate our group name/num info after execute
		validateGroupNamesNumbers(re, v)
	}
}

func TestGroupByNumberSparseRegression(t *testing.T) {
	for _, tt := range []struct {
		pattern, input string
		groups         map[int]string
		missing        []int
	}{
		{`(?<5>a)`, "a", map[int]string{0: "a", 5: "a"}, []int{-1, 1, 2, 6}},
		{`(a)(?<5>b)`, "ab", map[int]string{0: "ab", 1: "a", 5: "b"}, []int{-1, 2, 3, 6}},
		{`(a)(b)`, "ab", map[int]string{0: "ab", 1: "a", 2: "b"}, []int{-1, 3, 5}},
	} {
		t.Run(tt.pattern, func(t *testing.T) {
			m, err := MustCompile(tt.pattern).FindStringMatch(tt.input)
			if err != nil || m == nil {
				t.Fatalf("match = %v, %v", m, err)
			}
			for number, want := range tt.groups {
				g := m.GroupByNumber(number)
				if g == nil || g.String() != want {
					t.Errorf("group %d = %v; want %q", number, g, want)
				}
			}
			for _, number := range tt.missing {
				if g := m.GroupByNumber(number); g != nil {
					t.Errorf("group %d = %v; want nil", number, g)
				}
			}
		})
	}
}

func TestErr_GroupName(t *testing.T) {
	// group 0 is off limits
	if _, err := Compile("foo(?<0>bar)"); err == nil {
		t.Fatalf("zero group, expected error during compile")
	} else if want, got := "error parsing regexp: capture number cannot be zero in `foo(?<0>bar)`", err.Error(); want != got {
		t.Fatalf("invalid error text, want '%v', got '%v'", want, got)
	}
	if _, err := Compile("foo(?'0'bar)"); err == nil {
		t.Fatalf("zero group, expected error during compile")
	} else if want, got := "error parsing regexp: capture number cannot be zero in `foo(?'0'bar)`", err.Error(); want != got {
		t.Fatalf("invalid error text, want '%v', got '%v'", want, got)
	}

	// group tag can't start with a num
	if _, err := Compile("foo(?<1bar>)"); err == nil {
		t.Fatalf("invalid group name, expected error during compile")
	} else if want, got := "error parsing regexp: invalid group name: group names must begin with a word character and have a matching terminator in `foo(?<1bar>)`", err.Error(); want != got {
		t.Fatalf("invalid error text, want '%v', got '%v'", want, got)
	}
	if _, err := Compile("foo(?'1bar')"); err == nil {
		t.Fatalf("invalid group name, expected error during compile")
	} else if want, got := "error parsing regexp: invalid group name: group names must begin with a word character and have a matching terminator in `foo(?'1bar')`", err.Error(); want != got {
		t.Fatalf("invalid error text, want '%v', got '%v'", want, got)
	}

	// missing closing group tag
	if _, err := Compile("foo(?<bar)"); err == nil {
		t.Fatalf("invalid group name, expected error during compile")
	} else if want, got := "error parsing regexp: invalid group name: group names must begin with a word character and have a matching terminator in `foo(?<bar)`", err.Error(); want != got {
		t.Fatalf("invalid error text, want '%v', got '%v'", want, got)
	}
	if _, err := Compile("foo(?'bar)"); err == nil {
		t.Fatalf("invalid group name, expected error during compile")
	} else if want, got := "error parsing regexp: invalid group name: group names must begin with a word character and have a matching terminator in `foo(?'bar)`", err.Error(); want != got {
		t.Fatalf("invalid error text, want '%v', got '%v'", want, got)
	}

}

func TestErr_UnterminatedCommentAfterLiteral(t *testing.T) {
	if _, err := Compile("a(?#"); err == nil {
		t.Fatalf("unterminated comment, expected error during compile")
	} else if want, got := "error parsing regexp: unterminated comment in `a(?#`", err.Error(); want != got {
		t.Fatalf("invalid error text, want '%v', got '%v'", want, got)
	}
}

func TestConstantUneffected(t *testing.T) {
	// had a bug where "constant" sets would get modified with alternations and be broken in memory until restart
	// this meant that if you used a known-set (like \s) in a larger set it would "poison" \s for the process
	re := MustCompile(`(\s|\*)test\s`)
	if want, got := 2, len(re.code.Sets); want != got {
		t.Fatalf("wanted %v sets, got %v", want, got)
	}
	if want, got := "[\\*\\s]", re.code.Sets[0].String(); want != got {
		t.Fatalf("wanted set 0 %v, got %v", want, got)
	}
	if want, got := "[\\s]", re.code.Sets[1].String(); want != got {
		t.Fatalf("wanted set 1 %v, got %v", want, got)
	}
}

func TestAlternationConstAndEscape(t *testing.T) {
	re := MustCompile(`\:|\s`)
	if want, got := 1, len(re.code.Sets); want != got {
		t.Fatalf("wanted %v sets, got %v", want, got)
	}
	if want, got := "[:\\s]", re.code.Sets[0].String(); want != got {
		t.Fatalf("wanted set 0 %v, got %v", want, got)
	}
}

func TestStartingCharsOptionalNegate(t *testing.T) {
	// to maintain matching with the corefx we've made the negative char classes be negative and the
	// categories they contain positive.  This means they're not combinable or suitable for prefixes.
	// In general this could be a fine thing since negatives are extremely wide groups and not
	// missing much on prefix optimizations.

	// the below expression *could* have a prefix of [\S\d] but
	// this requires a change in charclass.go when setting
	// NotSpaceClass = getCharSetFromCategoryString()
	// to negate the individual categories rather than the CharSet itself
	// this would deviate from corefx

	re := MustCompile(`(^(\S{2} )?\S{2}(\d+|/) *\S{3}\S{3} ?\{2,4}[A-Z] ?\{2}[A-Z]{3}|(\S{2} )?\{2,4})`)
	if re.code.FcPrefix != nil {
		t.Fatalf("FcPrefix wanted nil, got %v", re.code.FcPrefix)
	}
}

func TestParseNegativeDigit(t *testing.T) {
	re := MustCompile(`\D`)
	if want, got := 1, len(re.code.Sets); want != got {
		t.Fatalf("wanted %v sets, got %v", want, got)
	}

	if want, got := "[\\P{Nd}]", re.code.Sets[0].String(); want != got {
		t.Fatalf("wanted set 0 %v, got %v", want, got)
	}
}

func TestRunNegativeDigit(t *testing.T) {
	re := MustCompile(`\D`)
	m, err := re.MatchString("this is a test")
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
	if !m {
		t.Fatalf("Expected match")
	}
}

func TestCancellingClasses(t *testing.T) {
	// [\w\W\s] should become "." because it means "anything"
	re := MustCompile(`[\w\W\s]`)
	if want, got := 1, len(re.code.Sets); want != got {
		t.Fatalf("wanted %v sets, got %v", want, got)
	}
	if want, got := syntax.AnyClass().String(), re.code.Sets[0].String(); want != got {
		t.Fatalf("wanted set 0 %v, got %v", want, got)
	}
}

func TestConcatLoopCaptureSet(t *testing.T) {
	//(A|B)*?CD different Concat/Loop/Capture/Set (had [A-Z] should be [AB])
	// we were not copying the Sets in the prefix FC stack, so the underlying sets were unexpectedly mutating
	// so set [AB] becomes [ABC] when we see the the static C in FC stack generation (which are the valid start chars),
	// but that was mutating the tree node's original set [AB] because even though we copied the slice header,
	// the two header's pointed to the same underlying byte array...which was mutated.

	re := MustCompile(`(A|B)*CD`)
	if want, got := 1, len(re.code.Sets); want != got {
		t.Fatalf("wanted %v sets, got %v", want, got)
	}
	if want, got := "[AB]", re.code.Sets[0].String(); want != got {
		t.Fatalf("wanted set 0 %v, got %v", want, got)
	}
}

func TestFirstcharsIgnoreCase(t *testing.T) {
	//((?i)AB(?-i)C|D)E different Firstchars (had [da] should be [ad])
	// we were not canonicalizing when converting the prefix set to lower case
	// so our set's were potentially not searching properly
	re := MustCompile(`((?i)AB(?-i)C|D)E`)

	if re.code.FcPrefix == nil {
		t.Fatalf("wanted prefix, got nil")
	}

	if want, got := "[ADa]", re.code.FcPrefix.PrefixSet.String(); want != got {
		t.Fatalf("wanted prefix %v, got %v", want, got)
	}
}

func TestRepeatingGroup(t *testing.T) {
	for _, tc := range []struct {
		pattern, input string
		start          int
		captures       []string
	}{
		{`(data?)+`, "datadat", 0, []string{"data", "dat"}},
		{`(?<piece>aaba|aaca|bada)+!`, strings.Repeat("a", 80) + "cabada!", 78, []string{"aaca", "bada"}},
	} {
		t.Run(tc.pattern, func(t *testing.T) {
			re := MustCompile(tc.pattern)
			for _, mode := range []string{"string", "runes"} {
				var m *Match
				var err error
				if mode == "string" {
					m, err = re.FindStringMatch(tc.input)
				} else {
					m, err = re.FindRunesMatch([]rune(tc.input))
				}
				if err != nil || m == nil {
					t.Fatalf("%s: missing match, %v", mode, err)
				}
				g := m.GroupByNumber(1)
				if g == nil || len(g.Captures) != len(tc.captures) {
					t.Fatalf("%s: unexpected captures %v", mode, g)
				}
				start := tc.start
				for i, want := range tc.captures {
					c := g.Captures[i]
					if c.String() != want || c.RuneIndex != start || c.RuneLength != len([]rune(want)) {
						t.Fatalf("%s: capture %d = %q at %d; want %q at %d", mode, i, c.String(), c.RuneIndex, want, start)
					}
					start += c.RuneLength
				}
				last := g.Captures[len(g.Captures)-1]
				if g.String() != last.String() || g.RuneIndex != last.RuneIndex {
					t.Fatal("expected last capture of the group to be embedded")
				}
			}
		})
	}
}

func TestFindNextMatch_Basic(t *testing.T) {
	re := MustCompile(`(T|E)(?=h|E|S|$)`)
	m, err := re.FindStringMatch(`This is a TEST`)
	if err != nil {
		t.Fatalf("Unexpected err 0: %v", err)
	}
	if m == nil {
		t.Fatalf("Expected match 0")
	}
	if want, got := 0, m.RuneIndex; want != got {
		t.Fatalf("expected match 0 to start at %v, got %v", want, got)
	}

	m, err = re.FindNextMatch(m)
	if err != nil {
		t.Fatalf("Unexpected err 1: %v", err)
	}
	if m == nil {
		t.Fatalf("Expected match 1")
	}
	if want, got := 10, m.RuneIndex; want != got {
		t.Fatalf("expected match 1 to start at %v, got %v", want, got)
	}

	m, err = re.FindNextMatch(m)
	if err != nil {
		t.Fatalf("Unexpected err 2: %v", err)
	}
	if m == nil {
		t.Fatalf("Expected match 2")
	}
	if want, got := 11, m.RuneIndex; want != got {
		t.Fatalf("expected match 2 to start at %v, got %v", want, got)
	}

	m, err = re.FindNextMatch(m)
	if err != nil {
		t.Fatalf("Unexpected err 3: %v", err)
	}
	if m == nil {
		t.Fatalf("Expected match 3")
	}
	if want, got := 13, m.RuneIndex; want != got {
		t.Fatalf("expected match 3 to start at %v, got %v", want, got)
	}
}

func TestFindNextMatch_ZeroWidthAfterScanAdvance(t *testing.T) {
	re := MustCompile(`(?=[A-Z])`)
	m, err := re.FindRunesMatch([]rune("userName"))
	if err != nil {
		t.Fatalf("FindRunesMatch failed: %v", err)
	}
	if m == nil {
		t.Fatal("FindRunesMatch did not match")
	}
	if got, want := m.RuneIndex, 4; got != want {
		t.Fatalf("match RuneIndex = %d, want %d", got, want)
	}
	if got := m.RuneLength; got != 0 {
		t.Fatalf("match RuneLength = %d, want 0", got)
	}

	m, err = re.FindNextMatch(m)
	if err != nil {
		t.Fatalf("FindNextMatch failed: %v", err)
	}
	if m != nil {
		t.Fatalf("FindNextMatch = (%d, %d), want nil", m.RuneIndex, m.RuneLength)
	}
}

func TestFindAllStringIndex(t *testing.T) {
	type indexCase struct {
		name, pattern, input string
		options              []CompileOption
		want                 [][2]int // rune spans; string APIs must return the corresponding byte spans
	}
	cases := []indexCase{
		{name: "unicode_captures", pattern: `é(.)`, input: "éxéy", options: []CompileOption{RE2}, want: [][2]int{{0, 2}, {2, 4}}},

		// Case folding and near misses.
		{name: "empty", pattern: `(?i)ab`, input: ""},
		{name: "too_short", pattern: `(?i)ab`, input: "A"},
		{name: "lowercase_candidates_only", pattern: `(?i)ab`, input: strings.Repeat("a", 80)},
		{name: "uppercase_candidates_only", pattern: `(?i)ab`, input: strings.Repeat("A", 80)},
		{name: "mixed_failed_candidates", pattern: `(?i)ab`, input: "aA-aX-Ac-aB", want: [][2]int{{9, 11}}},
		{name: "both_cases_after_dense_misses", pattern: `(?i)ab`, input: strings.Repeat("a", 80) + "ABAAAAab", want: [][2]int{{80, 82}, {86, 88}}},
		{name: "overlapping_candidate", pattern: `(?i)aab`, input: "AAAAb", want: [][2]int{{2, 5}}},
		{name: "punctuation_is_not_case_folded", pattern: `(?i)\[ab\]`, input: "{AB] [aB} [Ab]", want: [][2]int{{10, 14}}},
		{name: "unicode_before_ascii", pattern: `(?i)ab`, input: "界😀--AB!", want: [][2]int{{4, 6}}},
		{name: "invalid_utf8_between_candidates", pattern: `(?i)ab`, input: "x\xffAB\x80ab", want: [][2]int{{2, 4}, {5, 7}}},
		{name: "unicode_literal_fallback", pattern: `(?i)éx`, input: "Éx-éX", want: [][2]int{{0, 2}, {3, 5}}},
		{name: "non_ascii_near_miss", pattern: `(?i)ab`, input: "aβ aЬ"},
		{name: "scoped_case_sensitivity", pattern: `(?i:ab)C`, input: "ABc-abC", want: [][2]int{{4, 7}}},
		{name: "lookahead_rejects_early_prefix", pattern: `(?i:ab)(?=!)`, input: "AB? ab!", want: [][2]int{{4, 6}}},
		{name: "negative_lookahead", pattern: `(?i:ab)(?!!)`, input: "AB! ab?", want: [][2]int{{4, 6}}},
		{name: "rtl_case_folding", pattern: `(?i)ab`, input: "AB-aa-aB", options: []CompileOption{RightToLeft}, want: [][2]int{{6, 8}, {0, 2}}},

		// Alternative ordering, partial candidates, and fallback cases.
		{name: "distinct_shared_first_prefixes_miss", pattern: `aaba|aaca|bada`, input: strings.Repeat("a", 80)},
		{name: "distinct_shared_first_prefixes_hit", pattern: `aaba|aaca|bada`, input: strings.Repeat("a", 80) + "ca", want: [][2]int{{78, 82}}},
		{name: "later_branch_has_earlier_match", pattern: `bada|aaba|aaca`, input: strings.Repeat("a", 80) + "cabada", want: [][2]int{{78, 82}, {82, 86}}},
		{name: "distinct_prefix_near_miss_then_hit", pattern: `(?:aaba|aaca|bada)!`, input: strings.Repeat("a", 80) + "ca?bada!", want: [][2]int{{83, 88}}},
		{name: "distinct_prefix_missing_suffix", pattern: `(?:aaba|aaca|bada)!`, input: strings.Repeat("a", 80) + "ca"},
		{name: "distinct_prefix_invalid_byte_boundary", pattern: `aaba|aaca|bada`, input: strings.Repeat("a", 80) + "\xffaaca", want: [][2]int{{81, 85}}},
		{name: "distinct_prefix_unicode_boundary", pattern: `aaba|aaca|bada`, input: strings.Repeat("a", 80) + "界aaca", want: [][2]int{{81, 85}}},
		{name: "shared_prefix_miss", pattern: `aaab|aaac|aaad`, input: strings.Repeat("a", 80)},
		{name: "dense_last_position", pattern: `aaab|aaac|aaad`, input: strings.Repeat("a", 80) + "c", want: [][2]int{{77, 81}}},
		{name: "earliest_not_first_branch", pattern: `aaad|aaac|aaab`, input: strings.Repeat("a", 80) + "baaac", want: [][2]int{{77, 81}, {81, 85}}},
		{name: "overlapping_prefixes", pattern: `abab|baba|abac`, input: strings.Repeat("x", 80) + "babababac", want: [][2]int{{80, 84}, {84, 88}}},
		{name: "later_condition_rejects_candidate", pattern: `(?:aaab|aaac|aaad)!`, input: strings.Repeat("a", 80) + "c?aaab!", want: [][2]int{{82, 87}}},
		{name: "missing_required_suffix", pattern: `(?:aaab|aaac|aaad)!`, input: strings.Repeat("a", 80) + "c"},
		{name: "negative_assertion_rejects_candidate", pattern: `(?:aaab|aaac|aaad)(?!X)`, input: strings.Repeat("a", 80) + "cXaaab?", want: [][2]int{{82, 86}}},
		{name: "lookbehind_context", pattern: `(?<=!)(?:aaab|aaac|aaad)`, input: strings.Repeat("a", 80) + "c!aaad", want: [][2]int{{82, 86}}},
		{name: "invalid_utf8_breaks_prefix", pattern: `aaab|aaac|aaad`, input: strings.Repeat("a", 80) + "\xffaaac", want: [][2]int{{81, 85}}},
		{name: "non_ascii_breaks_prefix", pattern: `aaab|aaac|aaad`, input: strings.Repeat("a", 80) + "界aaad", want: [][2]int{{81, 85}}},
		{name: "different_lengths_earliest", pattern: `abcdef|bc`, input: "abcdef", want: [][2]int{{0, 6}}},
		{name: "different_lengths_branch_order", pattern: `ab|abcd`, input: "abcd", want: [][2]int{{0, 2}}},
		{name: "long_equal_alternatives", pattern: strings.Repeat("a", 32) + "|" + strings.Repeat("b", 32), input: strings.Repeat("a", 31) + "x" + strings.Repeat("b", 32), want: [][2]int{{32, 64}}},
		{name: "longer_equal_alternatives", pattern: strings.Repeat("a", 33) + "|" + strings.Repeat("b", 33), input: strings.Repeat("a", 32) + "x" + strings.Repeat("b", 33), want: [][2]int{{33, 66}}},
		{name: "unicode_alternative", pattern: `aaab|界界|aaac`, input: strings.Repeat("a", 80) + "界界aaac", want: [][2]int{{80, 82}, {82, 86}}},
		{name: "case_sensitive_alternatives", pattern: `aaab|aaac|aaad`, input: strings.Repeat("A", 80) + "AAAC"},
		{name: "case_insensitive_alternatives", pattern: `(?i:aaab|aaac|aaad)`, input: strings.Repeat("A", 80) + "C", want: [][2]int{{77, 81}}},
		{name: "rtl_alternatives", pattern: `aaab|aaac|aaad`, input: "aaab--aaac", options: []CompileOption{RightToLeft}, want: [][2]int{{6, 10}, {0, 4}}},
		{name: "nul_literal", pattern: "a\x00b|a\x00c", input: "a\x00x-a\x00c", want: [][2]int{{4, 7}}},

		// End anchors and final-newline candidates.
		{name: "absolute_end", pattern: `ab\z`, input: "xxab", want: [][2]int{{2, 4}}},
		{name: "absolute_end_rejects_final_newline", pattern: `ab\z`, input: "xxab\n"},
		{name: "end_z_before_final_newline", pattern: `ab\Z`, input: "xxab\n", want: [][2]int{{2, 4}}},
		{name: "crlf_requires_consuming_carriage_return", pattern: `ab$`, input: "xab\r\n"},
		{name: "crlf_with_explicit_carriage_return", pattern: `ab\r$`, input: "xab\r\n", want: [][2]int{{1, 4}}},
		{name: "double_newline_near_miss", pattern: `ab$`, input: "xab\n\n"},
		{name: "consume_first_of_two_final_newlines", pattern: `ab\n$`, input: "xab\n\n", want: [][2]int{{1, 4}}},
		{name: "first_prefix_fails_second_candidate_matches", pattern: `ab\n$`, input: "xab\n", want: [][2]int{{1, 4}}},
		{name: "first_prefix_matches_but_following_class_fails", pattern: `aa[ \n]$`, input: "aaa\n", want: [][2]int{{1, 4}}},
		{name: "first_prefix_matches_but_lookahead_fails", pattern: `a(?=\n)\n$`, input: "aa\n", want: [][2]int{{1, 3}}},
		{name: "too_short_before_final_newline", pattern: `ab$`, input: "a\n"},
		{name: "empty_input_absolute_end", pattern: `\z`, input: "", want: [][2]int{{0, 0}}},
		{name: "variable_length_repetition", pattern: `a+b$`, input: "xaaab\n", want: [][2]int{{1, 5}}},
		{name: "different_length_alternatives", pattern: `(?:a|bc)$`, input: "xbc\n", want: [][2]int{{1, 3}}},
		{name: "multiline_matches_each_line", pattern: `(?m)ab$`, input: "ab\nxxab\nab", want: [][2]int{{0, 2}, {5, 7}, {8, 10}}},
		{name: "absolute_beginning_and_end", pattern: `\Aab$`, input: "ab\n", want: [][2]int{{0, 2}}},
		{name: "leading_anchor_rejects_later_suffix", pattern: `^ab$`, input: "xab\n"},
		{name: "lookbehind_before_fixed_suffix", pattern: `(?<=界)ab$`, input: "x界ab\n", want: [][2]int{{2, 4}}},
		{name: "negative_lookbehind_rejects_suffix", pattern: `(?<!x)ab$`, input: "xab\n"},
		{name: "assertion_after_end_anchor", pattern: `ab$(?!\n)`, input: "xab\n"},
		{name: "unicode_case_insensitive_suffix", pattern: `(?i)äb$`, input: "xÄB\n", want: [][2]int{{1, 3}}},
		{name: "re2_requires_absolute_end", pattern: `ab$`, input: "xab\n", options: []CompileOption{RE2}},
		{name: "ecmascript_requires_absolute_end", pattern: `ab$`, input: "xab\n", options: []CompileOption{ECMAScript}},
		{name: "right_to_left_final_newline", pattern: `ab$`, input: "abxxab\n", options: []CompileOption{RightToLeft}, want: [][2]int{{4, 6}}},
		{name: "right_to_left_multiline_order", pattern: `(?m)ab$`, input: "ab\nxxab\nab", options: []CompileOption{RightToLeft}, want: [][2]int{{8, 10}, {5, 7}, {0, 2}}},

		// Character classes and bounded repetition.
		{
			name: "enumerated_minimum_hit", pattern: `[ACEGIK]{2,4}`,
			input: "zACE!IKz", want: [][2]int{{1, 4}, {5, 7}},
		},
		{
			name: "enumerated_minimum_miss", pattern: `[ACEGIK]{2,4}`,
			input: "A!C!E!G!I!K",
		},
		{
			name: "bounded_maximum_splits_run", pattern: `[ACEGIK]{2,4}`,
			input: "ACEGIKACEG", want: [][2]int{{0, 4}, {4, 8}, {8, 10}},
		},
		{
			name: "bounded_maximum_resumes_for_suffix", pattern: `[ACEGIK]{2,4}Z`,
			input: "ACEGIKZ", want: [][2]int{{2, 7}},
		},
		{
			name: "bounded_run_missing_suffix", pattern: `[ACEGIK]{2,4}Z`,
			input: "ACEGIKY",
		},
		{
			name: "set_at_fixed_distance", pattern: `..[ACEGIK]Z`,
			input: "xxBZ!yyCZ", want: [][2]int{{5, 9}},
		},
		{
			name: "fixed_distance_literal_and_set", pattern: `.x[ACEGIK]Z`,
			input: "axBZ-bxCZ", want: [][2]int{{5, 9}},
		},
		{
			name: "multiple_fixed_distance_sets", pattern: `[ACEGIK].[BDFHJL]`,
			input: "A?Z-K!L-E_F", want: [][2]int{{4, 7}, {8, 11}},
		},
		{
			name: "negated_ascii_accepts_unicode", pattern: `[^ACEGIK]{2,4}`,
			input: "ACxyEG!界IK", want: [][2]int{{2, 4}, {6, 8}},
		},
		{
			name: "negated_ascii_minimum_miss", pattern: `[^ACEGIK]{2,4}`,
			input: "AxCEG!IK",
		},
		{
			name: "unicode_enumeration", pattern: `[αβγδεζ]{2,3}`,
			input: "xαβγδεζy", want: [][2]int{{1, 4}, {4, 7}},
		},
		{
			name: "negated_unicode_accepts_ascii_and_astral", pattern: `[^αβγδεζ]{2,3}`,
			input: "αabβ界🙂γ", want: [][2]int{{1, 3}, {4, 6}},
		},
		{
			name: "mixed_ascii_unicode_enumeration", pattern: `[ACEαβγ]{2,4}`,
			input: "xAαβE!γCy", want: [][2]int{{1, 5}, {6, 8}},
		},
		{
			name: "ascii_class_subtraction", pattern: `[A-Z-[AEIOU]]{2,4}`,
			input: "ABCDExFGHIZ", want: [][2]int{{1, 4}, {6, 9}},
		},
		{
			name: "class_subtraction_minimum_miss", pattern: `[A-Z-[AEIOU]]{2,4}`,
			input: "AEIOUBAEIOU",
		},
		{
			name: "unicode_class_subtraction", pattern: `[αβγδεζηθ-[βδζθ]]{2,3}`,
			input: "βαγεδζηαγ", want: [][2]int{{1, 4}, {6, 9}},
		},
		{
			name: "ascii_ignore_case", pattern: `[ACEGIK]{2,4}`,
			input: "xace!gIkz", options: []CompileOption{IgnoreCase},
			want: [][2]int{{1, 4}, {5, 8}},
		},
		{
			name: "unicode_ignore_case", pattern: `[ΑΒΓΔΕΖ]{2,3}`,
			input: "xαΒγ!δεΖy", options: []CompileOption{IgnoreCase},
			want: [][2]int{{1, 4}, {5, 8}},
		},
		{
			name: "right_to_left_bounded_run", pattern: `[ACEGIK]{2,4}`,
			input: "zACEGIKz", options: []CompileOption{RightToLeft},
			want: [][2]int{{3, 7}, {1, 3}},
		},
		{
			name: "right_to_left_fixed_distance", pattern: `A[BCDEFG]{2}Z`,
			input: "ABCZ!ADEZ", options: []CompileOption{RightToLeft},
			want: [][2]int{{5, 9}, {0, 4}},
		},
		{
			name: "enumerated_bitmap_disabled", pattern: `[ACEGIK]{2,4}`,
			input: "zACE!IKz", options: []CompileOption{OptionDisableCharClassASCIIBitmap()},
			want: [][2]int{{1, 4}, {5, 7}},
		},
		{
			name: "negated_bitmap_disabled", pattern: `[^ACEGIK]{2,4}`,
			input: "ACxyEG!界IK", options: []CompileOption{OptionDisableCharClassASCIIBitmap()},
			want: [][2]int{{2, 4}, {6, 8}},
		},
		{
			name: "negated_set_accepts_newline", pattern: `[^ACEGIK]{2,3}`,
			input: "A\n\tC12", want: [][2]int{{1, 3}, {4, 6}},
		},
		{
			name: "nul_in_enumerated_set", pattern: `[\x00ACEGI]{2,3}`,
			input: "x\x00AC!\x00G", want: [][2]int{{1, 4}, {5, 7}},
		},
	}
	for _, n := range []int{0, 1, 31, 63, 64, 65, 128} {
		for _, fill := range []string{"a", "x"} {
			cases = append(cases, indexCase{name: fmt.Sprintf("padding_%d_%s", n, fill), pattern: `aaba|aaca|bada`, input: strings.Repeat(fill, n) + "aaca", want: [][2]int{{n, n + 4}}})
		}
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			re, err := Compile(tc.pattern, tc.options...)
			if err != nil {
				t.Fatal(err)
			}
			runes := []rune(tc.input)
			byteOffsets := make([]int, 0, len(runes)+1)
			for offset := range tc.input {
				byteOffsets = append(byteOffsets, offset)
			}
			byteOffsets = append(byteOffsets, len(tc.input))
			for _, span := range tc.want {
				if span[0] < 0 || span[0] > span[1] || span[1] > len(runes) {
					t.Fatalf("invalid expected span %v for %q", span, tc.input)
				}
			}
			for _, mode := range []string{"string", "runes"} {
				t.Run(mode, func(t *testing.T) {
					var matched bool
					var err error
					if mode == "string" {
						matched, err = re.MatchString(tc.input)
					} else {
						matched, err = re.MatchRunes(runes)
					}
					if err != nil || matched != (len(tc.want) > 0) {
						t.Fatalf("Match: got %v, %v; want %v", matched, err, len(tc.want) > 0)
					}
					for _, limit := range []int{-1, 0, 1} {
						var got [][]int
						if mode == "string" {
							got, err = re.FindAllStringIndex(tc.input, limit)
						} else {
							got, err = re.FindAllRunesIndex(runes, limit)
						}
						want := tc.want
						if limit >= 0 && limit < len(want) {
							want = want[:limit]
						}
						if err != nil || len(got) != len(want) {
							t.Fatalf("FindAllIndex limit %d: got %v, %v; want %v", limit, got, err, want)
						}
						if limit == 0 && got != nil {
							t.Fatalf("FindAllIndex limit 0: got %v, want nil", got)
						}
						for i, span := range want {
							if mode == "string" {
								span = [2]int{byteOffsets[span[0]], byteOffsets[span[1]]}
							}
							if !slices.Equal(got[i], span[:]) {
								t.Fatalf("FindAllIndex limit %d, match %d: got %v, want %v", limit, i, got[i], span)
							}
						}
					}
					var m *Match
					if mode == "string" {
						m, err = re.FindStringMatch(tc.input)
					} else {
						m, err = re.FindRunesMatch(runes)
					}
					for i, span := range tc.want {
						if err != nil || m == nil {
							t.Fatalf("match %d: got %v, %v; want span %v", i, m, err, span)
						}
						if m.RuneIndex != span[0] || m.RuneLength != span[1]-span[0] {
							t.Fatalf("match %d: got rune span [%d,%d), want %v", i, m.RuneIndex, m.RuneIndex+m.RuneLength, span)
						}
						wantText := string(runes[span[0]:span[1]])
						if mode == "string" {
							wantText = tc.input[byteOffsets[span[0]]:byteOffsets[span[1]]]
							start, length := m.ByteRange()
							if start != byteOffsets[span[0]] || start+length != byteOffsets[span[1]] {
								t.Fatalf("match %d: unexpected byte range [%d,%d)", i, start, start+length)
							}
						}
						if m.String() != wantText || !slices.Equal(m.Runes(), runes[span[0]:span[1]]) {
							t.Fatalf("match %d: got text %q, want %q", i, m.String(), wantText)
						}
						m, err = re.FindNextMatch(m)
					}
					if err != nil || m != nil {
						t.Fatalf("unexpected additional match %v, %v", m, err)
					}
				})
			}
		})
	}
}

func TestFindAllRunesIndex(t *testing.T) {
	for _, tc := range []struct {
		name, pattern string
		input         []rune
		want          [][2]int
	}{
		{"unicode_captures", `é(.)`, []rune("éxéy"), [][2]int{{0, 2}, {2, 4}}},
		{"invalid_prefix_boundary", `aaba|aaca|bada`, append(append([]rune(strings.Repeat("a", 80)), -1, 0x110000), []rune("aaca")...), [][2]int{{82, 86}}},
		{"invalid_negated_set", `[^ACEGIK]{2}`, []rune{'A', -1, 0x110000, 'C'}, [][2]int{{1, 3}}},
		{"invalid_casefold_boundary", `(?i)ab`, []rune{'a', -1, 'b', 0x110000, 'A', 'B'}, [][2]int{{4, 6}}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			re := MustCompile(tc.pattern)
			matched, err := re.MatchRunes(tc.input)
			if err != nil || matched != (len(tc.want) > 0) {
				t.Fatalf("MatchRunes = %v, %v", matched, err)
			}
			got, err := re.FindAllRunesIndex(tc.input, -1)
			if err != nil || len(got) != len(tc.want) {
				t.Fatalf("FindAllRunesIndex = %v, %v; want %v", got, err, tc.want)
			}
			m, err := re.FindRunesMatch(tc.input)
			for i, span := range tc.want {
				if !slices.Equal(got[i], span[:]) {
					t.Fatalf("index %d = %v; want %v", i, got[i], span)
				}
				if err != nil || m == nil {
					t.Fatalf("missing match %d: %v", i, err)
				}
				if m.RuneIndex != span[0] || m.RuneLength != span[1]-span[0] || !slices.Equal(m.Runes(), tc.input[span[0]:span[1]]) {
					t.Fatalf("unexpected match %d: %v", i, m)
				}
				m, err = re.FindNextMatch(m)
			}
			if err != nil || m != nil {
				t.Fatalf("unexpected additional match %v, %v", m, err)
			}
		})
	}
}

func TestFindAllIndexEmptyMatches(t *testing.T) {
	re := MustCompile(`x*`, RE2)
	got, err := re.FindAllStringIndex("ax", -1)
	if err != nil {
		t.Fatalf("FindAllStringIndex failed: %v", err)
	}
	want := [][]int{{0, 0}, {1, 2}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("FindAllStringIndex = %#v, want %#v", got, want)
	}
}

func TestFindAllStringIndexRequiredLandmarkChainEmail(t *testing.T) {
	re := MustCompile(`(?P<name>[-\w\d\.]+?)(?:\s+at\s+|\s*@\s*|\s*(?:[\[\]@]){3}\s*)(?P<host>[-\w\d\.]*?)\s*(?:dot|\.|(?:[\[\]dot\.]){3,5})\s*(?P<domain>\w+)`, RE2)
	input := "123@mail.co x user at example dot com y foo@@@bar...baz"
	got, err := re.FindAllStringIndex(input, -1)
	if err != nil {
		t.Fatalf("FindAllStringIndex failed: %v", err)
	}
	want := [][]int{{0, 11}, {14, 37}, {40, 55}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("FindAllStringIndex = %#v, want %#v", got, want)
	}
}

func TestRequiredLandmarkAlternativeMatchDoesNotRewindTrailingWhitespace(t *testing.T) {
	input := []rune("  foo ")
	alt := syntax.RequiredLandmarkAlternative{
		Literal:                []rune("foo"),
		TrailingWhitespaceSet:  syntax.SpaceClass(),
		MinRepeat:              1,
		MaxRepeat:              1,
		RequireWhitespaceAfter: true,
	}

	got, ok := requiredLandmarkAlternativeMatch(input, 2, len(input), alt)
	if !ok {
		t.Fatal("requiredLandmarkAlternativeMatch failed")
	}
	want := requiredLandmarkMatch{Start: 2, CoreStart: 2, End: 5}
	if got != want {
		t.Fatalf("requiredLandmarkAlternativeMatch = %#v, want %#v", got, want)
	}
}

func TestUnicodeSupplementaryCharSetMatch(t *testing.T) {
	//0x2070E 0x20731 𠜱 0x20779 𠝹
	re := MustCompile("[𠜎-𠝹]")

	if m, err := re.MatchString("\u2070"); err != nil {
		t.Fatalf("Unexpected err: %v", err)
	} else if m {
		t.Fatalf("Unexpected match")
	}

	if m, err := re.MatchString("𠜱"); err != nil {
		t.Fatalf("Unexpected err: %v", err)
	} else if !m {
		t.Fatalf("Expected match")
	}
}

func TestUnicodeSupplementaryCharInRange(t *testing.T) {
	//0x2070E 0x20731 𠜱 0x20779 𠝹
	re := MustCompile(".")

	if m, err := re.MatchString("\u2070"); err != nil {
		t.Fatalf("Unexpected err: %v", err)
	} else if !m {
		t.Fatalf("Expected match")
	}

	if m, err := re.MatchString("𠜱"); err != nil {
		t.Fatalf("Unexpected err: %v", err)
	} else if !m {
		t.Fatalf("Expected match")
	}
}

func TestUnicodeScriptSets(t *testing.T) {
	re := MustCompile(`\p{Katakana}+`)
	if m, err := re.MatchString("\u30A0\u30FF"); err != nil {
		t.Fatalf("Unexpected err: %v", err)
	} else if !m {
		t.Fatalf("Expected match")
	}
}

func TestUnicodePropertyAliases(t *testing.T) {
	tests := []struct {
		pattern string
		input   string
	}{
		{`\p{Math}`, "⋿"},
		{`\p{Emoji}`, "\u23E9"},
		{`\p{emoji}`, "\U0001F21A"},
		{`\p{EPres}`, "\u231A"},
		{`\p{extendedpictographic}`, "\U0001FA6E"},
		{`\p{ExtPict}`, "\U0001FA6E"},
		{`\p{grapheme_cluster_break=prepend}`, "\U00011D46"},
		{`\p{gcb=ri}`, "\U0001F1E7"},
		{`\p{GCB=LVT}`, "\uC989"},
		{`\p{regionalindicator}`, "\U0001F1FF"},
		{`\p{word_break=Hebrew_Letter}`, "\uFB46"},
		{`\p{wb=ExtendNumLet}`, "\uFF3F"},
		{`\p{sentence_break=Lower}`, "\u0469"},
		{`\p{sb=SContinue}`, "\uFF64"},
	}

	for _, tt := range tests {
		t.Run(tt.pattern, func(t *testing.T) {
			re := MustCompile(tt.pattern)
			if m, err := re.MatchString(tt.input); err != nil {
				t.Fatalf("Unexpected err: %v", err)
			} else if !m {
				t.Fatalf("Expected %q to match %q", tt.input, tt.pattern)
			}
		})
	}
}

func TestHexadecimalCurlyBraces(t *testing.T) {
	re := MustCompile(`\x20`)
	if m, err := re.MatchString(" "); err != nil {
		t.Fatalf("Unexpected err: %v", err)
	} else if !m {
		t.Fatalf("Expected match")
	}

	re = MustCompile(`\x{C4}`)
	if m, err := re.MatchString("Ä"); err != nil {
		t.Fatalf("Unexpected err: %v", err)
	} else if !m {
		t.Fatalf("Expected match")
	}

	re = MustCompile(`\x{0C5}`)
	if m, err := re.MatchString("Å"); err != nil {
		t.Fatalf("Unexpected err: %v", err)
	} else if !m {
		t.Fatalf("Expected match")
	}

	re = MustCompile(`\x{00C6}`)
	if m, err := re.MatchString("Æ"); err != nil {
		t.Fatalf("Unexpected err: %v", err)
	} else if !m {
		t.Fatalf("Expected match")
	}

	re = MustCompile(`\x{1FF}`)
	if m, err := re.MatchString("ǿ"); err != nil {
		t.Fatalf("Unexpected err: %v", err)
	} else if !m {
		t.Fatalf("Expected match")
	}

	re = MustCompile(`\x{02FF}`)
	if m, err := re.MatchString("˿"); err != nil {
		t.Fatalf("Unexpected err: %v", err)
	} else if !m {
		t.Fatalf("Expected match")
	}

	re = MustCompile(`\x{1392}`)
	if m, err := re.MatchString("᎒"); err != nil {
		t.Fatalf("Unexpected err: %v", err)
	} else if !m {
		t.Fatalf("Expected match")
	}

	re = MustCompile(`\x{0010ffff}`)
	if m, err := re.MatchString(string(rune(0x10ffff))); err != nil {
		t.Fatalf("Unexpected err: %v", err)
	} else if !m {
		t.Fatalf("Expected match")
	}

	if _, err := Compile(`\x2R`); err == nil {
		t.Fatal("Expected error")
	}
	if _, err := Compile(`\x0`); err == nil {
		t.Fatal("Expected error")
	}
	if _, err := Compile(`\x`); err == nil {
		t.Fatal("Expected error")
	}
	if _, err := Compile(`\x{`); err == nil {
		t.Fatal("Expected error")
	}
	if _, err := Compile(`\x{2`); err == nil {
		t.Fatal("Expected error")
	}
	if _, err := Compile(`\x{2R`); err == nil {
		t.Fatal("Expected error")
	}
	if _, err := Compile(`\x{2R}`); err == nil {
		t.Fatal("Expected error")
	}
	if _, err := Compile(`\x{}`); err == nil {
		t.Fatalf("Expected error")
	}
	if _, err := Compile(`\x{10000`); err == nil {
		t.Fatal("Expected error")
	}
	if _, err := Compile(`\x{1234`); err == nil {
		t.Fatal("Expected error")
	}
	if _, err := Compile(`\x{123456789}`); err == nil {
		t.Fatal("Expected error")
	}

}

func TestEmptyCharClass(t *testing.T) {
	if _, err := Compile("[]"); err == nil {
		t.Fatal("Empty char class isn't valid outside of ECMAScript mode")
	}
}

func TestECMAEmptyCharClass(t *testing.T) {
	re := MustCompile("[]", ECMAScript)
	if m, err := re.MatchString("a"); err != nil {
		t.Fatal(err)
	} else if m {
		t.Fatal("Expected no match")
	}
}

func TestDot(t *testing.T) {
	re := MustCompile(".")
	if m, err := re.MatchString("\r"); err != nil {
		t.Fatal(err)
	} else if !m {
		t.Fatal("Expected match")
	}
}

func TestECMADot(t *testing.T) {
	re := MustCompile(".", ECMAScript)
	if m, err := re.MatchString("\r"); err != nil {
		t.Fatal(err)
	} else if m {
		t.Fatal("Expected no match")
	}
}

func TestDecimalLookahead(t *testing.T) {
	re := MustCompile(`\1(A)`)
	m, err := re.FindStringMatch("AA")
	if err != nil {
		t.Fatal(err)
	} else if m != nil {
		t.Fatal("Expected no match")
	}
}

func TestECMADecimalLookahead(t *testing.T) {
	re := MustCompile(`\1(A)`, ECMAScript)
	m, err := re.FindStringMatch("AA")
	if err != nil {
		t.Fatal(err)
	}

	if c := m.GroupCount(); c != 2 {
		t.Fatalf("Group count !=2 (%d)", c)
	}

	if s := m.GroupByNumber(0).String(); s != "A" {
		t.Fatalf("Group0 != 'A' ('%s')", s)
	}

	if s := m.GroupByNumber(1).String(); s != "A" {
		t.Fatalf("Group1 != 'A' ('%s')", s)
	}
}

func TestECMAOctal(t *testing.T) {
	re := MustCompile(`\100`, ECMAScript)
	if m, err := re.MatchString("@"); err != nil {
		t.Fatal(err)
	} else if !m {
		t.Fatal("Expected match")
	}

	if m, err := re.MatchString("x"); err != nil {
		t.Fatal(err)
	} else if m {
		t.Fatal("Expected no match")
	}

	re = MustCompile(`\377`, ECMAScript)
	if m, err := re.MatchString("\u00ff"); err != nil {
		t.Fatal(err)
	} else if !m {
		t.Fatal("Expected match")
	}

	re = MustCompile(`\400`, ECMAScript)
	if m, err := re.MatchString(" 0"); err != nil {
		t.Fatal(err)
	} else if !m {
		t.Fatal("Expected match")
	}

}

func TestECMAInvalidEscape(t *testing.T) {
	re := MustCompile(`\x0`, ECMAScript)
	if m, err := re.MatchString("x0"); err != nil {
		t.Fatal(err)
	} else if !m {
		t.Fatal("Expected match")
	}

	re = MustCompile(`\x0z`, ECMAScript)
	if m, err := re.MatchString("x0z"); err != nil {
		t.Fatal(err)
	} else if !m {
		t.Fatal("Expected match")
	}
}

func TestECMANamedGroup(t *testing.T) {
	re := MustCompile(`\k`, ECMAScript)
	if m, err := re.MatchString("k"); err != nil {
		t.Fatal(err)
	} else if !m {
		t.Fatal("Expected match")
	}

	re = MustCompile(`\k'test'`, ECMAScript)
	if m, err := re.MatchString(`k'test'`); err != nil {
		t.Fatal(err)
	} else if !m {
		t.Fatal("Expected match")
	}

	re = MustCompile(`\k<test>`, ECMAScript)
	if m, err := re.MatchString(`k<test>`); err != nil {
		t.Fatal(err)
	} else if !m {
		t.Fatal("Expected match")
	}

	_, err := Compile(`(?<title>\w+), yes \k'title'`, ECMAScript)
	if err == nil {
		t.Fatal("Expected error")
	}

	re = MustCompile(`(?<title>\w+), yes \k<title>`, ECMAScript)
	if m, err := re.MatchString("sir, yes sir"); err != nil {
		t.Fatal(err)
	} else if !m {
		t.Fatal("Expected match")
	}

	re = MustCompile(`\k<title>, yes (?<title>\w+)`, ECMAScript)
	if m, err := re.MatchString(", yes sir"); err != nil {
		t.Fatal(err)
	} else if !m {
		t.Fatal("Expected match")
	}

	_, err = Compile(`\k<(?<name>)>`, ECMAScript)
	if err == nil {
		t.Fatal("Expected error")
	}

	MustCompile(`\k<(<name>)>`, ECMAScript)

	_, err = Compile(`\k<(<name>)>`)
	if err == nil {
		t.Fatal("Expected error")
	}

	re = MustCompile(`\'|\<?`)
	if m, err := re.MatchString("'"); err != nil {
		t.Fatal(err)
	} else if !m {
		t.Fatal("Expected match")
	}
	if m, err := re.MatchString("<"); err != nil {
		t.Fatal(err)
	} else if !m {
		t.Fatal("Expected match")
	}
}

func TestECMAGroupNameUnicode(t *testing.T) {
	t.Run("unicode-escape", func(t *testing.T) {
		const expr = `(?<\u03C0>a)`
		re := MustCompile(expr, ECMAScript)
		if want, got := []string{"", "\u03C0"}, re.GetGroupNames(); !stringSlicesEqual(want, got) {
			t.Fatalf("Group names = %v, want %v", got, want)
		}
		if _, err := Compile(expr); err == nil {
			t.Fatal("Expected error without ECMAScript")
		}
	})

	t.Run("extended-unicode-escape", func(t *testing.T) {
		const expr = `(?<\u{03C0}>a)`
		re := MustCompile(expr, ECMAScript|Unicode)
		if want, got := []string{"", "\u03C0"}, re.GetGroupNames(); !stringSlicesEqual(want, got) {
			t.Fatalf("Group names = %v, want %v", got, want)
		}
		m, err := re.FindStringMatch("bab")
		if err != nil {
			t.Fatal(err)
		}
		if m == nil {
			t.Fatal("Expected match")
		}
		if s := m.GroupByName("\u03C0").String(); s != "a" {
			t.Fatalf("GroupByName(pi) = %q, want %q", s, "a")
		}
	})

	t.Run("extended-unicode-escape-without-unicode-option", func(t *testing.T) {
		re := MustCompile(`(?<\u{1D49C}>a)`, ECMAScript)
		if want, got := []string{"", "\U0001D49C"}, re.GetGroupNames(); !stringSlicesEqual(want, got) {
			t.Fatalf("Group names = %v, want %v", got, want)
		}
	})

	t.Run("surrogate-pair-escape", func(t *testing.T) {
		for _, opt := range []RegexOptions{ECMAScript, ECMAScript | Unicode} {
			re := MustCompile(`(?<a\uD835\uDC9C>a)`, opt)
			if want, got := []string{"", "a\U0001D49C"}, re.GetGroupNames(); !stringSlicesEqual(want, got) {
				t.Fatalf("Group names = %v, want %v", got, want)
			}
		}
	})

	t.Run("lone-surrogate-escape", func(t *testing.T) {
		for _, expr := range []string{`(?<\uD835>a)`, `(?<\uD835\u0041>a)`, `(?<\uDC9C>a)`} {
			if _, err := Compile(expr, ECMAScript); err == nil {
				t.Fatalf("Expected error: %s", expr)
			}
		}
	})

	t.Run("invalid-escape-x", func(t *testing.T) {
		if _, err := Compile(`(?<\x68>a)`, ECMAScript); err == nil {
			t.Fatal("Expected error")
		}
	})

	t.Run("invalid-escape-u", func(t *testing.T) {
		if _, err := Compile(`(?<\ubob>a)`, ECMAScript); err == nil {
			t.Fatal("Expected error")
		}
	})

	t.Run("invalid-start-char", func(t *testing.T) {
		if _, err := Compile(`(?<\u0030>a)`, ECMAScript); err == nil {
			t.Fatal("Expected error")
		}
	})

	t.Run("invalid-identifier", func(t *testing.T) {
		if _, err := Compile(`(?<\u{1F98A}>fox)`, ECMAScript|Unicode); err == nil {
			t.Fatal("Expected error")
		}
	})
}

func TestECMAQuantifiedAssertion(t *testing.T) {
	for _, expr := range []string{`^*`, `a$+`, `\b{2}`, `\B?`, `(?=a)*`, `(?!a){1,}`} {
		if _, err := Compile(expr, ECMAScript|Unicode); err == nil {
			t.Fatalf("%s: expected error", expr)
		}
	}
	for _, expr := range []string{`^*`, `a$+`, `\b{2}`, `\B?`} {
		if _, err := Compile(expr, ECMAScript); err == nil {
			t.Fatalf("%s: expected error", expr)
		}
	}
	for _, expr := range []string{`(?=a)*`, `(?!a)?`, `(?:^)*`, `^{`, `\b{a}`} {
		MustCompile(expr, ECMAScript)
	}
}

func TestECMAGroupNameIdentifierChars(t *testing.T) {
	for _, tc := range []struct {
		expr string
		name string
	}{
		{`(?<$>a)`, "$"},
		{`(?<_>a)`, "_"},
	} {
		re := MustCompile(tc.expr, ECMAScript)
		m, err := re.FindStringMatch("bab")
		if err != nil {
			t.Fatal(err)
		}
		if m == nil {
			t.Fatalf("%s: expected match", tc.expr)
		}
		if s := m.GroupByName(tc.name).String(); s != "a" {
			t.Fatalf("%s: GroupByName(%q) = %q, want %q", tc.expr, tc.name, s, "a")
		}
	}
}

func TestECMADuplicateGroupNames(t *testing.T) {
	for _, expr := range []string{
		`(?<a>a)(?<a>a)`,
	} {
		if _, err := Compile(expr, ECMAScript); err == nil {
			t.Fatalf("%s: expected duplicate group name error", expr)
		}
		if _, err := Compile(expr); err != nil {
			t.Fatalf("%s: non-ECMAScript behavior changed: %v", expr, err)
		}
	}
	// Issue #116 identified this formerly rejected pattern. Identical branch
	// text is allowed by ECMAScript 2025 §22.2.1.4 MightBothParticipate and
	// must retain distinct group numbers; the first alternative wins.
	// https://github.com/dlclark/regexp2/issues/116
	for _, options := range []RegexOptions{ECMAScript, ECMAScript | Unicode} {
		re := MustCompile(`(?<a>a)|(?<a>a)`, options)
		m, err := re.FindStringMatch("a")
		if err != nil || m == nil {
			t.Fatalf("identical alternatives: FindStringMatch = %v, %v", m, err)
		}
		if m.GroupCount() != 3 || m.GroupByName("a") != m.GroupByNumber(1) || m.GroupByNumber(1).String() != "a" {
			t.Fatal("identical alternatives did not retain distinct groups and select the first")
		}
		if group := m.GroupByNumber(2); group.Name != "a" || len(group.Captures) != 0 {
			t.Fatal("second identical alternative should be an unmatched group named a")
		}
	}
}

func TestECMANamedGroupNumberAssignment(t *testing.T) {
	re := MustCompile(`(.)(?<x>a)(?<y>\1)(\k<x>)`, ECMAScript)
	m, err := re.FindStringMatch("baba")
	if err != nil {
		t.Fatal(err)
	}
	if m == nil {
		t.Fatal("Expected match")
	}

	groups := m.Groups()
	if len(groups) != 5 {
		t.Fatalf("len(Groups()) = %d, want 5", len(groups))
	}
	for i, want := range []struct {
		name  string
		index int
		text  string
	}{
		{"", 0, "baba"},
		{"", 0, "b"},
		{"x", 1, "a"},
		{"y", 2, "b"},
		{"", 3, "a"},
	} {
		if groups[i].Name != want.name || groups[i].RuneIndex != want.index || groups[i].String() != want.text {
			t.Fatalf("Groups()[%d] = {Name:%q RuneIndex:%d String:%q}, want {Name:%q RuneIndex:%d String:%q}",
				i, groups[i].Name, groups[i].RuneIndex, groups[i].String(), want.name, want.index, want.text)
		}
	}

	if want, got := []string{"", "", "x", "y", ""}, re.GetGroupNames(); !stringSlicesEqual(want, got) {
		t.Fatalf("Group names = %v, want %v", got, want)
	}
	if num := re.GroupNumberFromName("1"); num != -1 {
		t.Fatalf("GroupNumberFromName(\"1\") = %d, want -1", num)
	}
}

func TestECMAGroupByNumberRegression(t *testing.T) {
	if _, err := Compile(`(?<5>a)`, ECMAScript); err == nil {
		t.Fatal("ECMAScript accepted a numeric group name")
	}
	m, err := MustCompile(`(?<x>a)(b)`, ECMAScript).FindStringMatch("ab")
	if err != nil || m == nil {
		t.Fatalf("ECMAScript match = %v, %v", m, err)
	}
	if m.GroupByNumber(1).String() != "a" || m.GroupByNumber(2).String() != "b" || m.GroupByNumber(5) != nil {
		t.Fatal("incorrect ECMAScript group lookup")
	}
}

func TestNamedGroupNumberAssignmentNonECMAScript(t *testing.T) {
	re := MustCompile(`(.)(?<x>a)(?<y>\1)(\k<x>)`)
	m, err := re.FindStringMatch("baba")
	if err != nil {
		t.Fatal(err)
	}
	if m == nil {
		t.Fatal("Expected match")
	}

	groups := m.Groups()
	if len(groups) != 5 {
		t.Fatalf("len(Groups()) = %d, want 5", len(groups))
	}
	for i, want := range []struct {
		name  string
		index int
		text  string
	}{
		{"0", 0, "baba"},
		{"1", 0, "b"},
		{"2", 3, "a"},
		{"x", 1, "a"},
		{"y", 2, "b"},
	} {
		if groups[i].Name != want.name || groups[i].RuneIndex != want.index || groups[i].String() != want.text {
			t.Fatalf("Groups()[%d] = {Name:%q RuneIndex:%d String:%q}, want {Name:%q RuneIndex:%d String:%q}",
				i, groups[i].Name, groups[i].RuneIndex, groups[i].String(), want.name, want.index, want.text)
		}
	}

	if want, got := []string{"0", "1", "2", "x", "y"}, re.GetGroupNames(); !stringSlicesEqual(want, got) {
		t.Fatalf("Group names = %v, want %v", got, want)
	}
	if num := re.GroupNumberFromName("1"); num != 1 {
		t.Fatalf("GroupNumberFromName(\"1\") = %d, want 1", num)
	}
}

func TestECMAInvalidEscapeCharClass(t *testing.T) {
	re := MustCompile(`[\x0]`, ECMAScript)
	if m, err := re.MatchString("x"); err != nil {
		t.Fatal(err)
	} else if !m {
		t.Fatal("Expected match")
	}

	if m, err := re.MatchString("0"); err != nil {
		t.Fatal(err)
	} else if !m {
		t.Fatal("Expected match")
	}

	if m, err := re.MatchString("z"); err != nil {
		t.Fatal(err)
	} else if m {
		t.Fatal("Expected no match")
	}
}

func TestECMAScriptXCurlyBraceEscape(t *testing.T) {
	re := MustCompile(`\x{20}`, ECMAScript)
	if m, err := re.MatchString(" "); err != nil {
		t.Fatal(err)
	} else if m {
		t.Fatal("Expected no match")
	}

	if m, err := re.MatchString("xxxxxxxxxxxxxxxxxxxx"); err != nil {
		t.Fatal(err)
	} else if !m {
		t.Fatal("Expected match")
	}
}

func TestEcmaScriptUnicodeRange(t *testing.T) {
	r, err := Compile(`([\u{001a}-\u{ffff}]+)`, ECMAScript|Unicode)
	if err != nil {
		panic(err)
	}
	m, err := r.FindStringMatch("qqqq")
	if err != nil {
		panic(err)
	}
	if m == nil {
		t.Fatal("Expected non-nil, got nil")
	}
}

func TestNegateRange(t *testing.T) {
	re := MustCompile(`[\D]`)
	if m, err := re.MatchString("A"); err != nil {
		t.Fatal(err)
	} else if !m {
		t.Fatal("Expected match")
	}
}

func TestECMANegateRange(t *testing.T) {
	re := MustCompile(`[\D]`, ECMAScript)
	if m, err := re.MatchString("A"); err != nil {
		t.Fatal(err)
	} else if !m {
		t.Fatal("Expected match")
	}
}

func TestECMACharClassEscapeAfterDash(t *testing.T) {
	re := MustCompile(`[a-\s]`, ECMAScript)
	for _, input := range []string{"a", "-", " ", "\t"} {
		if m, err := re.MatchString(input); err != nil {
			t.Fatal(err)
		} else if !m {
			t.Fatalf("Expected match for %q", input)
		}
	}

	for _, input := range []string{"b", "_"} {
		if m, err := re.MatchString(input); err != nil {
			t.Fatal(err)
		} else if m {
			t.Fatalf("Expected no match for %q", input)
		}
	}

	if _, err := Compile(`[a-\s]`); err == nil {
		t.Fatal("Expected error without ECMAScript")
	}
}

func TestDollar(t *testing.T) {
	// PCRE/C# allow \n to match to $ at end-of-string in singleline mode...
	// a weird edge-case kept for compatibility, ECMAScript/RE2 mode don't allow it
	re := MustCompile(`ac$`)
	if m, err := re.MatchString("ac\n"); err != nil {
		t.Fatal(err)
	} else if !m {
		t.Fatal("Expected match")
	}
}
func TestECMADollar(t *testing.T) {
	re := MustCompile(`ac$`, ECMAScript)
	if m, err := re.MatchString("ac\n"); err != nil {
		t.Fatal(err)
	} else if m {
		t.Fatal("Expected no match")
	}
}

func TestThreeByteUnicode_InputOnly(t *testing.T) {
	// confirm the bmprefix properly ignores 3-byte unicode in the input value
	// this used to panic
	re := MustCompile("高")
	if m, err := re.MatchString("📍Test高"); err != nil {
		t.Fatal(err)
	} else if !m {
		t.Fatal("Expected match")
	}
}

func TestMultibyteUnicode_MatchPartialPattern(t *testing.T) {
	re := MustCompile("猟な")
	if m, err := re.MatchString("なあ🍺な"); err != nil {
		t.Fatal(err)
	} else if m {
		t.Fatal("Expected no match")
	}
}

func TestMultibyteUnicode_Match(t *testing.T) {
	re := MustCompile("猟な")
	if m, err := re.MatchString("なあ🍺猟な"); err != nil {
		t.Fatal(err)
	} else if !m {
		t.Fatal("Expected match")
	}
}

func TestAlternationNamedOptions_Errors(t *testing.T) {
	// all of these should give an error "error parsing regexp:"
	data := []string{
		"(?(?e))", "(?(?a)", "(?(?", "(?(", "?(a:b)", "?(a)", "?(a|b)", "?((a)", "?((a)a", "?((a)a|", "?((a)a|b",
		"(?(?i))", "(?(?I))", "(?(?m))", "(?(?M))", "(?(?s))", "(?(?S))", "(?(?x))", "(?(?X))", "(?(?n))", "(?(?N))", " (?(?n))",
	}
	for _, p := range data {
		re, err := Compile(p)
		if err == nil {
			t.Fatal("Expected error, got nil")
		}
		if re != nil {
			t.Fatal("Expected unparsed regexp, got non-nil")
		}

		if !strings.HasPrefix(err.Error(), "error parsing regexp: ") {
			t.Fatalf("Wanted parse error, got '%v'", err)
		}
	}
}

func TestAlternationNamedOptions_Success(t *testing.T) {
	data := []struct {
		pattern       string
		input         string
		expectSuccess bool
		matchVal      string
	}{
		{"(?(cat)|dog)", "cat", true, ""},
		{"(?(cat)|dog)", "catdog", true, ""},
		{"(?(cat)dog1|dog2)", "catdog1", false, ""},
		{"(?(cat)dog1|dog2)", "catdog2", true, "dog2"},
		{"(?(cat)dog1|dog2)", "catdog1dog2", true, "dog2"},
		{"(?(dog2))", "dog2", true, ""},
		{"(?(cat)|dog)", "oof", false, ""},
		{"(?(a:b))", "a", true, ""},
		{"(?(a:))", "a", true, ""},
	}
	for _, p := range data {
		re := MustCompile(p.pattern)
		m, err := re.FindStringMatch(p.input)

		if err != nil {
			t.Fatalf("Unexpected error during match: %v", err)
		}
		if want, got := p.expectSuccess, m != nil; want != got {
			t.Fatalf("Success mismatch for %v, wanted %v, got %v", p.pattern, want, got)
		}
		if m != nil {
			if want, got := p.matchVal, m.String(); want != got {
				t.Fatalf("Match val mismatch for %v, wanted %v, got %v", p.pattern, want, got)
			}
		}
	}
}

func TestAlternationConstruct_Matches(t *testing.T) {
	re := MustCompile("(?(A)A123|C789)")
	m, err := re.FindStringMatch("A123 B456 C789")
	if err != nil {
		t.Fatalf("Unexpected err: %v", err)
	}
	if m == nil {
		t.Fatal("Expected match, got nil")
	}

	if want, got := "A123", m.String(); want != got {
		t.Fatalf("Wanted %v, got %v", want, got)
	}

	m, err = re.FindNextMatch(m)
	if err != nil {
		t.Fatalf("Unexpected err in second match: %v", err)
	}
	if m == nil {
		t.Fatal("Expected second match, got nil")
	}
	if want, got := "C789", m.String(); want != got {
		t.Fatalf("Wanted %v, got %v", want, got)
	}

	m, err = re.FindNextMatch(m)
	if err != nil {
		t.Fatalf("Unexpected err in third match: %v", err)
	}
	if m != nil {
		t.Fatal("Did not expect third match")
	}
}

func TestGAnchor(t *testing.T) {
	re := MustCompile(`\Gfoo`)

	t.Run("match at origin", func(t *testing.T) {
		ok, err := re.MatchString("foo")
		if err != nil || !ok {
			t.Fatalf("MatchString(foo) = %v, %v", ok, err)
		}
		m, err := re.FindStringMatch("foo")
		if err != nil || m == nil {
			t.Fatalf("FindStringMatch(foo) = %v, %v", m, err)
		}
		if m.RuneIndex != 0 || m.String() != "foo" {
			t.Fatalf("got RuneIndex=%d String=%q", m.RuneIndex, m.String())
		}
	})

	t.Run("does not match later literal", func(t *testing.T) {
		for _, input := range []string{"xxfoo", "xxxxfoo", strings.Repeat("x", 100) + "foo"} {
			ok, err := re.MatchString(input)
			if err != nil || ok {
				t.Fatalf("MatchString(%q) = %v, %v", input, ok, err)
			}
			m, err := re.FindStringMatch(input)
			if err != nil {
				t.Fatal(err)
			}
			if m != nil {
				t.Fatalf("FindStringMatch(%q) = %q at %d, want no match", input, m.String(), m.RuneIndex)
			}
			idxs, err := re.FindAllStringIndex(input, -1)
			if err != nil {
				t.Fatal(err)
			}
			if len(idxs) != 0 {
				t.Fatalf("FindAllStringIndex(%q) = %v, want none", input, idxs)
			}
		}
	})

	t.Run("starting at origin of foo", func(t *testing.T) {
		m, err := re.FindStringMatchStartingAt("xxfoo", 2)
		if err != nil || m == nil {
			t.Fatalf("FindStringMatchStartingAt(xxfoo, 2) = %v, %v", m, err)
		}
		if m.RuneIndex != 2 || m.String() != "foo" {
			t.Fatalf("got RuneIndex=%d String=%q", m.RuneIndex, m.String())
		}
	})

	t.Run("starting before a later foo", func(t *testing.T) {
		m, err := re.FindStringMatchStartingAt("xxXfoo", 2)
		if err != nil {
			t.Fatal(err)
		}
		if m != nil {
			t.Fatalf("FindStringMatchStartingAt(xxXfoo, 2) = %q at %d, want no match", m.String(), m.RuneIndex)
		}
	})
}

func TestGAnchorFindNextMatch(t *testing.T) {
	re := MustCompile(`\G\w`)
	m, err := re.FindStringMatch("ab-c")
	if err != nil || m == nil || m.String() != "a" {
		t.Fatalf("first = %v, %v", m, err)
	}
	m, err = re.FindNextMatch(m)
	if err != nil || m == nil || m.RuneIndex != 1 || m.String() != "b" {
		t.Fatalf("second = %v, %v", m, err)
	}
	m, err = re.FindNextMatch(m)
	if err != nil {
		t.Fatal(err)
	}
	if m != nil {
		t.Fatalf("third = %q at %d, want no match across '-'", m.String(), m.RuneIndex)
	}
}

func TestStartAtEnd(t *testing.T) {
	re := MustCompile("(?:)")
	m, err := re.FindStringMatchStartingAt("t", 1)
	if err != nil {
		t.Fatal(err)
	}
	if m == nil {
		t.Fatal("Expected match")
	}
}

func TestParserFuzzCrashes(t *testing.T) {
	var crashes = []string{
		"(?'-", "(\\c0)", "(\\00(?())", "[\\p{0}", "(\x00?.*.()?(()?)?)*.x\xcb?&(\\s\x80)", "\\p{0}", "[0-[\\p{0}",
	}

	for _, c := range crashes {
		t.Log(c)
		_, _ = Compile(c)
	}
}

func TestParserFuzzHangs(t *testing.T) {
	var hangs = []string{
		"\r{865720113}z\xd5{\r{861o", "\r{915355}\r{9153}", "\r{525005}", "\x01{19765625}", "(\r{068828256})", "\r{677525005}",
	}

	for _, c := range hangs {
		t.Log(c)
		_, _ = Compile(c)
	}
}

func BenchmarkParserPrefixLongLen(b *testing.B) {
	re := MustCompile("\r{100001}T+")
	inp := strings.Repeat("testing", 10000) + strings.Repeat("\r", 100000) + "TTTT"

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if m, err := re.MatchString(inp); err != nil {
			b.Fatalf("Unexpected err: %v", err)
		} else if m {
			b.Fatalf("Expected no match")
		}
	}
}

/*
func TestPcreStuff(t *testing.T) {
	re := MustCompile(`(?(?=(a))a)`, OptionDebug())
	inp := unEscapeToMatch(`a`)
	fmt.Printf("Inp %q\n", inp)
	m, err := re.FindStringMatch(inp)

	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
	if m == nil {
		t.Fatalf("Expected match")
	}

	fmt.Printf("Match %s\n", m.dump())
	fmt.Printf("Text: %v\n", unEscapeGroup(m.String()))

}
*/

//(.*)(\d+) different FirstChars ([\x00-\t\v-\x08] OR [\x00-\t\v-\uffff\p{Nd}]

func TestControlBracketFail(t *testing.T) {
	re := MustCompile(`(cat)(\c[*)(dog)`)
	inp := "asdlkcat\u00FFdogiwod"

	if m, _ := re.MatchString(inp); m {
		t.Fatal("expected no match")
	}
}

func TestControlBracketGroups(t *testing.T) {
	re := MustCompile(`(cat)(\c[*)(dog)`)
	inp := "asdlkcat\u001bdogiwod"

	if want, got := 4, re.capsize; want != got {
		t.Fatalf("Capsize wrong, want %v, got %v", want, got)
	}

	m, _ := re.FindStringMatch(inp)
	if m == nil {
		t.Fatal("expected match")
	}

	g := m.Groups()
	want := []string{"cat\u001bdog", "cat", "\u001b", "dog"}
	for i := 0; i < len(g); i++ {
		if want[i] != g[i].String() {
			t.Fatalf("Bad group num %v, want %v, got %v", i, want[i], g[i].String())
		}
	}
}

func TestBadGroupConstruct(t *testing.T) {
	bad := []string{"(?>-", "(?<", "(?<=", "(?<!", "(?>", "(?)", "(?<)", "(?')", "(?<-"}

	for _, b := range bad {
		_, err := Compile(b)
		if err == nil {
			t.Fatalf("Wanted error, but got no error for pattern: %v", b)
		}
	}
}

func TestEmptyCaptureLargeRepeat(t *testing.T) {
	// a bug would cause our track to not grow and eventually panic
	// with large numbers of repeats of a non-capturing group (>16)

	// the issue was that the jump occured to the same statement over and over
	// and the "grow stack/track" logic only triggered on jumps that moved
	// backwards

	r := MustCompile(`(?:){40}`)
	m, err := r.FindStringMatch("1")
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
	if want, got := 0, m.RuneIndex; want != got {
		t.Errorf("First Match Index wanted %v got %v", want, got)
	}
	if want, got := 0, m.RuneLength; want != got {
		t.Errorf("First Match Length wanted %v got %v", want, got)
	}

	m, _ = r.FindNextMatch(m)
	if want, got := 1, m.RuneIndex; want != got {
		t.Errorf("Second Match Index wanted %v got %v", want, got)
	}
	if want, got := 0, m.RuneLength; want != got {
		t.Errorf("Second Match Length wanted %v got %v", want, got)
	}

	m, _ = r.FindNextMatch(m)
	if m != nil {
		t.Fatal("Expected 2 matches, got more")
	}
}

func TestFuzzBytes_NoCompile(t *testing.T) {
	//some crash cases found from fuzzing

	var testCases = []struct {
		r []byte
	}{
		{
			r: []byte{0x28, 0x28, 0x29, 0x5c, 0x37, 0x28, 0x3f, 0x28, 0x29, 0x29},
		},
		{
			r: []byte{0x28, 0x5c, 0x32, 0x28, 0x3f, 0x28, 0x30, 0x29, 0x29},
		},
		{
			r: []byte{0x28, 0x3f, 0x28, 0x29, 0x29, 0x5c, 0x31, 0x30, 0x28, 0x3f, 0x28, 0x30, 0x29},
		},
		{
			r: []byte{0x28, 0x29, 0x28, 0x28, 0x29, 0x5c, 0x37, 0x28, 0x3f, 0x28, 0x29, 0x29},
		},
	}

	for _, c := range testCases {
		r := string(c.r)
		t.Run(r, func(t *testing.T) {
			_, err := Compile(r, Multiline|ECMAScript, OptionDebug())
			// should fail compiling
			if err == nil {
				t.Fatal("should fail compile, but didn't")
			}
		})
	}

}

func TestFuzzBytes_Match(t *testing.T) {

	var testCases = []struct {
		r, s []byte
	}{
		{
			r: []byte{0x30, 0xbf, 0x30, 0x2a, 0x30, 0x30},
			s: []byte{0xf0, 0xb0, 0x80, 0x91, 0xf7},
		},
		{
			r: []byte{0x30, 0xaf, 0xf3, 0x30, 0x2a},
			s: []byte{0xf3, 0x80, 0x80, 0x87, 0x80, 0x89},
		},
	}

	for _, c := range testCases {
		r := string(c.r)
		t.Run(r, func(t *testing.T) {
			re, err := Compile(r)

			if err != nil {
				t.Fatal("should compile, but didn't")
			}

			_, _ = re.MatchString(string(c.s))
		})
	}
}

func TestIssue37FuzzBytes_NoPanic(t *testing.T) {
	// Regression test for #37: these fuzzed ECMAScript inputs used to panic.
	var testCases = []struct {
		r, s []byte
	}{
		{
			r: []byte{0x30, 0x28, 0x3f, 0x3e, 0x28, 0x29, 0x2b, 0x3f, 0x30, 0x29, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x77},
			s: []byte{0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30},
		},
		{
			r: []byte{0x28, 0x3f, 0x3e, 0x28, 0x3f, 0x3e, 0x29, 0x2b, 0x3f, 0x3e, 0x29, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30},
			s: []byte{0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x3e, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30},
		},
	}

	for _, c := range testCases {
		r := string(c.r)
		t.Run(r, func(t *testing.T) {
			re, err := Compile(r, ECMAScript)
			if err != nil {
				t.Fatalf("should compile, but got %v", err)
			}

			if _, err := re.FindStringMatch(string(c.s)); err != nil {
				t.Fatalf("unexpected match error: %v", err)
			}
		})
	}
}

func TestConcatAccidentalPatternCharge(t *testing.T) {
	// originally this pattern would parse incorrectly
	// specifically the closing group would concat the string literals
	// together but the raw rune slice would blow over the original pattern
	// so the final bit of pattern parsing would be wrong
	// fixed in #49
	r, err := Compile(`(?<=1234\.\*56).*(?=890)`)

	if err != nil {
		panic(err)
	}

	m, err := r.FindStringMatch(`1234.*567890`)
	if err != nil {
		panic(err)
	}
	if m == nil {
		t.Fatal("Expected non-nil, got nil")
	}
}

func TestGoodReverseOrderMessage(t *testing.T) {
	_, err := Compile(`[h-c]`, ECMAScript)
	if err == nil {
		t.Fatal("expected error")
	}
	expected := "error parsing regexp: [h-c] range in reverse order in `[h-c]`"
	if err.Error() != expected {
		t.Fatalf("expected %q got %q", expected, err.Error())
	}
}

func TestParseShortSlashP(t *testing.T) {
	re := MustCompile(`[!\pL\pN]{1,}`)
	m, err := re.FindStringMatch("this23! is a! test 1a 2b")
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
	if m.String() != "this23!" {
		t.Fatalf("Expected match")
	}
}

func TestParseShortSlashNegateP(t *testing.T) {
	re := MustCompile(`\PNa`)
	m, err := re.FindStringMatch("this is a test 1a 2b")
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
	if m.String() != " a" {
		t.Fatalf("Expected match")
	}
}

func TestParseShortSlashPEnd(t *testing.T) {
	re := MustCompile(`\pN`)
	m, err := re.FindStringMatch("this is a test 1a 2b")
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
	if m.String() != "1" {
		t.Fatalf("Expected match")
	}
}

func TestMarshal(t *testing.T) {
	re := MustCompile(`.*`)
	m, err := json.Marshal(re)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
	if string(m) != `".*"` {
		t.Fatalf(`Expected ".*"`)
	}
}

func TestUnMarshal(t *testing.T) {
	DefaultUnmarshalOptions = IgnoreCase
	bytes := []byte(`"^[abc]"`)
	var re *Regexp
	err := json.Unmarshal(bytes, &re)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
	if re.options != IgnoreCase {
		t.Fatalf("Expected options ignore case")
	}
	if re.String() != `^[abc]` {
		t.Fatalf(`Expected "^[abc]"`)
	}
	ok, err := re.MatchString("A")
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
	if !ok {
		t.Fatalf(`Expected match`)
	}
}

func TestRegexpECMAScriptWithSingleline(t *testing.T) {
	re := MustCompile(`.`, Singleline)
	if isMatch, _ := re.MatchString("\n"); !isMatch {
		t.Fatal("Expected match")
	}

	re = MustCompile(`.`, ECMAScript|Singleline)
	if isMatch, _ := re.MatchString("\n"); !isMatch {
		t.Fatal("Expected match")
	}
}

func TestIssue34LazyEmptyLoopFindNextMatchTerminates(t *testing.T) {
	re := MustCompile(`((?:0*)+?(?:.*)+?)?`)
	input := "0\xfd"

	type matchRange struct {
		index  int
		length int
	}

	var got []matchRange
	m, err := re.FindStringMatch(input)
	for i := 0; m != nil; i++ {
		if err != nil {
			t.Fatalf("unexpected match error: %v", err)
		}
		got = append(got, matchRange{index: m.RuneIndex, length: m.RuneLength})
		if i > len([]rune(input))+1 {
			t.Fatalf("FindNextMatch did not terminate; matches so far: %#v", got)
		}

		m, err = re.FindNextMatch(m)
	}
	if err != nil {
		t.Fatalf("unexpected match error: %v", err)
	}

	want := []matchRange{
		{index: 0, length: 2},
		{index: 2, length: 0},
	}
	if len(got) != len(want) {
		t.Fatalf("match count mismatch: want %#v, got %#v", want, got)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("match %d mismatch: want %#v, got %#v", i, want[i], got[i])
		}
	}
}

func TestAnythingSet(t *testing.T) {
	re := MustCompile(`(?ism)<(.+?)`)
	if m, _ := re.MatchString("<a"); !m {
		t.Fail()
	}
}

func TestRepeatPrefixNullableFixed(t *testing.T) {
	// a bug where the prefix optimization wasn't handling
	// the case where a repeated value followed by a nullable
	// followed by a fixed value wasn't handled correctly
	re := MustCompile(`(\d+)(-)?b.c`, OptionDebug())
	if m, _ := re.FindStringMatch("1-b.c"); m == nil {
		t.Fatal("Expected match")
	} else {
		g := m.Groups()
		if want, got := "1-b.c", g[0].String(); want != got {
			t.Fatalf("Group 0 Wanted %v, got %v", want, got)
		}
		if want, got := "1", g[1].String(); want != got {
			t.Fatalf("Group 1 Wanted %v, got %v", want, got)
		}
		if want, got := "-", g[2].String(); want != got {
			t.Fatalf("Group 2 Wanted %v, got %v", want, got)
		}
	}
}
