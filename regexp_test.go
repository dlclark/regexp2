package regexp2

import (
	"reflect"
	"testing"
	"time"
)

func TestBacktrack_CatastrophicTimeout(t *testing.T) {
	r, err := Compile("(.+)*\\?", 0)
	r.MatchTimeout = time.Millisecond * 1
	t.Logf("code dump: %v", r.code.Dump())
	m, err := r.FindStringMatch("Do you think you found the problem string!")
	if err == nil {
		t.Errorf("expected timeout err")
	}
	if m != nil {
		t.Errorf("Expected no match")
	}
}

func TestSetPrefix(t *testing.T) {
	r := MustCompile(`^\s*-TEST`, 0)
	if r.code.FcPrefix == nil {
		t.Fatalf("Expected prefix set [-\\s] but was nil")
	}
	if r.code.FcPrefix.PrefixSet.String() != "[-\\s]" {
		t.Fatalf("Expected prefix set [\\s-] but was %v", r.code.FcPrefix.PrefixSet.String())
	}
}

func TestSetInCode(t *testing.T) {
	r := MustCompile(`(?<body>\s*(?<name>.+))`, 0)
	t.Logf("code dump: %v", r.code.Dump())
	if want, got := 1, len(r.code.Sets); want != got {
		t.Fatalf("r.code.Sets wanted %v, got %v", want, got)
	}
	if want, got := "[\\s]", r.code.Sets[0].String(); want != got {
		t.Fatalf("first set wanted %v, got %v", want, got)
	}
}

func TestRegexp_Basic(t *testing.T) {
	r, err := Compile("test(?<named>ing)?", 0)
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
	} else {
		//t.Logf("Match: %v", m.dump())
	}
}

// check all our functions and properties around basic capture groups and referential for Group 0
func TestCapture_Basic(t *testing.T) {
	r := MustCompile(`.*\B(SUCCESS)\B.*`, 0)
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
	if want, got := 0, m.Index; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := 22, m.Length; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := 1, len(m.Captures); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}

	if want, got := m.String(), m.Captures[0].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := 0, m.Captures[0].Index; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := 22, m.Captures[0].Length; want != got {
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
	if want, got := 7, g[1].Index; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := 7, g[1].Length; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := "SUCCESS", g[1].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
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
		d{"(?<first_name>\\S+)\\s(?<last_name>\\S+)", // example
			"Ryan Byington",
			[]string{"0", "first_name", "last_name"},
			[]int{0, 1, 2},
			[]string{"Ryan Byington", "Ryan", "Byington"}},
		d{"((?<One>abc)\\d+)?(?<Two>xyz)(.*)", // example
			"abc208923xyzanqnakl",
			[]string{"0", "1", "2", "One", "Two"},
			[]int{0, 1, 2, 3, 4},
			[]string{"abc208923xyzanqnakl", "abc208923", "anqnakl", "abc", "xyz"}},
		d{"((?<256>abc)\\d+)?(?<16>xyz)(.*)", // numeric names
			"0272saasdabc8978xyz][]12_+-",
			[]string{"0", "1", "2", "16", "256"},
			[]int{0, 1, 2, 16, 256},
			[]string{"abc8978xyz][]12_+-", "abc8978", "][]12_+-", "xyz", "abc"}},
		d{"((?<4>abc)(?<digits>\\d+))?(?<2>xyz)(?<everything_else>.*)", // mix numeric and string names
			"0272saasdabc8978xyz][]12_+-",
			[]string{"0", "1", "2", "digits", "4", "everything_else"},
			[]int{0, 1, 2, 3, 4, 5},
			[]string{"abc8978xyz][]12_+-", "abc8978", "xyz", "8978", "abc", "][]12_+-"}},
		d{"(?<first_name>\\S+)\\s(?<first_name>\\S+)", // dupe string names
			"Ryan Byington",
			[]string{"0", "first_name"},
			[]int{0, 1},
			[]string{"Ryan Byington", "Byington"}},
		d{"(?<15>\\S+)\\s(?<15>\\S+)", // dupe numeric names
			"Ryan Byington",
			[]string{"0", "15"},
			[]int{0, 15},
			[]string{"Ryan Byington", "Byington"}},
		// *** repeated from above, but with alt cap syntax ***
		d{"(?'first_name'\\S+)\\s(?'last_name'\\S+)", //example
			"Ryan Byington",
			[]string{"0", "first_name", "last_name"},
			[]int{0, 1, 2},
			[]string{"Ryan Byington", "Ryan", "Byington"}},
		d{"((?'One'abc)\\d+)?(?'Two'xyz)(.*)", // example
			"abc208923xyzanqnakl",
			[]string{"0", "1", "2", "One", "Two"},
			[]int{0, 1, 2, 3, 4},
			[]string{"abc208923xyzanqnakl", "abc208923", "anqnakl", "abc", "xyz"}},
		d{"((?'256'abc)\\d+)?(?'16'xyz)(.*)", // numeric names
			"0272saasdabc8978xyz][]12_+-",
			[]string{"0", "1", "2", "16", "256"},
			[]int{0, 1, 2, 16, 256},
			[]string{"abc8978xyz][]12_+-", "abc8978", "][]12_+-", "xyz", "abc"}},
		d{"((?'4'abc)(?'digits'\\d+))?(?'2'xyz)(?'everything_else'.*)", // mix numeric and string names
			"0272saasdabc8978xyz][]12_+-",
			[]string{"0", "1", "2", "digits", "4", "everything_else"},
			[]int{0, 1, 2, 3, 4, 5},
			[]string{"abc8978xyz][]12_+-", "abc8978", "xyz", "8978", "abc", "][]12_+-"}},
		d{"(?'first_name'\\S+)\\s(?'first_name'\\S+)", // dupe string names
			"Ryan Byington",
			[]string{"0", "first_name"},
			[]int{0, 1},
			[]string{"Ryan Byington", "Byington"}},
		d{"(?'15'\\S+)\\s(?'15'\\S+)", // dupe numeric names
			"Ryan Byington",
			[]string{"0", "15"},
			[]int{0, 15},
			[]string{"Ryan Byington", "Byington"}},
	}

	fatalf := func(re *Regexp, v d, format string, args ...interface{}) {
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
		re := MustCompile(v.p, 0)

		// validate our group name/num info before execute
		validateGroupNamesNumbers(re, v)

		m, err := re.FindStringMatch(v.s)
		if err != nil {
			fatalf(re, v, "Unexpected error in match: %v", err)
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

func TestErr_GroupName(t *testing.T) {
	// group 0 is off limits
	if _, err := Compile("foo(?<0>bar)", 0); err == nil {
		t.Fatalf("zero group, expected error during compile")
	} else if want, got := "error parsing regexp: capture number cannot be zero in `foo(?<0>bar)`", err.Error(); want != got {
		t.Fatalf("invalid error text, want '%v', got '%v'", want, got)
	}
	if _, err := Compile("foo(?'0'bar)", 0); err == nil {
		t.Fatalf("zero group, expected error during compile")
	} else if want, got := "error parsing regexp: capture number cannot be zero in `foo(?'0'bar)`", err.Error(); want != got {
		t.Fatalf("invalid error text, want '%v', got '%v'", want, got)
	}

	// group tag can't start with a num
	if _, err := Compile("foo(?<1bar>)", 0); err == nil {
		t.Fatalf("invalid group name, expected error during compile")
	} else if want, got := "error parsing regexp: invalid group name: group names must begin with a word character and have a matching terminator in `foo(?<1bar>)`", err.Error(); want != got {
		t.Fatalf("invalid error text, want '%v', got '%v'", want, got)
	}
	if _, err := Compile("foo(?'1bar')", 0); err == nil {
		t.Fatalf("invalid group name, expected error during compile")
	} else if want, got := "error parsing regexp: invalid group name: group names must begin with a word character and have a matching terminator in `foo(?'1bar')`", err.Error(); want != got {
		t.Fatalf("invalid error text, want '%v', got '%v'", want, got)
	}

	// missing closing group tag
	if _, err := Compile("foo(?<bar)", 0); err == nil {
		t.Fatalf("invalid group name, expected error during compile")
	} else if want, got := "error parsing regexp: invalid group name: group names must begin with a word character and have a matching terminator in `foo(?<bar)`", err.Error(); want != got {
		t.Fatalf("invalid error text, want '%v', got '%v'", want, got)
	}
	if _, err := Compile("foo(?'bar)", 0); err == nil {
		t.Fatalf("invalid group name, expected error during compile")
	} else if want, got := "error parsing regexp: invalid group name: group names must begin with a word character and have a matching terminator in `foo(?'bar)`", err.Error(); want != got {
		t.Fatalf("invalid error text, want '%v', got '%v'", want, got)
	}

}

func TestConstantUneffected(t *testing.T) {
	// had a bug where "constant" sets would get modified with alternations and be broken in memory until restart
	// this meant that if you used a known-set (like \s) in a larger set it would "poison" \s for the process
	re := MustCompile(`(\s|\*)test\s`, 0)
	if want, got := 2, len(re.code.Sets); want != got {
		t.Fatalf("wanted %v sets, got %v", want, got)
	}
	if want, got := "[*\\s]", re.code.Sets[0].String(); want != got {
		t.Fatalf("wanted set 0 %v, got %v", want, got)
	}
	if want, got := "[\\s]", re.code.Sets[1].String(); want != got {
		t.Fatalf("wanted set 1 %v, got %v", want, got)
	}
}
