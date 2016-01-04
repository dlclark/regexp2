package regexp2

import (
	"reflect"
	"testing"
)

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
	if want, got := m.Captures[0], g[0].Captures[0]; want != got {
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
		d{"(?<first_name>\\S+)\\s(?<last_name>\\S+)",
			"Ryan Byington",
			[]string{"0", "first_name", "last_name"},
			[]int{0, 1, 2},
			[]string{"Ryan Byington", "Ryan", "Byington"}},
		d{"((?<One>abc)\\d+)?(?<Two>xyz)(.*)",
			"abc208923xyzanqnakl",
			[]string{"0", "1", "2", "One", "Two"},
			[]int{0, 1, 2, 3, 4},
			[]string{"abc208923xyzanqnakl", "abc208923", "anqnakl", "abc", "xyz"}},
	}

	validateGroupNamesNumbers := func(re *Regexp, v d) {
		if len(v.name) != len(v.num) {
			t.Fatalf("Invalid data, group name count and number count must match: %+v", v)
		}

		groupNames := re.GetGroupNames()
		if !reflect.DeepEqual(groupNames, v.name) {
			t.Fatalf("expected: %v, actual: %v", v.name, groupNames)
		}
		groupNums := re.GetGroupNumbers()
		if !reflect.DeepEqual(groupNums, v.num) {
			t.Fatalf("expected: %v, actual: %v", v.num, groupNums)
		}
		// make sure we can freely get names and numbers from eachother
		for i := range groupNums {
			if want, got := groupNames[i], re.GroupNameFromNumber(groupNums[i]); want != got {
				t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
			}
			if want, got := groupNums[i], re.GroupNumberFromName(groupNames[i]); want != got {
				t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
			}
		}
	}

	for _, v := range data {
		// compile the regex
		re := MustCompile(v.p, 0)

		fatalf := func(format string, args ...interface{}) {
			args = append(args, v, re.code.Dump())
			t.Fatalf(format+" using test data: %#v\ndump:%v", args...)
		}

		// validate our group name/num info before execute
		validateGroupNamesNumbers(re, v)

		m, err := re.FindStringMatch(v.s)
		if err != nil {
			fatalf("Unexpected error in match: %v", err)
		}
		if want, got := len(v.strs), m.GroupCount(); want != got {
			fatalf("Wanted '%v'\nGot '%v'", want, got)
		}
		g := m.Groups()
		if want, got := len(v.strs), len(g); want != got {
			fatalf("Wanted '%v'\nGot '%v'", want, got)
		}
		// validate each group's value from the execute
		for i := range v.name {
			grp1 := m.GroupByName(v.name[i])
			grp2 := m.GroupByNumber(v.num[i])
			// should be identical reference
			if grp1 != grp2 {
				fatalf("Expected GroupByName and GroupByNumber to return same result for %v, %v", v.name[i], v.num[i])
			}
			if want, got := v.strs[i], grp1.String(); want != got {
				fatalf("Value[%v] Wanted '%v'\nGot '%v'", i, want, got)
			}
		}

		// validate our group name/num info after execute
		validateGroupNamesNumbers(re, v)
	}
}
