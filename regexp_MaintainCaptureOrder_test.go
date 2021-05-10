package regexp2

import (
	"testing"
)

func TestMaintainCaptureOrder_Basic(t *testing.T) {
	r, err := Compile("(?<first>this).+?(testing).+?(?<last>stuff)", MaintainCaptureOrder)
	// t.Logf("code dump: %v", r.code.Dump())
	if err != nil {
		t.Errorf("unexpected compile err: %v", err)
	}
	text := `this is a testing stuff`
	m, err := r.FindStringMatch(text)
	if err != nil {
		t.Errorf("unexpected match err: %v", err)
	}
	if m == nil {
		t.Error("Nil match, expected success")
	} else {
		//t.Logf("Match: %v", m.dump())
	}

	groups := m.Groups()
	if want, got := text, m.String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := text, groups[0].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `this`, groups[1].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `first`, groups[1].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `this`, string(m.GroupByName(`first`).Runes()); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `testing`, groups[2].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `2`, groups[2].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `stuff`, groups[3].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `last`, groups[3].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `stuff`, string(m.GroupByNumber(3).Runes()); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
}

func TestMaintainCaptureOrder_With_Other_Options(t *testing.T) {
	r, err := Compile("(?si)(?<first>this).+?\n(testing).+?(?<last>stuff)", MaintainCaptureOrder)
	// t.Logf("code dump: %v", r.code.Dump())
	if err != nil {
		t.Errorf("unexpected compile err: %v", err)
	}
	text := "This is a \ntesting stuff"
	m, err := r.FindStringMatch(text)
	if err != nil {
		t.Errorf("unexpected match err: %v", err)
	}
	if m == nil {
		t.Error("Nil match, expected success")
	} else {
		//t.Logf("Match: %v", m.dump())
	}

	groups := m.Groups()
	if want, got := text, m.String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := text, groups[0].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `This`, groups[1].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `first`, groups[1].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `testing`, groups[2].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `2`, groups[2].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `stuff`, groups[3].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `last`, groups[3].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
}

func TestMaintainCaptureOrder_Enable_Inline(t *testing.T) {
	r, err := Compile("(?sio)(?<first>this).+?\n(testing).+?(?<last>stuff)", 0)
	// t.Logf("code dump: %v", r.code.Dump())
	if err != nil {
		t.Errorf("unexpected compile err: %v", err)
	}
	text := "This is a \ntesting stuff"
	m, err := r.FindStringMatch(text)
	// t.Errorf(" groups: %#v\n", m)
	if err != nil {
		t.Errorf("unexpected match err: %v", err)
	}
	if m == nil {
		t.Error("Nil match, expected success")
	} else {
		//t.Logf("Match: %v", m.dump())
	}

	groups := m.Groups()
	if want, got := text, m.String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := text, groups[0].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `This`, groups[1].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `first`, groups[1].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `testing`, groups[2].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `2`, groups[2].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `stuff`, groups[3].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `last`, groups[3].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
}

func TestMaintainCaptureOrder_NestedCaptures(t *testing.T) {
	r, err := Compile(
		`(?<first>This)(?<second>(.)+?(?<test>testing)).+?(some.+?(other).+?(?<last>stuff)) (?<test>\k<test>)`, MaintainCaptureOrder)
	// t.Logf("code dump: %v", r.code.Dump())
	if err != nil {
		t.Errorf("unexpected compile err: %v", err)
	}
	text := "This is a testing some other stuff testing"
	m, err := r.FindStringMatch(text)

	if err != nil {
		t.Errorf("unexpected match err: %v", err)
	}
	if m == nil {
		t.Error("Nil match, expected success")
	} else {
		//t.Logf("Match: %v", m.dump())
	}

	groups := m.Groups()
	if want, got := text, m.String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := text, groups[0].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `This`, groups[1].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `first`, groups[1].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := ` is a testing`, groups[2].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `second`, groups[2].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := groups[2].String(), groups[2].Captures[0].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := ` `, groups[3].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `a`, groups[3].Captures[4].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `3`, groups[3].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `testing`, groups[4].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `test`, groups[4].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `some other stuff`, groups[5].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `5`, groups[5].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `other`, groups[6].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `6`, groups[6].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `stuff`, groups[7].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `last`, groups[7].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := 8, len(groups); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
}

func TestMaintainCaptureOrder_RE2_And_NumBackref(t *testing.T) {
	r, err := Compile(
		`(?'first'This).+?(?P<test>testing) (some).+?(?<4>stuff) \2`, MaintainCaptureOrder | RE2)
	// t.Logf("code dump: %v", r.code.Dump())
	if err != nil {
		t.Errorf("unexpected compile err: %v", err)
	}
	text := "This is a testing some other stuff testing"
	m, err := r.FindStringMatch(text)

	if err != nil {
		t.Errorf("unexpected match err: %v", err)
	}
	if m == nil {
		t.Error("Nil match, expected success")
	} else {
		//t.Logf("Match: %v", m.dump())
	}

	groups := m.Groups()
	if want, got := text, m.String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := text, groups[0].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `This`, groups[1].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `first`, groups[1].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `testing`, groups[2].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `test`, groups[2].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `some`, groups[3].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `3`, groups[3].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `stuff`, groups[4].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `4`, groups[4].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
}

func TestMaintainCaptureOrder_Balancing_Conditional_Alternation(t *testing.T) {
	r, err := Compile(
		`^[^<>]*(((?'Open'<)[^<>]*)+((?'Close-Open'>)[^<>]*)+)*(?(Open)(?!))$`, MaintainCaptureOrder)
	// t.Logf("code dump: %v", r.code.Dump())
	if err != nil {
		t.Errorf("unexpected compile err: %v", err)
	}
	text := "<abc><mno<xyz>>"
	m, err := r.FindStringMatch(text)

	if err != nil {
		t.Errorf("unexpected match err: %v", err)
	}
	if m == nil {
		t.Error("Nil match, expected success")
	} else {
		//t.Logf("Match: %v", m.dump())
	}

	groups := m.Groups()
	if want, got := text, m.String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := text, groups[0].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `<mno<xyz>>`, groups[1].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `<abc>`, groups[1].Captures[0].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `<mno<xyz>>`, groups[1].Captures[1].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `1`, groups[1].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `<xyz`, groups[2].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `<abc`, groups[2].Captures[0].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `<mno`, groups[2].Captures[1].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `<xyz`, groups[2].Captures[2].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `2`, groups[2].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := ``, groups[3].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `Open`, groups[3].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `>`, groups[4].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `>`, groups[4].Captures[0].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `>`, groups[4].Captures[1].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `>`, groups[4].Captures[2].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `4`, groups[4].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `mno<xyz>`, groups[5].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `abc`, groups[5].Captures[0].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `xyz`, groups[5].Captures[1].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `mno<xyz>`, groups[5].Captures[2].String(); want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
	if want, got := `Close`, groups[5].Name; want != got {
		t.Fatalf("Wanted '%v'\nGot '%v'", want, got)
	}
}
