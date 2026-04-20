package regexp2

import "testing"

func TestMaintainCaptureOrder_Basic(t *testing.T) {
	re := MustCompile(`(?<first>this).+?(testing).+?(?<last>stuff)`, OptionMaintainCaptureOrder())
	m, err := re.FindStringMatch(`this is a testing stuff`)
	if err != nil {
		t.Fatalf("unexpected match err: %v", err)
	}
	if m == nil {
		t.Fatal("expected match")
	}

	groups := m.Groups()
	if want, got := []string{"0", "first", "2", "last"}, re.GetGroupNames(); !stringSlicesEqual(want, got) {
		t.Fatalf("group names wanted %v got %v", want, got)
	}
	if want, got := []int{0, 1, 2, 3}, re.GetGroupNumbers(); !intSlicesEqual(want, got) {
		t.Fatalf("group numbers wanted %v got %v", want, got)
	}
	if want, got := "this", groups[1].String(); want != got {
		t.Fatalf("group 1 wanted %q got %q", want, got)
	}
	if want, got := "first", groups[1].Name; want != got {
		t.Fatalf("group 1 name wanted %q got %q", want, got)
	}
	if want, got := "testing", groups[2].String(); want != got {
		t.Fatalf("group 2 wanted %q got %q", want, got)
	}
	if want, got := "2", groups[2].Name; want != got {
		t.Fatalf("group 2 name wanted %q got %q", want, got)
	}
	if want, got := "stuff", groups[3].String(); want != got {
		t.Fatalf("group 3 wanted %q got %q", want, got)
	}
	if want, got := "last", groups[3].Name; want != got {
		t.Fatalf("group 3 name wanted %q got %q", want, got)
	}
}

func TestMaintainCaptureOrder_DefaultIsDotNetOrder(t *testing.T) {
	re := MustCompile(`(?<first>this).+?(testing).+?(?<last>stuff)`)
	m, err := re.FindStringMatch(`this is a testing stuff`)
	if err != nil {
		t.Fatalf("unexpected match err: %v", err)
	}
	if m == nil {
		t.Fatal("expected match")
	}

	groups := m.Groups()
	if want, got := []string{"0", "1", "first", "last"}, re.GetGroupNames(); !stringSlicesEqual(want, got) {
		t.Fatalf("group names wanted %v got %v", want, got)
	}
	if want, got := []int{0, 1, 2, 3}, re.GetGroupNumbers(); !intSlicesEqual(want, got) {
		t.Fatalf("group numbers wanted %v got %v", want, got)
	}
	if want, got := "testing", groups[1].String(); want != got {
		t.Fatalf("group 1 wanted %q got %q", want, got)
	}
	if want, got := "this", groups[2].String(); want != got {
		t.Fatalf("group 2 wanted %q got %q", want, got)
	}
	if want, got := "stuff", groups[3].String(); want != got {
		t.Fatalf("group 3 wanted %q got %q", want, got)
	}
}

func TestMaintainCaptureOrder_DuplicateNamesShareSlot(t *testing.T) {
	re := MustCompile(`(?<word>\w+)-(?<word>\w+)`, OptionMaintainCaptureOrder())
	m, err := re.FindStringMatch(`first-second`)
	if err != nil {
		t.Fatalf("unexpected match err: %v", err)
	}
	if m == nil {
		t.Fatal("expected match")
	}

	group := m.GroupByName("word")
	if group == nil {
		t.Fatal("expected group by name")
	}
	if want, got := "second", group.String(); want != got {
		t.Fatalf("group value wanted %q got %q", want, got)
	}
	if want, got := 2, len(group.Captures); want != got {
		t.Fatalf("capture count wanted %v got %v", want, got)
	}
	if want, got := []string{"0", "word"}, re.GetGroupNames(); !stringSlicesEqual(want, got) {
		t.Fatalf("group names wanted %v got %v", want, got)
	}
}

func stringSlicesEqual(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func intSlicesEqual(a, b []int) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
