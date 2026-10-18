package regexp2

import (
	"strconv"
	"strings"
	"testing"
)

func TestReplace_Basic(t *testing.T) {
	re := MustCompile(`test`)
	str, err := re.Replace("this is a test", "unit", -1, -1)
	if err != nil {
		t.Fatalf("Unexpected err: %v", err)
	}
	if want, got := "this is a unit", str; want != got {
		t.Fatalf("Replace failed, wanted %v, got %v", want, got)
	}
}

func TestReplace_NamedGroup(t *testing.T) {
	re := MustCompile(`[^ ]+\s(?<time>)`)
	str, err := re.Replace("08/10/99 16:00", "${time}", -1, -1)
	if err != nil {
		t.Fatalf("Unexpected err: %v", err)
	}
	if want, got := "16:00", str; want != got {
		t.Fatalf("Replace failed, wanted %v, got %v", want, got)
	}
}

func TestReplace_IgnoreCaseUpper(t *testing.T) {
	re := MustCompile(`dog`, IgnoreCase)
	str, err := re.Replace("my dog has fleas", "CAT", -1, -1)
	if err != nil {
		t.Fatalf("Unexpected err: %v", err)
	}
	if want, got := "my CAT has fleas", str; want != got {
		t.Fatalf("Replace failed, wanted %v, got %v", want, got)
	}
}

func TestReplace_IgnoreCaseLower(t *testing.T) {
	re := MustCompile(`olang`, IgnoreCase)
	str, err := re.Replace("GoLAnG", "olang", -1, -1)
	if err != nil {
		t.Fatalf("Unexpected err: %v", err)
	}
	if want, got := "Golang", str; want != got {
		t.Fatalf("Replace failed, wanted %v, got %v", want, got)
	}
}

func TestReplace_NumberGroup(t *testing.T) {
	re := MustCompile(`D\.(.+)`, None)
	str, err := re.Replace("D.Bau", "David $1", -1, -1)
	if err != nil {
		t.Fatalf("Unexpected err: %v", err)
	}
	if want, got := "David Bau", str; want != got {
		t.Fatalf("Replace failed, wanted %v, got %v", want, got)
	}
}

func TestReplace_LimitCount(t *testing.T) {
	re := MustCompile(`a`, None)
	str, err := re.Replace("aaaaa", "b", 0, 2)
	if err != nil {
		t.Fatalf("Unexpected err: %v", err)
	}
	if want, got := "bbaaa", str; want != got {
		t.Fatalf("Replace failed, wanted %v, got %v", want, got)
	}
}

func TestReplace_ZeroCount(t *testing.T) {
	for _, option := range []RegexOptions{None, RightToLeft} {
		re := MustCompile(`a`, option)
		for _, input := range []string{"aあa", "no match"} {
			if got, err := re.Replace(input, "b", -1, 0); err != nil || got != input {
				t.Errorf("Replace(%q, count=0) = %q, %v", input, got, err)
			}
			got, err := re.ReplaceFunc(input, func(Match) string {
				t.Error("evaluator called with count=0")
				return "b"
			}, -1, 0)
			if err != nil || got != input {
				t.Errorf("ReplaceFunc(%q, count=0) = %q, %v", input, got, err)
			}
		}
	}
}

func TestReplace_LimitCountSlice(t *testing.T) {
	re := MustCompile(`a`, None)
	myStr := "aaaaa"
	str, err := re.Replace(myStr, "b", 3, 2)
	if err != nil {
		t.Fatalf("Unexpected err: %v", err)
	}
	if want, got := "aaabb", str; want != got {
		t.Fatalf("Replace failed, wanted %v, got %v", want, got)
	}
}

func TestReplace_BeginBeforeAfterEnd(t *testing.T) {
	re := MustCompile(`a`, None)
	myStr := "a test a blah and a"
	str, err := re.Replace(myStr, "stuff", -1, -1)
	if err != nil {
		t.Fatalf("Unexpected err: %v", err)
	}
	if want, got := "stuff test stuff blstuffh stuffnd stuff", str; want != got {
		t.Fatalf("Replace failed, wanted %v, got %v", want, got)
	}
}

func TestReplacementRightToLeftFragmentsRegression(t *testing.T) {
	for _, tt := range []struct{ replacement, want string }{
		{`x${1}y`, "xay-xay"},
		{`pre${1}post`, "preapost-preapost"},
		{`${1}${1}`, "aa-aa"},
		{`literal`, "literal-literal"},
		{"", "-"},
		{"x$`y", "xy-xa-y"},
		{"x$'y", "x-ay-xy"},
		{`x$+y`, "xay-xay"},
		{`x$_y`, "xa-ay-xa-ay"},
	} {
		t.Run(tt.replacement, func(t *testing.T) {
			got, err := MustCompile(`(a)`, RightToLeft).Replace("a-a", tt.replacement, -1, -1)
			if err != nil || got != tt.want {
				t.Fatalf("got %q, %v; want %q", got, err, tt.want)
			}
		})
	}
}

func TestReplace_BadSyntax(t *testing.T) {
	re := MustCompile(`a`, None)
	myStr := "this is a test"
	_, err := re.Replace(myStr, `$5000000000`, -1, -1)
	if err == nil {
		t.Fatalf("Expected err")
	}
}

func TestReplaceFunc_Basic(t *testing.T) {
	re := MustCompile(`test`, None)
	str, err := re.ReplaceFunc("this is a test", func(m Match) string { return "unit" }, -1, -1)
	if err != nil {
		t.Fatalf("Unexpected err: %v", err)
	}
	if want, got := "this is a unit", str; want != got {
		t.Fatalf("Replace failed, wanted %v, got %v", want, got)
	}
}

func TestReplaceFunc_Multiple(t *testing.T) {
	re := MustCompile(`test`, None)
	count := 0
	str, err := re.ReplaceFunc("This test is another test for stuff", func(m Match) string {
		count++
		return strconv.Itoa(count)
	}, -1, -1)
	if err != nil {
		t.Fatalf("Unexpected err: %v", err)
	}
	if want, got := "This 1 is another 2 for stuff", str; want != got {
		t.Fatalf("Replace failed, wanted %v, got %v", want, got)
	}
}

func TestReplaceFunc_Groups(t *testing.T) {
	re := MustCompile(`test(?<sub>ing)?`, None)
	count := 0
	str, err := re.ReplaceFunc("This testing is another test testingly junk", func(m Match) string {
		count++
		if m.GroupByName("sub").RuneLength > 0 {
			// we have an "ing", make it negative
			return strconv.Itoa(count * -1)
		}
		return strconv.Itoa(count)
	}, -1, -1)
	if err != nil {
		t.Fatalf("Unexpected err: %v", err)
	}
	if want, got := "This -1 is another 2 -3ly junk", str; want != got {
		t.Fatalf("Replace failed, wanted %v, got %v", want, got)
	}
}

func TestReplace_RefNumsDollarAmbiguous(t *testing.T) {
	re := MustCompile("(123)hello(789)", None)
	res, err := re.Replace("123hello789", "$1456$2", -1, -1)
	if err != nil {
		t.Fatal(err)
	}
	if want, got := "$1456789", res; want != got {
		t.Fatalf("Wrong result: %s", got)
	}
}

func TestReplace_NestedGroups(t *testing.T) {
	re := MustCompile(`(\p{Sc}\s?)?(\d+\.?((?<=\.)\d+)?)(?(1)|\s?\p{Sc})?`, None)
	res, err := re.Replace("$17.43  €2 16.33  £0.98  0.43   £43   12€  17", "$2", -1, -1)
	if err != nil {
		t.Fatal(err)
	}
	if want, got := "17.43  2 16.33  0.98  0.43   43   12  17", res; want != got {
		t.Fatalf("Wrong result: %s", got)
	}
}

func TestReplace_SpecialsUseSliceIndexes(t *testing.T) {
	re := MustCompile(`needle`)
	input := strings.Repeat("x", 20) + "needle" + "tail"
	res, err := re.Replace(input, "[$`$']", -1, -1)
	if err != nil {
		t.Fatal(err)
	}
	if want, got := strings.Repeat("x", 20)+"["+strings.Repeat("x", 20)+"tail]tail", res; want != got {
		t.Fatalf("Replace $` $' = %q, want %q", got, want)
	}

	res, err = re.Replace("préneedle", "$_", -1, -1)
	if err != nil {
		t.Fatal(err)
	}
	if want, got := "prépréneedle", res; want != got {
		t.Fatalf("Replace $_ = %q, want %q", got, want)
	}
}

func TestReplace_UnicodePrefix(t *testing.T) {
	re := MustCompile(`(needle)`)
	prefix := strings.Repeat("pré", 8)
	input := prefix + "needle" + "tail"

	res, err := re.Replace(input, "[$1]", -1, -1)
	if err != nil {
		t.Fatal(err)
	}
	if want, got := prefix+"[needle]tail", res; want != got {
		t.Fatalf("Replace $1 = %q, want %q", got, want)
	}

	res, err = re.Replace(input, "[$`$']", -1, -1)
	if err != nil {
		t.Fatal(err)
	}
	if want, got := prefix+"["+prefix+"tail]tail", res; want != got {
		t.Fatalf("Replace $` $' = %q, want %q", got, want)
	}

	res, err = re.Replace(input, "$_", -1, -1)
	if err != nil {
		t.Fatal(err)
	}
	if want, got := prefix+input+"tail", res; want != got {
		t.Fatalf("Replace $_ = %q, want %q", got, want)
	}

	res, err = re.ReplaceFunc(input, func(m Match) string {
		if m.String() != "needle" {
			t.Fatalf("ReplaceFunc match = %q", m.String())
		}
		if g := m.GroupByNumber(1); g == nil || g.String() != "needle" {
			t.Fatalf("ReplaceFunc group = %v", g)
		}
		return "<" + m.String() + ">"
	}, -1, -1)
	if err != nil {
		t.Fatal(err)
	}
	if want, got := prefix+"<needle>tail", res; want != got {
		t.Fatalf("ReplaceFunc = %q, want %q", got, want)
	}
}

func TestReplace_StartAtMidRune(t *testing.T) {
	re := MustCompile(`abc`)
	if _, err := re.Replace("aéabc", "X", 2, -1); err == nil {
		t.Fatal("Expected startAt in the middle of a rune to fail")
	}
}
