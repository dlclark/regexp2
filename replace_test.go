package regexp2

import "testing"

func TestReplace_Basic(t *testing.T) {
	re := MustCompile(`test`, 0)
	str, err := re.Replace("this is a test", "unit", -1, -1)
	if err != nil {
		t.Fatalf("Unexpected err: %v", err)
	}
	if want, got := "this is a unit", str; want != got {
		t.Fatalf("Replace failed, wanted %v, got %v", want, got)
	}
}

func TestReplace_NamedGroup(t *testing.T) {
	re := MustCompile(`[^ ]+\s(?<time>)`, 0)
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

func TestReplace_BadSyntax(t *testing.T) {
	re := MustCompile(`a`, None)
	myStr := "this is a test"
	_, err := re.Replace(myStr, `$5000000000`, -1, -1)
	if err == nil {
		t.Fatalf("Expected err")
	}
}
