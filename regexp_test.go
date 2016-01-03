package regexp2

import "testing"

func TestBasicRegexp(t *testing.T) {
	r, err := Compile("test(?<named>ing)?", 0)
	t.Logf("code dump: %v", r.code.Dump())

	if err != nil {
		t.Errorf("unexpected compile err: %v", err)
	}
	m, err := r.FindStringMatch("this is a test stuff")
	if err != nil {
		t.Errorf("unexpected match err: %v", err)
	}
	if m == nil {
		t.Error("Nil match, expected success")
	} else {
		t.Logf("Match: %v", m.dump())
	}
}
