package regexp2

import "testing"

func TestGroupNumberFromNameRejectsInvalidNumbers(t *testing.T) {
	re := MustCompile(`(x)`)
	for _, tc := range []struct {
		name string
		want int
	}{
		{"", -1},
		{"18446744073709551616", -1},
		{"18446744073709551617", -1},
		{"+1", -1},
		{"-1", -1},
		{"2", -1},
		{"0", 0},
		{"1", 1},
		{"0001", 1},
	} {
		if got := re.GroupNumberFromName(tc.name); got != tc.want {
			t.Errorf("GroupNumberFromName(%q) = %d, want %d", tc.name, got, tc.want)
		}
	}
}
