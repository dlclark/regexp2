package regexp2

import "testing"

func TestRunnerFixedStackPushHelpers(t *testing.T) {
	r := &Runner{
		runstack:      make([]int, 8),
		Runstackpos:   8,
		runtrackcount: 1,
	}

	r.StackPush4(1, 2, 3, 4)
	if got := popN(r, 4); !equalInts(got, []int{4, 3, 2, 1}) {
		t.Fatalf("StackPush4 pop order = %v", got)
	}

	r.StackPush5(1, 2, 3, 4, 5)
	if got := popN(r, 5); !equalInts(got, []int{5, 4, 3, 2, 1}) {
		t.Fatalf("StackPush5 pop order = %v", got)
	}
}

func popN(r *Runner, n int) []int {
	vals := make([]int, 0, n)
	for range n {
		vals = append(vals, r.StackPop())
	}
	return vals
}

func equalInts(left, right []int) bool {
	if len(left) != len(right) {
		return false
	}
	for i := range left {
		if left[i] != right[i] {
			return false
		}
	}
	return true
}
