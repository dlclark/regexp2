package regexp2

import (
	"fmt"
	"testing"
	"time"
)

func TestDeadline(t *testing.T) {
	for _, delay := range []time.Duration{
		clockPeriod / 10,
		clockPeriod,
		clockPeriod * 5,
		clockPeriod * 10,
	} {
		delay := delay // Make copy for parallel sub-test.
		t.Run(fmt.Sprint(delay), func(t *testing.T) {
			t.Parallel()
			start := time.Now()
			d := makeDeadline(delay)
			if d.reached() {
				t.Fatalf("deadline (%v) unexpectedly expired immediately", delay)
			}
			time.Sleep(delay / 2)
			if d.reached() {
				t.Fatalf("deadline (%v) expired too soon (after %v)", delay, time.Since(start))
			}
			time.Sleep(delay/2 + 2*clockPeriod) // Give clock time to tick
			if !d.reached() {
				t.Fatalf("deadline (%v) did not expire within %v", delay, time.Since(start))
			}
		})
	}
}
