package regexp2

import (
	"fmt"
	"testing"
	"time"
)

func init() {
	//speed up testing by making the timeout clock 1ms instead of 100ms...
	//bad for benchmark tests though
	SetTimeoutCheckPeriod(time.Millisecond)
}
func TestDeadline(t *testing.T) {
	StopTimeoutClock()
	fast.mu.Lock()
	fast.current.write(0)
	fast.clockEnd.write(fasttime(1 << 62))
	fast.start = time.Time{}
	fast.running = false
	fast.mu.Unlock()

	t.Cleanup(func() {
		StopTimeoutClock()
		fast.mu.Lock()
		fast.current.write(0)
		fast.clockEnd.write(0)
		fast.start = time.Time{}
		fast.running = false
		fast.mu.Unlock()
	})

	for _, delay := range []time.Duration{
		clockPeriod * 10,
		clockPeriod * 50,
		clockPeriod * 100,
	} {
		t.Run(fmt.Sprint(delay), func(t *testing.T) {
			d := makeDeadline(delay)
			if d.reached() {
				t.Fatalf("deadline (%v) unexpectedly expired immediately", delay)
			}

			expected := fast.current.read() + durationToTicks(delay+clockPeriod)
			if d != expected {
				t.Fatalf("deadline (%v) = %v, want %v", delay, d, expected)
			}

			fast.current.write(d - 1)
			if d.reached() {
				t.Fatalf("deadline (%v) expired too soon", delay)
			}
			fast.current.write(d)
			if !d.reached() {
				t.Fatalf("deadline (%v) did not expire", delay)
			}
		})
	}
}

func TestStopTimeoutClock(t *testing.T) {
	// run a quick regex with a long timeout
	// make sure the stop clock returns quickly
	r := MustCompile(".")
	r.MatchTimeout = time.Second * 10

	_, _ = r.MatchString("a")
	start := time.Now()
	StopTimeoutClock()
	stop := time.Now()

	if want, got := clockPeriod*2, stop.Sub(start); want < got {
		t.Errorf("Expected duration less than %v, got %v", want, got)
	}
	if want, got := false, fast.running; want != got {
		t.Errorf("Expected isRunning to be %v, got %v", want, got)
	}
}
func TestIncorrectDeadline(t *testing.T) {
	if fast.start.IsZero() {
		fast.start = time.Now()
	}
	// make fast stopped
	for fast.running {
		time.Sleep(clockPeriod)
	}
	fast.mu.Lock()
	t.Logf("current fast: start=%v running=%v current=%v clockEnd=%v", fast.start, fast.running, fast.current.read(), fast.clockEnd.read())
	fast.mu.Unlock()
	timeout := 5 * clockPeriod
	// make the error time much bigger
	time.Sleep(10 * clockPeriod)
	nowTick := durationToTicks(time.Since(fast.start))
	// before fix, fast.current will be the time fast stopped, and end is incorrect too
	// after fix, fast.current will be current time.
	d := makeDeadline(timeout)
	gotTick := fast.current.read()
	t.Logf("nowTick: %+v, gotTick: %+v", nowTick, gotTick)
	if nowTick > gotTick {
		t.Errorf("Expectd current should greater than %v, got %v", gotTick, nowTick)
	}
	expectedDeadTick := nowTick + durationToTicks(timeout)
	if d < expectedDeadTick {
		t.Errorf("Expectd deadTick %v, got %v", expectedDeadTick, d)
	}
}

func TestIncorrectTimeoutError(t *testing.T) {
	StopTimeoutClock()
	t.Cleanup(StopTimeoutClock)

	const input = "[10000] [Dec 15, 2012 1:42:43 AM] com.dev.log.LoggingExample main"
	re := MustCompile(`\[(\d+)\]\s+\[([\s\S]+)\]\s+([\s\S]+).*`, RE2)
	timeout := 5 * clockPeriod
	idle := 10 * timeout
	re.MatchTimeout = timeout

	// This test covers a false-timeout regression in the public regex path.
	// If the fast timeout clock has been idle long enough to stop, fast.current
	// can be stale. A new match must refresh fast.current before computing its
	// deadline; otherwise, when the background clock next updates to real time,
	// the freshly-created deadline can already look expired.
	fast.mu.Lock()
	fast.start = time.Now().Add(-idle)
	fast.current.write(0)
	fast.clockEnd.write(0)
	fast.running = false
	fast.mu.Unlock()

	// Keep the test deterministic while still exercising MatchString and the
	// normal engine execution. The hook runs after startTimeoutWatch has created
	// the deadline, so advancing fast.current here simulates the first clock tick
	// after the match starts without depending on scheduler timing or a real
	// short timeout.
	re.execute = func(r *Runner) error {
		StopTimeoutClock()
		fast.current.write(durationToTicks(idle + clockPeriod))
		return executeDefault(r)
	}

	if _, err := re.MatchString(input); err != nil {
		t.Fatalf("expected no timeout from a deadline made after idle clock stop, got %v", err)
	}
}
