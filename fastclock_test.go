package regexp2

import (
	"fmt"
	"log"
	"testing"
	"time"
)

func init() {
	//speed up testing by making the timeout clock 1ms instead of 100ms...
	//bad for benchmark tests though
	SetTimeoutCheckPeriod(time.Millisecond)
}
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

func TestStopTimeoutClock(t *testing.T) {
	// run a quick regex with a long timeout
	// make sure the stop clock returns quickly
	r := MustCompile(".", 0)
	r.MatchTimeout = time.Second * 10

	r.MatchString("a")
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
	t.Logf("current fast: %+v", fast)
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
	log.SetFlags(log.Lmicroseconds)
	re := MustCompile(`\[(\d+)\]\s+\[([\s\S]+)\]\s+([\s\S]+).*`, RE2)
	// there's a lot of slop in the timeout process (on purpose to keep resources low)
	// need a timeout of at least 5x the clockPeriod to have consistent test behavior
	re.MatchTimeout = 5 * clockPeriod

	_, err := re.FindStringMatch("[10000] [Dec 15, 2012 1:42:43 AM] com.dev.log.LoggingExample main")
	if err != nil {
		t.Fatalf("expected no error, got %v", err)
	}

	// now wait - this makes sure the background timer goroutine is stopped
	time.Sleep(time.Second + re.MatchTimeout*2)

	if val := fast.clockEnd.read() - fast.current.read(); val > 0 {
		t.Fatalf("unexpected bg timer running: %v", val)
	}

	// each of these should be plenty fast to not timeout
	for i := 0; i < 1000; i++ {
		_, err := re.FindStringMatch("[10000] [Dec 15, 2012 1:42:43 AM] com.dev.log.LoggingExample main")
		if err != nil {
			log.Printf("timeout")
			t.Fatalf("Expecting no error, got: '%v' on iteration %v", err, i)
		}
	}
}
