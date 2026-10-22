package regexp2

import (
	"errors"
	"strings"
	"sync"
	"testing"
	"time"
)

func TestMatchStepLimitStopsCatastrophicBacktracking(t *testing.T) {
	re := MustCompile(`^(a+)+$`, ECMAScript)
	re.MatchStepLimit = 100_000

	m, err := re.MatchString(strings.Repeat("a", 40) + "!")
	if !errors.Is(err, ErrMatchStepLimit) {
		t.Fatalf("MatchString = %v, %v; want ErrMatchStepLimit", m, err)
	}
}

func TestMatchStepLimitAllowsOrdinaryMatches(t *testing.T) {
	re := MustCompile(`^[A-Z]{2}$`, ECMAScript)
	re.MatchStepLimit = 1_000

	for in, want := range map[string]bool{"FR": true, "fr": false, "FRA": false, "": false} {
		if got, err := re.MatchString(in); err != nil || got != want {
			t.Errorf("MatchString(%q) = %v, %v; want %v, nil", in, got, err, want)
		}
	}
}

func TestMatchStepLimitCountsEachMatchFromZero(t *testing.T) {
	re := MustCompile(`^[a-z]+$`, ECMAScript)
	re.MatchStepLimit = 1_000
	in := strings.Repeat("a", 100)

	// The runner is pooled: a count left over from an earlier match would fail a later one.
	for i := 0; i < 50; i++ {
		if ok, err := re.MatchString(in); err != nil || !ok {
			t.Fatalf("match %d: MatchString = %v, %v; want true, nil", i, ok, err)
		}
	}
}

func TestMatchStepLimitIsDeterministic(t *testing.T) {
	re := MustCompile(`^(a|aa)+$`, ECMAScript)
	in := strings.Repeat("a", 20) + "!"

	// Find the smallest limit this input passes; the result one step below it never changes.
	var limit int64 = 1
	for {
		re.MatchStepLimit = limit
		if _, err := re.MatchString(in); err == nil {
			break
		}
		limit *= 2
		if limit > 1<<40 {
			t.Fatal("no limit was enough")
		}
	}
	lo, hi := limit/2, limit
	for lo+1 < hi {
		mid := (lo + hi) / 2
		re.MatchStepLimit = mid
		if _, err := re.MatchString(in); err == nil {
			hi = mid
		} else {
			lo = mid
		}
	}
	for i := 0; i < 20; i++ {
		re.MatchStepLimit = hi
		if _, err := re.MatchString(in); err != nil {
			t.Fatalf("run %d at limit %d: %v; want nil", i, hi, err)
		}
		re.MatchStepLimit = hi - 1
		if _, err := re.MatchString(in); !errors.Is(err, ErrMatchStepLimit) {
			t.Fatalf("run %d at limit %d: %v; want ErrMatchStepLimit", i, hi-1, err)
		}
	}
}

func TestMatchStepLimitUnderContention(t *testing.T) {
	re := MustCompile(`^[A-Z]{2}$`, ECMAScript)
	re.MatchStepLimit = 1_000

	// Far more goroutines than CPUs: none may fail, however long it waits to run.
	var wg sync.WaitGroup
	errs := make(chan error, 256)
	for g := 0; g < 256; g++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for i := 0; i < 200; i++ {
				if ok, err := re.MatchString("FR"); err != nil || !ok {
					errs <- err
					return
				}
			}
		}()
	}
	wg.Wait()
	close(errs)
	for err := range errs {
		t.Fatalf("a match failed under contention: %v", err)
	}
}

func TestMatchStepLimitWithMatchTimeout(t *testing.T) {
	re := MustCompile(`^(a+)+$`, ECMAScript)
	re.MatchStepLimit = 100_000
	re.MatchTimeout = time.Minute

	// The step limit is reached long before a minute.
	if _, err := re.MatchString(strings.Repeat("a", 40) + "!"); !errors.Is(err, ErrMatchStepLimit) {
		t.Fatalf("err = %v; want ErrMatchStepLimit", err)
	}

	re.MatchStepLimit = 0
	re.MatchTimeout = 50 * time.Millisecond
	if _, err := re.MatchString(strings.Repeat("a", 40) + "!"); err == nil || errors.Is(err, ErrMatchStepLimit) {
		t.Fatalf("err = %v; want a timeout", err)
	}
}

func TestMatchStepLimitZeroIsNoLimit(t *testing.T) {
	re := MustCompile(`^(a|b)*c$`, ECMAScript)
	if ok, err := re.MatchString(strings.Repeat("ab", 10_000) + "c"); err != nil || !ok {
		t.Fatalf("MatchString = %v, %v; want true, nil", ok, err)
	}
}
