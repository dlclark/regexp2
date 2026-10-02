package regexp2

import (
	"sync"
	"testing"
	"time"
)

func TestDeadlineAfterConcurrentRestart(t *testing.T) {
	StopTimeoutClock()
	t.Cleanup(func() {
		StopTimeoutClock()
		fast.mu.Lock()
		fast.current.write(0)
		fast.clockEnd.write(0)
		fast.start = time.Time{}
		fast.running = false
		fast.mu.Unlock()
	})
	for round := 0; round < 50; round++ {
		StopTimeoutClock()
		fast.mu.Lock()
		fast.start = time.Now().Add(-time.Minute)
		fast.current.write(0)
		fast.clockEnd.write(0)
		fast.running = false
		fast.mu.Unlock()
		start := make(chan struct{})
		var wg sync.WaitGroup
		deadlines := make(chan fasttime, 128)
		for i := 0; i < 128; i++ {
			wg.Add(1)
			go func() {
				defer wg.Done()
				<-start
				deadlines <- makeDeadline(10 * time.Second)
			}()
		}
		close(start)
		wg.Wait()
		close(deadlines)
		for deadline := range deadlines {
			if deadline.reached() {
				t.Errorf("round %d: fresh deadline %d already expired at %d", round, deadline, fast.current.read())
			}
		}
	}
}
