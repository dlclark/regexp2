package regexp2

import (
	"sync"
)

type RuntimeEngine interface {
	Caps() map[int]int        // capnum->index
	CapNames() map[string]int // cap group name -> index
	CapsList() []string       //sorted list of capture group names
	CapSize() int             // size of the capture array
	FindFirstChar(r *Runner) bool
	Execute(r *Runner) error
}

type cacheKey struct {
	pattern string
	opt     RegexOptions
}

func RegisterEngine(pattern string, opt RegexOptions, engine RuntimeEngine) {
	enginesMu.Lock()
	engines[cacheKey{pattern, opt}] = engine
	enginesMu.Unlock()
}

func newEngineRegexp(pattern string, opt RegexOptions, engine RuntimeEngine) *Regexp {
	return &Regexp{
		pattern:       pattern,
		options:       opt,
		caps:          engine.Caps(),
		capnames:      engine.CapNames(),
		capslist:      engine.CapsList(),
		capsize:       engine.CapSize(),
		MatchTimeout:  DefaultMatchTimeout,
		muRun:         &sync.Mutex{},
		findFirstChar: engine.FindFirstChar,
		execute:       engine.Execute,
	}
}

func getEngineRegexp(pattern string, opt RegexOptions) *Regexp {
	enginesMu.RLock()
	engine, ok := engines[cacheKey{pattern, opt}]
	enginesMu.RUnlock()
	if !ok {
		return nil
	}
	return newEngineRegexp(pattern, opt, engine)
}

var (
	enginesMu sync.RWMutex
	engines   = map[cacheKey]RuntimeEngine{}
)
