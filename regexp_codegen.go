package regexp2

import (
	"sync"
)

type RuntimeEngineData struct {
	Caps          map[int]int        // capnum->index
	CapNames      map[string]int     // cap group name -> index
	CapsList      []string           // sorted list of capture group names
	CapSize       int                // size of the capture array
	FindFirstChar func(*Runner) bool // generated candidate search
	Execute       func(*Runner) error
}

type cacheKey struct {
	pattern string
	opt     RegexOptions
}

func RegisterEngine(pattern string, opt RegexOptions, engine RuntimeEngineData) {
	enginesMu.Lock()
	engines[cacheKey{pattern, opt}] = engine
	enginesMu.Unlock()
}

func newEngineRegexp(pattern string, opt RegexOptions, optimizations OptimizationOptions, engine RuntimeEngineData) *Regexp {
	re := &Regexp{
		pattern:       pattern,
		options:       opt,
		caps:          engine.Caps,
		capnames:      engine.CapNames,
		capslist:      engine.CapsList,
		capsize:       engine.CapSize,
		MatchTimeout:  DefaultMatchTimeout,
		optimizations: optimizations,
		findFirstChar: engine.FindFirstChar,
		execute:       engine.Execute,
	}
	re.initCaches()
	return re
}

func getEngineRegexp(pattern string, opt RegexOptions, optimizations OptimizationOptions) *Regexp {
	enginesMu.RLock()
	engine, ok := engines[cacheKey{pattern, opt}]
	enginesMu.RUnlock()
	if !ok {
		return nil
	}
	return newEngineRegexp(pattern, opt, optimizations, engine)
}

var (
	enginesMu sync.RWMutex
	engines   = map[cacheKey]RuntimeEngineData{}
)
