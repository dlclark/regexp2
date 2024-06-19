package regexp2

import (
	"sync"
	"time"
)

type RuntimeEngine interface {
	Caps() map[int]int        // capnum->index
	CapNames() map[string]int // cap group name -> index
	CapsList() []string       //sorted list of capture group names
	CapSize() int             // size of the capture array
	NewRunner() RegexRunner   // return a new regex runner, these are cached
}

type RegexRunner interface {
	Scan(rt []rune, textstart int, quick bool, timeout time.Duration) (*Match, error)
}

type cacheKey struct {
	expr string
	opt  RegexOptions
}

func RegisterEngine(expr string, opt RegexOptions, engine RuntimeEngine) {
	engines[cacheKey{expr, opt}] = &Regexp{
		pattern:      expr,
		options:      opt,
		caps:         engine.Caps(),
		capnames:     engine.CapNames(),
		capslist:     engine.CapsList(),
		capsize:      engine.CapSize(),
		MatchTimeout: DefaultMatchTimeout,
		muRun:        &sync.Mutex{},
		newRunner:    engine.NewRunner,
	}
}

func getEngineRegexp(expr string, opt RegexOptions) *Regexp {
	val, ok := engines[cacheKey{expr, opt}]
	if !ok {
		return nil
	}
	return val
}

var engines = map[cacheKey]*Regexp{}
