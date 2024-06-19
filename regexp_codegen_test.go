package regexp2

import (
	"testing"
	"time"
)

type emptyEngine struct {
	scan func(rt []rune, textstart int, quick bool, timeout time.Duration) (*Match, error)
}

func (t *emptyEngine) Caps() map[int]int        { return nil }
func (t *emptyEngine) CapNames() map[string]int { return nil }
func (t *emptyEngine) CapsList() []string       { return nil }
func (t *emptyEngine) CapSize() int             { return 0 }
func (t *emptyEngine) NewRunner() RegexRunner   { return &emptyRunner{t.scan} }

type emptyRunner struct {
	scan func(rt []rune, textstart int, quick bool, timeout time.Duration) (*Match, error)
}

func (t *emptyRunner) Scan(rt []rune, textstart int, quick bool, timeout time.Duration) (*Match, error) {
	return t.scan(rt, textstart, quick, timeout)
}

func TestRegisterEngine_CacheHit(t *testing.T) {
	didScan := false
	RegisterEngine("This is a regexp", Debug, &emptyEngine{
		scan: func(rt []rune, textstart int, quick bool, timeout time.Duration) (*Match, error) {
			didScan = true
			return nil, nil
		},
	})

	re := MustCompile("This is a regexp", Debug)
	val, err := re.MatchString("This is a regexp")

	if !didScan {
		t.Fatal("Expected custom scan to be called, but it wasn't")
	}
	if err != nil {
		t.Fatal("Unexpected error", err)
	}
	if val {
		t.Fatal("Expected no match")
	}
}

func TestRegisterEngine_CacheMiss(t *testing.T) {
	didScan := false
	RegisterEngine("This is a regexp", Debug, &emptyEngine{
		scan: func(rt []rune, textstart int, quick bool, timeout time.Duration) (*Match, error) {
			didScan = true
			return nil, nil
		},
	})

	// only difference in our cache is our options
	re := MustCompile("This is a regexp", 0)
	val, err := re.MatchString("This is a regexp")

	if didScan {
		t.Fatal("Expected custom scan to not be called, but it was")
	}
	if err != nil {
		t.Fatal("Unexpected error", err)
	}
	if !val {
		t.Fatal("Expected match")
	}
}
