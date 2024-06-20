package regexp2

import (
	"testing"
)

type emptyEngine struct {
	findFirstChar func(r *Runner) bool
	execute       func(r *Runner) error
}

func (t *emptyEngine) Caps() map[int]int        { return nil }
func (t *emptyEngine) CapNames() map[string]int { return nil }
func (t *emptyEngine) CapsList() []string       { return nil }
func (t *emptyEngine) CapSize() int             { return 1 }
func (t *emptyEngine) FindFirstChar(r *Runner) bool {
	return t.findFirstChar(r)
}
func (t *emptyEngine) Execute(r *Runner) error {
	return t.execute(r)
}

func TestRegisterEngine_CacheHit(t *testing.T) {
	didFindFirst := false
	didExecute := false
	RegisterEngine("This is a regexp", RE2, &emptyEngine{
		findFirstChar: func(r *Runner) bool {
			didFindFirst = true
			return true
		},
		execute: func(r *Runner) error {
			didExecute = true
			return nil
		},
	})

	re := MustCompile("This is a regexp", RE2)
	val, err := re.MatchString("This is a regexp")

	if !didFindFirst {
		t.Fatal("Expected find first char to be called, but it wasn't")
	}
	if !didExecute {
		t.Fatal("Expected execute to be called, but it wasn't")
	}
	if err != nil {
		t.Fatal("Unexpected error", err)
	}
	if val {
		t.Fatal("Expected no match")
	}
}

func TestRegisterEngine_CacheMiss(t *testing.T) {
	didFindFirst := false
	didExecute := false
	RegisterEngine("This is a regexp", Debug, &emptyEngine{
		findFirstChar: func(r *Runner) bool {
			didFindFirst = true
			return true
		},
		execute: func(r *Runner) error {
			didExecute = true
			return nil
		},
	})

	// only difference in our cache is our options
	re := MustCompile("This is a regexp", 0)
	val, err := re.MatchString("This is a regexp")

	if didFindFirst {
		t.Fatal("Expected find first to not be called, but it was")
	}
	if didExecute {
		t.Fatal("Expected execute to not be called, but it was")
	}
	if err != nil {
		t.Fatal("Unexpected error", err)
	}
	if !val {
		t.Fatal("Expected match")
	}
}
