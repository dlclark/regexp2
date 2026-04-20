package regexp2

import (
	"testing"
)

type emptyEngine struct {
	findFirstChar func(r *Runner) bool
	execute       func(r *Runner) error
}

func (t *emptyEngine) FindFirstChar(r *Runner) bool {
	return t.findFirstChar(r)
}
func (t *emptyEngine) Execute(r *Runner) error {
	return t.execute(r)
}

func TestRegisterEngine_CacheHit(t *testing.T) {
	didFindFirst := false
	didExecute := false
	engine := emptyEngine{
		findFirstChar: func(r *Runner) bool {
			didFindFirst = true
			return true
		},
		execute: func(r *Runner) error {
			didExecute = true
			return nil
		},
	}
	RegisterEngine("This is a regexp", RuntimeEngineData{
		CapSize:       1,
		FindFirstChar: engine.FindFirstChar,
		Execute:       engine.Execute,
	}, RE2)

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
	engine := emptyEngine{
		findFirstChar: func(r *Runner) bool {
			didFindFirst = true
			return true
		},
		execute: func(r *Runner) error {
			didExecute = true
			return nil
		},
	}
	RegisterEngine("This is a regexp", RuntimeEngineData{
		CapSize:       1,
		FindFirstChar: engine.FindFirstChar,
		Execute:       engine.Execute,
	}, IgnoreCase)

	// only difference in our cache is our options
	re := MustCompile("This is a regexp")
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

func TestRegisterEngine_CacheMissMaintainCaptureOrder(t *testing.T) {
	didFindFirst := false
	didExecute := false
	engine := emptyEngine{
		findFirstChar: func(r *Runner) bool {
			didFindFirst = true
			return true
		},
		execute: func(r *Runner) error {
			didExecute = true
			return nil
		},
	}
	RegisterEngine("(?<first>This) (is)", RuntimeEngineData{
		CapSize:       3,
		FindFirstChar: engine.FindFirstChar,
		Execute:       engine.Execute,
	})

	re := MustCompile("(?<first>This) (is)", OptionMaintainCaptureOrder())
	val, err := re.MatchString("This is")

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

func TestRegisterEngine_CacheHitMaintainCaptureOrder(t *testing.T) {
	didFindFirst := false
	didExecute := false
	engine := emptyEngine{
		findFirstChar: func(r *Runner) bool {
			didFindFirst = true
			return true
		},
		execute: func(r *Runner) error {
			didExecute = true
			return nil
		},
	}
	RegisterEngine("(?<first>This) (is)", RuntimeEngineData{
		CapSize:       3,
		FindFirstChar: engine.FindFirstChar,
		Execute:       engine.Execute,
	}, OptionMaintainCaptureOrder())

	re := MustCompile("(?<first>This) (is)", OptionMaintainCaptureOrder())
	val, err := re.MatchString("This is")

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
