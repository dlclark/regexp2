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

func TestRegisterEngine_QuickExecute(t *testing.T) {
	const pattern = "quick-execute"
	fullCalls := 0
	quickCalls := 0
	RegisterEngine(pattern, RuntimeEngineData{
		CapSize: 1,
		FindFirstChar: func(r *Runner) bool {
			return r.Runtextpos == 0
		},
		Execute: func(r *Runner) error {
			fullCalls++
			r.Capture(0, 0, 1)
			return nil
		},
		ExecuteQuick: func(r *Runner) error {
			quickCalls++
			r.Capture(0, 0, 1)
			return nil
		},
	})

	re := MustCompile(pattern)
	if matched, err := re.MatchString("x"); err != nil || !matched {
		t.Fatalf("MatchString = %v, %v; want true, nil", matched, err)
	}
	if fullCalls != 0 || quickCalls != 1 {
		t.Fatalf("after MatchString: full calls = %d, quick calls = %d; want 0, 1", fullCalls, quickCalls)
	}
	if match, err := re.FindStringMatch("x"); err != nil || match == nil {
		t.Fatalf("FindStringMatch = %v, %v; want match, nil", match, err)
	}
	if fullCalls != 1 || quickCalls != 1 {
		t.Fatalf("after FindStringMatch: full calls = %d, quick calls = %d; want 1, 1", fullCalls, quickCalls)
	}
}

func TestRegisterEngine_CacheKeyIncludesBacktrackingStackLimit(t *testing.T) {
	const pattern = "stack-limit-cache-key"
	register := func(limit, marker int) {
		RegisterEngine(pattern, RuntimeEngineData{
			CapSize: 1,
			FindFirstChar: func(r *Runner) bool {
				return r.Runtextpos == 0
			},
			Execute: func(r *Runner) error {
				r.Runtextpos = marker
				r.Capture(0, 0, marker)
				return nil
			},
		}, OptionMaxBacktrackingStackSize(limit))
	}
	register(10, 1)
	register(20, 2)

	for _, tt := range []struct {
		limit, wantLength int
	}{
		{limit: 10, wantLength: 1},
		{limit: 20, wantLength: 2},
	} {
		re := MustCompile(pattern, OptionMaxBacktrackingStackSize(tt.limit))
		match, err := re.FindStringMatch("xx")
		if err != nil || match == nil {
			t.Fatalf("limit %d: FindStringMatch = %v, %v; want match, nil", tt.limit, match, err)
		}
		if match.RuneLength != tt.wantLength {
			t.Fatalf("limit %d: match length = %d, want %d", tt.limit, match.RuneLength, tt.wantLength)
		}
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

func TestStringPrefixFilterSkipsDecodeOnMiss(t *testing.T) {
	const pattern = `string-prefix-filter-miss-only`
	RegisterEngine(pattern, RuntimeEngineData{
		CapSize: 1,
		StringPrefixFilter: func(input string, startAt int) (candidateByteIndex int, ok bool) {
			if input != "missing" || startAt != 0 {
				t.Fatalf("filter got input=%q startAt=%d", input, startAt)
			}
			return 0, false
		},
		FindFirstChar: func(r *Runner) bool {
			t.Fatal("FindFirstChar should not run after a string prefix filter miss")
			return false
		},
		Execute: func(r *Runner) error {
			t.Fatal("Execute should not run after a string prefix filter miss")
			return nil
		},
	})

	re := MustCompile(pattern)
	matched, err := re.MatchString("missing")
	if err != nil {
		t.Fatalf("MatchString failed: %v", err)
	}
	if matched {
		t.Fatal("unexpected match")
	}
}
