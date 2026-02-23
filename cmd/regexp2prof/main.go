package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"runtime"
	"runtime/pprof"
	"strings"
	"time"

	"github.com/dlclark/regexp2"
)

type workload struct {
	name string
	run  func() error
}

func main() {
	seconds := flag.Int("seconds", 3, "seconds to run each workload")
	warmup := flag.Int("warmup", 2000, "warmup iterations per workload before profiling")
	scenarioFlag := flag.String("scenario", "all", "comma-separated workload names or 'all'")
	cpuProfilePath := flag.String("cpuprofile", "regexp2_cpu.pprof", "cpu profile output path (empty to disable)")
	memProfilePath := flag.String("memprofile", "regexp2_mem.pprof", "heap profile output path (empty to disable)")
	flag.Parse()

	if *seconds <= 0 {
		log.Fatal("-seconds must be > 0")
	}
	if *warmup < 0 {
		log.Fatal("-warmup must be >= 0")
	}

	all, err := buildWorkloads()
	if err != nil {
		log.Fatalf("failed to initialize workloads: %v", err)
	}

	selected, err := selectWorkloads(all, *scenarioFlag)
	if err != nil {
		log.Fatalf("invalid -scenario: %v", err)
	}

	for _, w := range selected {
		for i := 0; i < *warmup; i++ {
			if err := w.run(); err != nil {
				log.Fatalf("warmup failed for %s: %v", w.name, err)
			}
		}
	}

	var cpuFile *os.File
	if *cpuProfilePath != "" {
		cpuFile, err = os.Create(*cpuProfilePath)
		if err != nil {
			log.Fatalf("could not create cpu profile: %v", err)
		}
		if err := pprof.StartCPUProfile(cpuFile); err != nil {
			_ = cpuFile.Close()
			log.Fatalf("could not start cpu profile: %v", err)
		}
	}

	fmt.Printf("running %d workloads, %ds each\n", len(selected), *seconds)
	for _, w := range selected {
		ops, elapsed, err := runForDuration(w, time.Duration(*seconds)*time.Second)
		if err != nil {
			log.Fatalf("workload failed for %s: %v", w.name, err)
		}
		nsPerOp := float64(elapsed.Nanoseconds()) / float64(ops)
		opsPerSec := float64(ops) / elapsed.Seconds()
		fmt.Printf("%-26s ops=%9d ns/op=%12.1f ops/s=%12.0f\n", w.name, ops, nsPerOp, opsPerSec)
	}

	if cpuFile != nil {
		pprof.StopCPUProfile()
		if err := cpuFile.Close(); err != nil {
			log.Fatalf("could not close cpu profile: %v", err)
		}
	}

	if *memProfilePath != "" {
		runtime.GC()
		memFile, err := os.Create(*memProfilePath)
		if err != nil {
			log.Fatalf("could not create mem profile: %v", err)
		}
		if err := pprof.WriteHeapProfile(memFile); err != nil {
			_ = memFile.Close()
			log.Fatalf("could not write mem profile: %v", err)
		}
		if err := memFile.Close(); err != nil {
			log.Fatalf("could not close mem profile: %v", err)
		}
	}
}

func runForDuration(w workload, d time.Duration) (int, time.Duration, error) {
	start := time.Now()
	end := start.Add(d)
	ops := 0
	for time.Now().Before(end) {
		if err := w.run(); err != nil {
			return 0, 0, err
		}
		ops++
	}
	if ops == 0 {
		return 0, 0, fmt.Errorf("no iterations executed")
	}
	return ops, time.Since(start), nil
}

func selectWorkloads(all []workload, scenarioFlag string) ([]workload, error) {
	if scenarioFlag == "all" {
		return all, nil
	}

	wanted := make(map[string]bool)
	for _, name := range strings.Split(scenarioFlag, ",") {
		trimmed := strings.TrimSpace(name)
		if trimmed == "" {
			continue
		}
		wanted[trimmed] = true
	}

	if len(wanted) == 0 {
		return nil, fmt.Errorf("no scenarios requested")
	}

	selected := make([]workload, 0, len(wanted))
	for _, w := range all {
		if wanted[w.name] {
			selected = append(selected, w)
			delete(wanted, w.name)
		}
	}

	if len(wanted) > 0 {
		missing := make([]string, 0, len(wanted))
		for name := range wanted {
			missing = append(missing, name)
		}
		return nil, fmt.Errorf("unknown scenarios: %s", strings.Join(missing, ", "))
	}

	return selected, nil
}

func buildWorkloads() ([]workload, error) {
	logCorpus := profileLogCorpus(1200)
	wordCorpus := profileWordCorpus(300)

	matchLiteral, err := makeMatchRunesWorkload(
		"match-literal-log",
		"ERROR",
		0,
		logCorpus,
		true,
	)
	if err != nil {
		return nil, err
	}

	matchAlternation, err := makeMatchRunesWorkload(
		"match-alternation",
		"(ERROR|WARN|INFO|DEBUG|TRACE)",
		0,
		logCorpus,
		true,
	)
	if err != nil {
		return nil, err
	}

	matchLookaround, err := makeMatchRunesWorkload(
		"match-lookaround",
		`(?<=token=)[A-Za-z0-9_]+(?=;)`,
		0,
		strings.Repeat("a=1;", 200)+"token=session_ABC123;z=9;",
		true,
	)
	if err != nil {
		return nil, err
	}

	matchBackref, err := makeMatchRunesWorkload(
		"match-backref",
		`\b(\w+)\s+\1\b`,
		0,
		"one two two three",
		true,
	)
	if err != nil {
		return nil, err
	}

	matchCharClass, err := makeMatchRunesWorkload(
		"match-charclass",
		`[A-F0-9]{32}`,
		0,
		strings.Repeat("z", 1024)+"5F4DCC3B5AA765D61D8327DEB882CF99",
		true,
	)
	if err != nil {
		return nil, err
	}

	findIndices, err := makeFindIndicesWorkload(
		"find-indices-words",
		`\b\w+\b`,
		0,
		wordCorpus,
		300,
	)
	if err != nil {
		return nil, err
	}

	replaceToken, err := makeReplaceWorkload(
		"replace-token",
		`(?<=token=)[A-Za-z0-9_]+(?=;)`,
		0,
		strings.Repeat("a=1;token=session_ABC123;z=9;", 50),
		"REDACTED",
		strings.Repeat("a=1;token=REDACTED;z=9;", 50),
	)
	if err != nil {
		return nil, err
	}

	compileLookaround := workload{
		name: "compile-lookaround",
		run: func() error {
			re, err := regexp2.Compile(`(?<=token=)[A-Za-z0-9_]+(?=;)`, 0)
			if err != nil {
				return err
			}
			if re == nil {
				return fmt.Errorf("compile returned nil")
			}
			return nil
		},
	}

	return []workload{
		matchLiteral,
		matchAlternation,
		matchLookaround,
		matchBackref,
		matchCharClass,
		findIndices,
		replaceToken,
		compileLookaround,
	}, nil
}

func makeMatchRunesWorkload(name, pattern string, options regexp2.RegexOptions, input string, expected bool) (workload, error) {
	re, err := regexp2.Compile(pattern, options)
	if err != nil {
		return workload{}, err
	}
	runes := []rune(input)

	run := func() error {
		ok, err := re.MatchRunes(runes)
		if err != nil {
			return err
		}
		if ok != expected {
			return fmt.Errorf("match=%v expected=%v", ok, expected)
		}
		return nil
	}

	if err := run(); err != nil {
		return workload{}, err
	}

	return workload{name: name, run: run}, nil
}

func makeFindIndicesWorkload(name, pattern string, options regexp2.RegexOptions, input string, expected int) (workload, error) {
	re, err := regexp2.Compile(pattern, options)
	if err != nil {
		return workload{}, err
	}

	run := func() error {
		matches, err := re.FindStringMatchIndices(input)
		if err != nil {
			return err
		}
		if len(matches) != expected {
			return fmt.Errorf("count=%d expected=%d", len(matches), expected)
		}
		return nil
	}

	if err := run(); err != nil {
		return workload{}, err
	}

	return workload{name: name, run: run}, nil
}

func makeReplaceWorkload(name, pattern string, options regexp2.RegexOptions, input, replacement, expected string) (workload, error) {
	re, err := regexp2.Compile(pattern, options)
	if err != nil {
		return workload{}, err
	}

	run := func() error {
		result, err := re.Replace(input, replacement, -1, -1)
		if err != nil {
			return err
		}
		if result != expected {
			return fmt.Errorf("unexpected replacement result")
		}
		return nil
	}

	if err := run(); err != nil {
		return workload{}, err
	}

	return workload{name: name, run: run}, nil
}

func profileLogCorpus(lines int) string {
	var b strings.Builder
	b.Grow(lines * 72)
	for i := 0; i < lines; i++ {
		if i%97 == 0 {
			b.WriteString("2026-01-01T00:00:00Z service=payments level=ERROR msg=timeout request_id=abc123\n")
		} else {
			b.WriteString("2026-01-01T00:00:00Z service=payments level=INFO msg=ok request_id=abc123\n")
		}
	}
	return b.String()
}

func profileWordCorpus(words int) string {
	var b strings.Builder
	b.Grow(words * 8)
	for i := 0; i < words; i++ {
		if i > 0 {
			b.WriteRune(' ')
		}
		b.WriteString("word")
	}
	return b.String()
}
