package syntax

import (
	"strings"
	"testing"
)

func BenchmarkECMADuplicateAlternatives(b *testing.B) {
	pattern := strings.Repeat(`(?<x>a)|`, 7999) + `(?<x>a)`
	b.ReportAllocs()
	for b.Loop() {
		if _, err := Parse(pattern, ParseOptions{RegexOptions: ECMAScript}); err != nil {
			b.Fatal(err)
		}
	}
}

// Deciding whether [\r\n]+ can be atomic compares its set with ECMAScript's
// ".", which is stored as ranges that cover about 1.1M runes.
func BenchmarkECMASetLoopBeforeDot(b *testing.B) {
	b.ReportAllocs()
	for b.Loop() {
		if _, err := Parse(`[\r\n]+.`, ParseOptions{RegexOptions: ECMAScript}); err != nil {
			b.Fatal(err)
		}
	}
}
