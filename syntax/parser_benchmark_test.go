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
