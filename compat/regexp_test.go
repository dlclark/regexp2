package compat

import (
	"reflect"
	"regexp"
	"strings"
	"testing"

	regexp2 "github.com/dlclark/regexp2/v2"
)

func TestRegexpMethodsMatchStdlib(t *testing.T) {
	std := regexp.MustCompile(`a(x*)b`)
	re := MustCompile(`a(x*)b`)
	input := "zzaxb zzaxxxb zzab"

	if got, want := re.MatchString(input), std.MatchString(input); got != want {
		t.Fatalf("MatchString() = %v, want %v", got, want)
	}
	if got, want := re.Match([]byte(input)), std.Match([]byte(input)); got != want {
		t.Fatalf("Match() = %v, want %v", got, want)
	}
	if got, want := re.MatchReader(strings.NewReader(input)), std.MatchReader(strings.NewReader(input)); got != want {
		t.Fatalf("MatchReader() = %v, want %v", got, want)
	}

	assertEqual(t, "Find", re.Find([]byte(input)), std.Find([]byte(input)))
	assertEqual(t, "FindIndex", re.FindIndex([]byte(input)), std.FindIndex([]byte(input)))
	assertEqual(t, "FindString", re.FindString(input), std.FindString(input))
	assertEqual(t, "FindStringIndex", re.FindStringIndex(input), std.FindStringIndex(input))
	assertEqual(t, "FindReaderIndex", re.FindReaderIndex(strings.NewReader(input)), std.FindReaderIndex(strings.NewReader(input)))
	assertEqual(t, "FindSubmatch", re.FindSubmatch([]byte(input)), std.FindSubmatch([]byte(input)))
	assertEqual(t, "FindSubmatchIndex", re.FindSubmatchIndex([]byte(input)), std.FindSubmatchIndex([]byte(input)))
	assertEqual(t, "FindStringSubmatch", re.FindStringSubmatch(input), std.FindStringSubmatch(input))
	assertEqual(t, "FindStringSubmatchIndex", re.FindStringSubmatchIndex(input), std.FindStringSubmatchIndex(input))
	assertEqual(t, "FindReaderSubmatchIndex", re.FindReaderSubmatchIndex(strings.NewReader(input)), std.FindReaderSubmatchIndex(strings.NewReader(input)))
	assertEqual(t, "FindAll", re.FindAll([]byte(input), -1), std.FindAll([]byte(input), -1))
	assertEqual(t, "FindAllIndex", re.FindAllIndex([]byte(input), -1), std.FindAllIndex([]byte(input), -1))
	assertEqual(t, "FindAllString", re.FindAllString(input, -1), std.FindAllString(input, -1))
	assertEqual(t, "FindAllStringIndex", re.FindAllStringIndex(input, -1), std.FindAllStringIndex(input, -1))
	assertEqual(t, "FindAllSubmatch", re.FindAllSubmatch([]byte(input), -1), std.FindAllSubmatch([]byte(input), -1))
	assertEqual(t, "FindAllSubmatchIndex", re.FindAllSubmatchIndex([]byte(input), -1), std.FindAllSubmatchIndex([]byte(input), -1))
	assertEqual(t, "FindAllStringSubmatch", re.FindAllStringSubmatch(input, -1), std.FindAllStringSubmatch(input, -1))
	assertEqual(t, "FindAllStringSubmatchIndex", re.FindAllStringSubmatchIndex(input, -1), std.FindAllStringSubmatchIndex(input, -1))
}

func TestRegexpMethodsReturnNilWhenNoMatch(t *testing.T) {
	std := regexp.MustCompile(`x+`)
	re := MustCompile(`x+`)
	input := "abc"

	assertEqual(t, "Find", re.Find([]byte(input)), std.Find([]byte(input)))
	assertEqual(t, "FindIndex", re.FindIndex([]byte(input)), std.FindIndex([]byte(input)))
	assertEqual(t, "FindString", re.FindString(input), std.FindString(input))
	assertEqual(t, "FindStringIndex", re.FindStringIndex(input), std.FindStringIndex(input))
	assertEqual(t, "FindSubmatch", re.FindSubmatch([]byte(input)), std.FindSubmatch([]byte(input)))
	assertEqual(t, "FindSubmatchIndex", re.FindSubmatchIndex([]byte(input)), std.FindSubmatchIndex([]byte(input)))
	assertEqual(t, "FindStringSubmatch", re.FindStringSubmatch(input), std.FindStringSubmatch(input))
	assertEqual(t, "FindStringSubmatchIndex", re.FindStringSubmatchIndex(input), std.FindStringSubmatchIndex(input))
	assertEqual(t, "FindAllString", re.FindAllString(input, -1), std.FindAllString(input, -1))
	assertEqual(t, "FindAllString n=0", re.FindAllString("xxx", 0), std.FindAllString("xxx", 0))
}

func TestRegexpMethodsUseByteIndexes(t *testing.T) {
	std := regexp.MustCompile(`é(.)`)
	re := MustCompile(`é(.)`, regexp2.RE2)
	input := "aé𐐷z"

	assertEqual(t, "FindStringIndex", re.FindStringIndex(input), std.FindStringIndex(input))
	assertEqual(t, "FindStringSubmatchIndex", re.FindStringSubmatchIndex(input), std.FindStringSubmatchIndex(input))
	assertEqual(t, "FindAllStringSubmatchIndex", re.FindAllStringSubmatchIndex(input, -1), std.FindAllStringSubmatchIndex(input, -1))
	assertEqual(t, "FindReaderSubmatchIndex", re.FindReaderSubmatchIndex(strings.NewReader(input)), std.FindReaderSubmatchIndex(strings.NewReader(input)))
}

func TestRegexpMethodsHandleInvalidUTF8Bytes(t *testing.T) {
	std := regexp.MustCompile(`.`)
	re := MustCompile(`.`)
	input := []byte{'a', 0xff, 'b'}

	assertEqual(t, "FindAll", re.FindAll(input, -1), std.FindAll(input, -1))
	assertEqual(t, "FindAllIndex", re.FindAllIndex(input, -1), std.FindAllIndex(input, -1))
	assertEqual(t, "FindAllSubmatchIndex", re.FindAllSubmatchIndex(input, -1), std.FindAllSubmatchIndex(input, -1))
}

func TestRegexpMethodsHandleEmptyMatches(t *testing.T) {
	input := "abc"
	for _, pattern := range []string{`a*`, `.*?`, `(|b)`} {
		std := regexp.MustCompile(pattern)
		re := MustCompile(pattern)

		assertEqual(t, pattern+" FindAllString", re.FindAllString(input, -1), std.FindAllString(input, -1))
		assertEqual(t, pattern+" FindAllString limited", re.FindAllString(input, 2), std.FindAllString(input, 2))
		assertEqual(t, pattern+" FindAllStringIndex", re.FindAllStringIndex(input, -1), std.FindAllStringIndex(input, -1))
		assertEqual(t, pattern+" FindAllStringSubmatchIndex", re.FindAllStringSubmatchIndex(input, -1), std.FindAllStringSubmatchIndex(input, -1))
	}
}

func TestWrapAndUnwrap(t *testing.T) {
	base := regexp2.MustCompile(`x`)
	re := Wrap(base)
	if re.Unwrap() != base {
		t.Fatal("Unwrap did not return wrapped regexp")
	}
	if got, want := re.String(), base.String(); got != want {
		t.Fatalf("String() = %q, want %q", got, want)
	}
}

func assertEqual(t *testing.T, name string, got, want any) {
	t.Helper()
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("%s = %#v, want %#v", name, got, want)
	}
}
