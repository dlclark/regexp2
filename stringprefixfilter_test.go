package regexp2

import "testing"

func TestStringIndexPrefixFilter(t *testing.T) {
	filter := stringIndexPrefixFilter("abc", false, 3)
	if filter == nil {
		t.Fatal("expected filter")
	}

	candidateByteIndex, ok := filter("xxabc", 0)
	if !ok {
		t.Fatal("expected candidate")
	}
	if got, want := candidateByteIndex, 2; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}

	candidateByteIndex, ok = filter("xxabcabc", 3)
	if !ok {
		t.Fatal("expected candidate after startAt")
	}
	if got, want := candidateByteIndex, 5; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}

	if _, ok := filter("xxab", 0); ok {
		t.Fatal("unexpected candidate for missing prefix")
	}
}

func TestStringIndexPrefixFilterMinRequiredLengthUsesBytes(t *testing.T) {
	filter := stringIndexPrefixFilter("é", false, 2)
	if filter == nil {
		t.Fatal("expected filter")
	}

	candidateByteIndex, ok := filter("é", 0)
	if !ok {
		t.Fatal("expected candidate")
	}
	if got, want := candidateByteIndex, 0; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}
}

func TestStringIndexPrefixFilterASCIIIgnoreCase(t *testing.T) {
	filter := stringIndexPrefixFilter("abc", true, 3)
	if filter == nil {
		t.Fatal("expected filter")
	}

	candidateByteIndex, ok := filter("xxABc", 0)
	if !ok {
		t.Fatal("expected candidate")
	}
	if got, want := candidateByteIndex, 2; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}
}

func TestStringIndexPrefixFilterRejectsNonASCIIIgnoreCasePrefix(t *testing.T) {
	if filter := stringIndexPrefixFilter("é", true, 1); filter != nil {
		t.Fatal("expected nil filter for non-ASCII ignore-case prefix")
	}
}

func TestStringIndexPrefixesFilter(t *testing.T) {
	filter := stringIndexPrefixesFilter([]string{"xyz", "abc"}, false, 3)
	if filter == nil {
		t.Fatal("expected filter")
	}

	candidateByteIndex, ok := filter("00abcxyz", 0)
	if !ok {
		t.Fatal("expected candidate")
	}
	if got, want := candidateByteIndex, 2; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}

	candidateByteIndex, ok = filter("00abcxyz", 5)
	if !ok {
		t.Fatal("expected candidate after startAt")
	}
	if got, want := candidateByteIndex, 5; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}

	if _, ok := filter("00abxy", 0); ok {
		t.Fatal("unexpected candidate for missing prefixes")
	}
}

func TestStringIndexPrefixesFilterASCIIIgnoreCase(t *testing.T) {
	filter := stringIndexPrefixesFilter([]string{"xyz", "abc"}, true, 3)
	if filter == nil {
		t.Fatal("expected filter")
	}

	candidateByteIndex, ok := filter("00ABcxyz", 0)
	if !ok {
		t.Fatal("expected candidate")
	}
	if got, want := candidateByteIndex, 2; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}
}

func TestStringIndexPrefixesFilterRejectsNonASCIIIgnoreCasePrefix(t *testing.T) {
	if filter := stringIndexPrefixesFilter([]string{"abc", "é"}, true, 1); filter != nil {
		t.Fatal("expected nil filter for non-ASCII ignore-case prefix")
	}
}

func TestFindStringPrefixCandidateFallbacks(t *testing.T) {
	re := &Regexp{
		stringPrefixFilter: func(input string, startAt int) (candidateByteIndex int, ok bool) {
			return 1, true
		},
	}

	candidateByteIndex, ok := re.findStringPrefixCandidate("éabc", 0)
	if !ok {
		t.Fatal("expected fallback candidate")
	}
	if got, want := candidateByteIndex, 0; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}

	re.stringPrefixFilter = func(input string, startAt int) (candidateByteIndex int, ok bool) {
		return startAt - 1, true
	}
	candidateByteIndex, ok = re.findStringPrefixCandidate("abc", 1)
	if !ok {
		t.Fatal("expected fallback candidate")
	}
	if got, want := candidateByteIndex, 1; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}
}

func TestFindStringPrefixCandidateDisabledForRightToLeft(t *testing.T) {
	re := &Regexp{
		options: RightToLeft,
		stringPrefixFilter: func(input string, startAt int) (candidateByteIndex int, ok bool) {
			t.Fatal("right-to-left should not call stringPrefixFilter")
			return 0, false
		},
	}

	candidateByteIndex, ok := re.findStringPrefixCandidate("abc", 3)
	if !ok {
		t.Fatal("expected candidate")
	}
	if got, want := candidateByteIndex, 3; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}
}

func TestFindStringMatchStart(t *testing.T) {
	re := &Regexp{}

	candidateByteIndex, ok, err := re.findStringMatchStart("abc", -1)
	if err != nil {
		t.Fatalf("findStringMatchStart failed: %v", err)
	}
	if !ok {
		t.Fatal("expected candidate")
	}
	if got, want := candidateByteIndex, 0; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}

	re.options = RightToLeft
	candidateByteIndex, ok, err = re.findStringMatchStart("abc", -1)
	if err != nil {
		t.Fatalf("findStringMatchStart failed: %v", err)
	}
	if !ok {
		t.Fatal("expected candidate")
	}
	if got, want := candidateByteIndex, 3; got != want {
		t.Fatalf("candidateByteIndex = %d, want %d", got, want)
	}

	if _, _, err := re.findStringMatchStart("abc", 4); err == nil {
		t.Fatal("expected startAt too large error")
	}
	if _, _, err := re.findStringMatchStart("aé", 2); err == nil {
		t.Fatal("expected invalid rune boundary error")
	}
}
