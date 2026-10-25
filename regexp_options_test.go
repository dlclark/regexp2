package regexp2

import "testing"

func TestIgnoreCase_BackreferenceFolding(t *testing.T) {
	re := MustCompile(`^(s)\1$`, IgnoreCase)
	if got, err := re.MatchString("sſ"); err != nil || !got {
		t.Fatalf("backreference on sſ = %v, %v; want true, nil", got, err)
	}
	for _, options := range []RegexOptions{None, Unicode, RE2, RE2 | Unicode} {
		for _, direction := range []RegexOptions{None, RightToLeft} {
			for _, form := range []struct {
				ltr, rtl   string
				options    RegexOptions
				ignoreCase bool
			}{
				{`^([\s\S]*)#\1$`, `^\1#([\s\S]*)$`, IgnoreCase, true},
				{`^(?<v>[\s\S]*)#\k<v>$`, `^\k<v>#(?<v>[\s\S]*)$`, IgnoreCase, true},
				{`^([\s\S]*)#(?i:\1)$`, `^(?i:\1)#([\s\S]*)$`, None, true},
				{`^(?i:([\s\S]*))#\1$`, `^\1#(?i:([\s\S]*))$`, None, false},
				{`^([\s\S]*)#(?-i:\1)$`, `^(?-i:\1)#([\s\S]*)$`, IgnoreCase, false},
			} {
				pattern := form.ltr
				if direction != 0 {
					pattern = form.rtl
				}
				opts := options | direction | form.options
				re := MustCompile(pattern, opts)
				for _, tc := range []struct {
					a, b string
					fold bool
				}{
					{"s", "ſ", true}, {"Σ", "ς", true}, {"σ", "ς", true},
					{"k", "K", true}, {"ß", "ẞ", true}, {"\u0345", "Ι", true},
					{"\u1f80", "\u1f88", true}, {"𐐀", "𐐨", true},
					{"i", "İ", false}, {"I", "ı", false}, {"ß", "ss", false},
					{"é", "e\u0301", false}, {"KΣs𐐀!", "kςſ𐐨!", true},
					{"KΣs!", "kςſ?", false}, {"s", "ſs", false},
					{"same!", "same!", true}, {"", "", true}, {"s", "", false},
				} {
					want := tc.a == tc.b || form.ignoreCase && tc.fold
					for _, input := range []string{tc.a + "#" + tc.b, tc.b + "#" + tc.a} {
						if got, err := re.MatchString(input); err != nil || got != want {
							t.Errorf("%s options %v on %q = %v, %v; want %v", pattern, opts, input, got, err, want)
						}
					}
				}
			}
		}
	}
}

func TestIgnoreCase_BackreferenceBacktracking(t *testing.T) {
	for _, tc := range []struct{ pattern, input, capture string }{
		{`^(s)\1+$`, "sſS", "s"},
		{`^(.+)\1$`, "sΣſς", "sΣ"},
		{`^(.+?)\1$`, "sΣſς", "sΣ"},
		{`^(sΣ)(?:\1x|\1y)$`, "sΣſςy", "sΣ"},
		{`^(sΣ)(?:\1|ſσ?)$`, "sΣſ", "sΣ"},
	} {
		re := MustCompile(tc.pattern, IgnoreCase)
		m, err := re.FindStringMatch(tc.input)
		if err != nil || m == nil {
			t.Fatalf("%s on %q = %v, %v; want a match", tc.pattern, tc.input, m, err)
		}
		if m.String() != tc.input || m.GroupByNumber(1).String() != tc.capture {
			t.Errorf("%s on %q: match %q, capture %q; want capture %q", tc.pattern, tc.input, m.String(), m.GroupByNumber(1).String(), tc.capture)
		}
	}
}

func TestIgnoreCase_Simple(t *testing.T) {
	r := MustCompile("aaamatch thisbbb", IgnoreCase)
	m, err := r.FindStringMatch("AaAMatch thisBBb")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if m == nil {
		t.Fatalf("no match when one was expected")
	}
	if want, got := "AaAMatch thisBBb", m.String(); want != got {
		t.Fatalf("group 0 wanted '%v', got '%v'", want, got)
	}
}

func TestIgnoreCase_Inline(t *testing.T) {
	r := MustCompile("aaa(?i:match this)bbb")
	m, err := r.FindStringMatch("aaaMaTcH ThIsbbb")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if m == nil {
		t.Fatalf("no match when one was expected")
	}
	if want, got := "aaaMaTcH ThIsbbb", m.String(); want != got {
		t.Fatalf("group 0 wanted '%v', got '%v'", want, got)
	}
}

func TestIgnoreCase_Revert(t *testing.T) {

	r := MustCompile("aaa(?-i:match this)bbb", IgnoreCase)
	m, err := r.FindStringMatch("AaAMatch thisBBb")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if m != nil {
		t.Fatalf("had a match but expected no match")
	}
}

func TestIgnoreCase_NonECMAScriptModeIsolation(t *testing.T) {
	// Default mode combines Go's simple folding with Unicode word classes.
	// ECMAScript's legacy folding and ASCII word rules must not leak into it,
	// including when the Unicode option is set on its own.
	for _, options := range []RegexOptions{
		IgnoreCase, IgnoreCase | Unicode,
		IgnoreCase | RightToLeft, IgnoreCase | Unicode | RightToLeft,
	} {
		for _, tc := range []struct{ pattern, matches, misses string }{
			{`k`, "kK\u212a", "s"},
			{`[a-z]`, "kK\u212a\u017f", "!"},
			{`Σ`, "Σσς", "s"},
			{`[Σ]`, "Σσς", "s"},
			{`\u212a`, "kK\u212a", "s"},
			{`[\u212a]`, "kK\u212a", "s"},
			{`[\u1f80]`, "\u1f80\u1f88", "a"},
			{`\w`, "IiK\u212aéα\u0130\u0131\u017f", "!"},
			{`[\w]`, "IiK\u212aéα\u0130\u0131\u017f", "!"},
			{`\W`, "!", "IiK\u212aéα\u0130\u0131\u017f"},
			{`[\W]`, "!", "IiK\u212aéα\u0130\u0131\u017f"},
			{`[^\W]`, "IiK\u212aéα\u0130\u0131\u017f", "!"},
		} {
			re := MustCompile(`^(?:`+tc.pattern+`)$`, options)
			for _, inputs := range []struct {
				chars string
				want  bool
			}{{tc.matches, true}, {tc.misses, false}} {
				for _, r := range inputs.chars {
					if got, err := re.MatchString(string(r)); err != nil || got != inputs.want {
						t.Errorf("%s options %v on %U = %v, %v; want %v", tc.pattern, options, r, got, err, inputs.want)
					}
				}
			}
		}
	}
}
