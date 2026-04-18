package syntax

import (
	"fmt"
	"strings"
	"testing"

	"slices"
)

func TestMinMaxLength(t *testing.T) {
	scenarios := []struct {
		p             string
		opt, min, max int
	}{
		{`a`, 0, 1, 1},
		{`[^a]`, 0, 1, 1},
		{`[abcdefg]`, 0, 1, 1},
		{`abcd`, 0, 4, 4},
		{`a*`, 0, 0, -1},
		{`a*?`, 0, 0, -1},
		{`a?`, 0, 0, 1},
		{`a??`, 0, 0, 1},
		{`a+`, 0, 1, -1},
		{`a+?`, 0, 1, -1},
		{`(?>a*)a`, 0, 1, -1},
		{`(?>a*)a+`, 0, 1, -1},
		{`(?>a*)a*`, 0, 0, -1},
		{`a{2}`, 0, 2, 2},
		{`a{2}?`, 0, 2, 2},
		{`a{3,17}`, 0, 3, 17},
		{`a{3,17}?`, 0, 3, 17},
		{`[^a]{3,17}`, 0, 3, 17},
		{`[^a]{3,17}?`, 0, 3, 17},
		{`(abcd){5}`, 0, 20, 20},
		{`(abcd|ef){2,6}`, 0, 4, 24},
		{`abcef|de`, 0, 2, 5},
		{`abc(def|ghij)k`, 0, 7, 8},
		{`abc(def|ghij|k||lmnopqrs|t)u`, 0, 4, 12},
		{`(ab)c(def|ghij|k|l|\1|m)n`, 0, 4, -1},
		{`abc|de*f|ghi`, 0, 2, -1},
		{`abc|de+f|ghi`, 0, 3, -1},
		{`abc|(def)+|ghi`, 0, 3, -1},
		{`(abc)+|def`, 0, 3, -1},
		{`\d{1,2}-\d{1,2}-\d{2,4}`, 0, 6, 10},
		{`\d{1,2}-(?>\d{1,2})-\d{2,4}`, 0, 6, 10},
		{`1(?=9)\d`, 0, 2, 2},
		{`1(?!\d)\w`, 0, 2, 2},
		{`a*a*a*a*a*a*a*b*`, 0, 0, -1},
		{`((a{1,2}){4}){3,7}`, 0, 12, 56},
		{`((a{1,2}){4}?){3,7}`, 0, 12, 56},
		{`\b\w{4}\b`, 0, 4, 4},
		{`\b\w{4}\b`, int(ECMAScript), 4, 4},
		{`abcd(?=efgh)efgh`, 0, 8, 8},
		{`abcd(?<=cd)efgh`, 0, 8, 8},
		{`abcd(?!ab)efgh`, 0, 8, 8},
		{`abcd(?<!ef)efgh`, 0, 8, 8},
		{`(a{1073741824}){2}`, 0, 2147483646, -1}, // min length max is bound to int.MaxValue - 1 for convenience in other places where we need to be able to add 1 without risk of overflow
		{`a{1073741824}b{1073741824}`, 0, 2147483646, -1},
		{`((((((((((((((((((((((((((((((ab|cd+)|ef+)|gh+)|ij+)|kl+)|mn+)|op+)|qr+)|st+)|uv+)|wx+)|yz+)|01+)|23+)|45+)|67+)|89+)|AB+)|CD+)|EF+)|GH+)|IJ+)|KL+)|MN+)|OP+)|QR+)|ST+)|UV+)|WX+)|YZ)`, 0, 2, -1},
		{`(YZ+|(WX+|(UV+|(ST+|(QR+|(OP+|(MN+|(KL+|(IJ+|(GH+|(EF+|(CD+|(AB+|(89+|(67+|(45+|(23+|(01+|(yz+|(wx+|(uv+|(st+|(qr+|(op+|(mn+|(kl+|(ij+|(gh+|(ef+|(de+|(a|bc+)))))))))))))))))))))))))))))))`, 0, 1, -1},
		{`a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(ab|cd+)|ef+)|gh+)|ij+)|kl+)|mn+)|op+)|qr+)|st+)|uv+)|wx+)|yz+)|01+)|23+)|45+)|67+)|89+)|AB+)|CD+)|EF+)|GH+)|IJ+)|KL+)|MN+)|OP+)|QR+)|ST+)|UV+)|WX+)|YZ+)`, 0, 3, -1},
		{`(((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))`, 0, 1, 1},
		{`(?(\d)\d{3}|\d)`, 0, 1, 3},
		{`(?(\d{7})\d{3}|\d{2})`, 0, 2, 3},
		{`(?(\d{7})\d{2}|\d{3})`, 0, 2, 3},
		{`(?(\d)\d{3}|\d{2})`, 0, 2, 3},
		{`(?(\d)|\d{2})`, 0, 0, 2},
		{`(?(\d)\d{3})`, 0, 0, 3},
		{`(abc)(?(1)\d{3}|\d{2})`, 0, 5, 6},
		{`(abc)(?(1)\d{2}|\d{3})`, 0, 5, 6},
		{`(abc)(?(1)|\d{2})`, 0, 3, 5},
		{`(abc)(?(1)\d{3})`, 0, 3, 6},
		{`(abc|)`, 0, 0, 3},
		{`(|abc)`, 0, 0, 3},
		{`(?(x)abc|)`, 0, 0, 3},
		{`(?(x)|abc)`, 0, 0, 3},
		{`(?(x)|abc)^\A\G\z\Z$`, 0, 0, 3},
		{`(?(x)|abc)^\A\G\z$\Z`, int(Multiline), 0, 3},
		{`^\A\Gabc`, 0, 3, -1}, // leading anchor currently prevents ComputeMaxLength from being invoked, as it's not needed
		{`^\A\Gabc`, int(Multiline), 3, -1},
		{`abc            def`, int(IgnorePatternWhitespace), 6, 6},
		{`abcdef`, int(RightToLeft), 6, -1},
	}

	for _, s := range scenarios {
		t.Run(s.p, func(t *testing.T) {
			//parse our two strings into trees, debug dump them, expecting equal
			treeA, err := Parse(s.p, RegexOptions(s.opt))
			if err != nil {
				t.Fatalf("failed to parse %s: %v", s.p, err)
			}
			if want, got := s.min, treeA.FindOptimizations.MinRequiredLength; want != got {
				t.Errorf("For Pattern %s Min Length, want %v got %v", s.p, want, got)
			}

			// MaxPossibleLength is currently only computed/stored if there's a trailing End{Z} anchor
			// as the max length is otherwise unused, so add anchors to get calcs to kick in if needed
			if !strings.HasSuffix(s.p, "$") && !strings.HasSuffix(s.p, `\Z`) {
				treeA, _ = Parse(fmt.Sprint("(?:", s.p, ")$"), RegexOptions(s.opt))
			}

			if want, got := s.max, treeA.FindOptimizations.MaxPossibleLength; want != got {
				t.Errorf("For Pattern %s Max Length, want %v got %v", s.p, want, got)
			}
		})
	}
}

func TestPrefixCharSets(t *testing.T) {
	scenarios := []struct {
		p   string
		opt int
		set string
	}{
		// Produce starting sets
		{`a`, 0, "[a]"},
		{`(a)`, 0, "[a]"},
		{`(a)+`, 0, "[a]"},
		{`abc`, 0, "[a]"},
		{`abc`, int(RightToLeft), "[c]"},
		{`abc|def`, int(RightToLeft), "[cf]"},
		{`a?b`, 0, "[ab]"},
		{`a?[bcd]`, 0, "[a-d]"},
		{`a?[bcd]*[xyz]`, 0, "[a-dx-z]"},
		{`[a-c]`, 0, "[a-c]"},
		{`a+b+c+`, 0, "[a]"},
		{`a*b+c+`, 0, "[ab]"},
		{`a*b*c+`, 0, "[a-c]"},
		{`.`, 0, "[^\\n]"},
		{`.|\n`, 0, ""},
		{`[^\n]?[\n]`, 0, ""},
		{`[^a]?[a]`, 0, ""},
		{`(abc)?(?(\1)yes|no)`, 0, "[any]"},
		{`(abc)?(?(xyz)yes|no)`, 0, "[any]"},
		{`[^a-zA-Z0-9_.]`, 0, "[^\\.0-9A-Z_a-z]"},
		// Can't produce starting sets
		{"", 0, ""},
		{`a*`, 0, ""},
		{`a*b*`, 0, ""},
		{`a*b*c*`, 0, ""},
		{`a*|b*`, 0, ""},
		{`(a)*`, 0, ""},
		{`(?:a)*`, 0, ""},
		{`(a*)+`, 0, ""},
		{`(a*)+?`, 0, ""},
		{`[^ab]|a`, 0, ""},
		{`[^ab]|ab`, 0, ""},
		{`[^ab]?[a]`, 0, ""},
		{`[^ab]?(a)+`, 0, ""},
		{`(abc)?\1`, 0, ""},
		{`[abc-[bc]]|[def]`, 0, ""},
		{`[def]|[abc-[bc]]`, 0, ""},
		{`(abc)?(?(\1)d*|f)`, 0, ""},
		{`(abc)?(?(\1)d|f*)`, 0, ""},
		{`(abc)?(?(xyz)d*|f)`, 0, ""},
		{`(abc)?(?(xyz)d|f*)`, 0, ""},
	}

	for _, s := range scenarios {
		t.Run(s.p, func(t *testing.T) {
			// parse our two strings into trees, debug dump them, expecting equal
			tree, err := Parse(s.p, RegexOptions(s.opt))
			if err != nil {
				t.Fatalf("failed to parse %s: %v", s.p, err)
			}
			cs := findFirstCharClass(tree.Root)

			if cs == nil {
				if s.set == "" {
					return
				}
				t.Fatalf("wanted %s but got nil for pattern %s", s.set, s.p)
			}

			if want, got := s.set, cs.String(); want != got {
				t.Fatalf("wanted %v got %v for pattern %s", want, got, s.p)
			}
		})
	}
}

func TestFindPrefixes(t *testing.T) {
	// case-sensitive
	scenarios := []struct {
		p          string
		expected   []string
		ignoreCase bool
	}{
		{"abc", []string{"abc"}, false},
		{"(abc+|bcd+)", []string{"abc", "bcd"}, false},
		{"(ab+c|bcd+)", []string{"ab", "bcd"}, false},
		{"(ab+c|bcd+)*", nil, false},
		{"(ab+c|bcd+)+", []string{"ab", "bcd"}, false},
		{"(ab+c|bcd+){3,5}", []string{"ab", "bcd"}, false},
		{"abc|def", []string{"abc", "def"}, false},
		{"ab{4}c|def{5}|g{2,4}h", []string{"abbbbc", "defffff", "gg"}, false},
		{"abc|def|(ghi|jklm)", []string{"abc", "def", "ghi", "jklm"}, false},
		{"abc[def]ghi", []string{"abcdghi", "abceghi", "abcfghi"}, false},
		{"abc[def]ghi|[jkl]m", []string{"abcdghi", "abceghi", "abcfghi", "jm", "km", "lm"}, false},
		{"agggtaaa|tttaccct", []string{"agggtaaa", "tttaccct"}, false},
		{"[cgt]gggtaaa|tttaccc[acg]", []string{"cgggtaaa", "ggggtaaa", "tgggtaaa", "tttaccca", "tttacccc", "tttacccg"}, false},
		{"a[act]ggtaaa|tttacc[agt]t", []string{"aaggtaaa", "acggtaaa", "atggtaaa", "tttaccat", "tttaccgt", "tttacctt"}, false},
		{"ag[act]gtaaa|tttac[agt]ct", []string{"agagtaaa", "agcgtaaa", "agtgtaaa", "tttacact", "tttacgct", "tttactct"}, false},
		{"agg[act]taaa|ttta[agt]cct", []string{"aggataaa", "aggctaaa", "aggttaaa", "tttaacct", "tttagcct", "tttatcct"}, false},
		{`\b(abc|def)\b`, []string{"abc", "def"}, false},
		{"^(abc|def)$", []string{"abc", "def"}, false},
		{"abcdefg|h", nil, false},
		{"abc[def]ghi|[jkl]", nil, false},
		{"[12][45][789]", []string{"147", "148", "149", "157", "158", "159", "247", "248", "249", "257", "258", "259"}, false},
		{"[12]a[45]b[789]c", []string{"1a4b7c", "1a4b8c", "1a4b9c", "1a5b7c", "1a5b8c", "1a5b9c", "2a4b7c", "2a4b8c", "2a4b9c", "2a5b7c", "2a5b8c", "2a5b9c"}, false},
		{"(abc){3}|(def){3}", []string{"abcabcabc", "defdefdef"}, false},
		{"(abc){4,8}|(def){2,3}", []string{"abcabcabc", "defdef"}, false},
		{"(abc){4,8}|(de+f){2,3}", []string{"abcabcabc", "de"}, false},
		{"(ab{2}c){4,8}|(de+f){2,3}", []string{"abbcabbc", "de"}, false},
		// case-insensitive
		{"[Aa][Bb][Cc]", []string{"abc"}, true},
		{"[Aa][Bbc][Cc]", nil, true},
		{":[Aa]![Bb]@", []string{":a!b@"}, true},
		{"(?i)abc", []string{"abc"}, true},
		{"(?i)(abc+|bcd+)", []string{"abc", "bcd"}, true},
		{"(?i)(ab+c|bcd+)", []string{"ab", "bcd"}, true},
		{"(?i)(ab+c|bcd+)*", nil, true},
		{"(?i)(ab+c|bcd+)+", []string{"ab", "bcd"}, true},
		{"(?i)(ab+c|bcd+){3,5}", []string{"ab", "bcd"}, true},
		{"(?i)abc|def", []string{"abc", "def"}, true},
		{"(?i)ab{4}c|def{5}|g{2,4}h", []string{"abbbbc", "defffff", "gg"}, true},
		{"(?i)(((?>abc)|(?>def)))", []string{"abc", "def"}, true},
		{"(?i)(abc|def|(ghi|jklm))", nil, true},
		{"(?i)(abc|def|(ghi|jlmn))", []string{"abc", "def", "ghi", "jlmn"}, true},
		{"abc", nil, true},
		{"abc|def", nil, true},
		{"abc|def|(ghi|jklm)", nil, true},
		{"://[Aa][Bb]|[Cc]@!", []string{"://ab", "c@!"}, true},
		{"(?i)((abc){4,8}|(def){2,3})", []string{"abcabcab", "defdef"}, true},
		{"(?i)((abc){4,8}|(de+f){2,3})", []string{"abcabcab", "de"}, true},
		{"(?i)((ab{2}c){4,8}|(de+f){2,3})", []string{"abbcabbc", "de"}, true},
	}

	for _, s := range scenarios {
		t.Run(s.p, func(t *testing.T) {
			// parse our two strings into trees, debug dump them, expecting equal
			tree, err := Parse(s.p, 0)
			if err != nil {
				t.Fatalf("failed to parse %s: %v", s.p, err)
			}
			vals := findPrefixes(tree.Root, s.ignoreCase)

			slices.Sort(vals)
			slices.Sort(s.expected)

			if !slices.Equal(vals, s.expected) {
				t.Fatalf("wanted %v got %v for pattern %s ignorecase=%v", s.expected, vals, s.p, s.ignoreCase)
			}
		})
	}
}
