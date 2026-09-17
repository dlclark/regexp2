package regexp2

import "testing"

// These tests pin IgnoreCase matching to .NET's case equivalences, which group
// characters by their invariant lowercase form. That relation is narrower than
// Unicode simple case folding: folding also relates LATIN SMALL LETTER LONG S
// to 's' and GREEK SMALL LETTER FINAL SIGMA to the other sigmas, and .NET
// treats none of those as equivalent.
type ignoreCaseCase struct {
	name  string
	pat   string
	input string
	// want is the expected matched text; an empty string means no match.
	want string
	// wantIdx is the rune index of the expected match, ignored when want is "".
	wantIdx int
}

func runIgnoreCaseCases(t *testing.T, cases []ignoreCaseCase) {
	t.Helper()
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			re := MustCompile(c.pat, IgnoreCase)
			m, err := re.FindStringMatch(c.input)
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if c.want == "" {
				if m != nil {
					t.Fatalf("pattern %q on %q: got match %q at %v, wanted no match", c.pat, c.input, m.String(), m.RuneIndex)
				}
				return
			}
			if m == nil {
				t.Fatalf("pattern %q on %q: got no match, wanted %q at %v", c.pat, c.input, c.want, c.wantIdx)
			}
			if m.String() != c.want || m.RuneIndex != c.wantIdx {
				t.Fatalf("pattern %q on %q: got %q at %v, wanted %q at %v", c.pat, c.input, m.String(), m.RuneIndex, c.want, c.wantIdx)
			}
		})
	}
}

// Characters that simple case folding relates but that do not share an
// invariant lowercase form are not equivalent in .NET.
func TestIgnoreCase_EquivalenceClasses(t *testing.T) {
	runIgnoreCaseCases(t, []ignoreCaseCase{
		// U+017F LATIN SMALL LETTER LONG S lowercases to itself, so .NET never
		// relates it to 's' or 'S' the way unicode.SimpleFold does.
		{name: "long_s_is_not_s", pat: "s", input: "ſ"},
		{name: "s_is_not_long_s", pat: "ſ", input: "s"},
		{name: "capital_s_is_not_long_s", pat: "ſ", input: "S"},
		// U+03C2 GREEK SMALL LETTER FINAL SIGMA lowercases to itself. In .NET
		// it is its own class, distinct from U+03C3 and U+03A3.
		{name: "final_sigma_is_not_small_sigma", pat: "σ", input: "ς"},
		{name: "small_sigma_is_not_final_sigma", pat: "ς", input: "σ"},
		{name: "capital_sigma_is_not_final_sigma", pat: "Σ", input: "ς"},
		{name: "final_sigma_is_not_capital_sigma", pat: "ς", input: "Σ"},
	})
}

// Characters keep the equivalences they do have.
func TestIgnoreCase_EquivalencesRetained(t *testing.T) {
	runIgnoreCaseCases(t, []ignoreCaseCase{
		// U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE is left alone by the
		// invariant culture, so it still has to match itself.
		{name: "dotted_capital_i_in_a_class_matches_itself", pat: "[İ]", input: "İ", want: "İ"},
	})
}

// A subtracted set narrows the set it is subtracted from, so .NET folds the
// subtracted members too: [a-z-[aeiou]] excludes 'A' as well as 'a'.
func TestIgnoreCase_ClassSubtraction(t *testing.T) {
	runIgnoreCaseCases(t, []ignoreCaseCase{
		{name: "uppercase_vowel_excluded", pat: "[a-z-[aeiou]]", input: "AB", want: "B", wantIdx: 1},
		{name: "uppercase_vowel_excluded_from_uppercase_range", pat: "[A-Z-[AEIOU]]", input: "AB", want: "B", wantIdx: 1},
		{name: "single_subtracted_char_excluded", pat: "[a-z-[b]]", input: "BA", want: "A", wantIdx: 1},
		// Subtraction nests, and every level folds.
		{name: "nested_subtraction", pat: "[a-z-[a-c-[b]]]", input: "AB", want: "B", wantIdx: 1},
		{name: "mixed_case_range_and_subtraction", pat: "[a-zA-Z-[aeiouAEIOU]]", input: "AB", want: "B", wantIdx: 1},
		{name: "subtracted_vowel_has_no_match", pat: "[a-z-[aeiou]]", input: "A"},
		{name: "subtracted_vowel_has_no_match_uppercase", pat: "[A-Z-[AEIOU]]", input: "E"},
		// [a-z] never picks up U+017F, so subtracting from it cannot either.
		{name: "long_s_is_not_subtracted_by_a_z", pat: "[a-z-[aeiou]]", input: "ſ"},
	})
}

// Subtraction composes with unicode categories the same way.
func TestIgnoreCase_CategorySubtraction(t *testing.T) {
	runIgnoreCaseCases(t, []ignoreCaseCase{
		// Subtracting [a-z] removes 'A'-'Z' as well, so \p{Ll} minus [a-z]
		// matches neither case of an ASCII letter.
		{name: "lowercase_letters_minus_ascii_excludes_uppercase", pat: `[\p{Ll}-[a-z]]`, input: "A"},
		{name: "lowercase_letters_minus_ascii_excludes_uppercase_z", pat: `[\p{Ll}-[a-z]]`, input: "Z"},
		// U+212A KELVIN SIGN lowercases to 'k', so subtracting [A-Z] takes it
		// out of \p{Lu} under IgnoreCase.
		{name: "uppercase_letters_minus_ascii_excludes_kelvin", pat: `[\p{Lu}-[A-Z]]`, input: "K"},
	})
}

// Ranges fold each member individually rather than by lowercasing the bounds,
// so a range never acquires a character outside its own equivalence classes.
func TestIgnoreCase_Ranges(t *testing.T) {
	runIgnoreCaseCases(t, []ignoreCaseCase{
		// U+0100-U+0200 contains U+0130, whose Unicode lowercase is 'i'. .NET
		// does not lowercase it, so the range must not reach ASCII 'i' or 'I'.
		{name: "latin_extended_range_does_not_reach_ascii_i", pat: "[Ā-Ȁ]", input: "i"},
		{name: "latin_extended_range_does_not_reach_ascii_capital_i", pat: "[Ā-Ȁ]", input: "I"},
		// Simple case folding would pull U+017F into [a-z] via 's'.
		{name: "ascii_range_does_not_reach_long_s", pat: "[a-z]", input: "ſ"},
		{name: "uppercase_ascii_range_does_not_reach_long_s", pat: "[A-Z]", input: "ſ"},
		{name: "latin_extended_range_minus_a_macron_excludes_ascii_i", pat: "[Ā-Ȁ-[ā]]", input: "i"},
	})
}

// Backreferences compare captured text with the same equivalence classes.
func TestIgnoreCase_Backreferences(t *testing.T) {
	runIgnoreCaseCases(t, []ignoreCaseCase{
		// U+0130 and 'i' are different classes, so neither order backreferences.
		{name: "dot_backreference_does_not_fold_dotted_capital_i", pat: `(.)\1`, input: "iİ"},
		{name: "dot_backreference_does_not_fold_dotted_capital_i_reversed", pat: `(.)\1`, input: "İi"},
		{name: "literal_backreference_does_not_fold_dotted_capital_i", pat: `(i)\1`, input: "iİ"},
		{name: "dotted_capital_i_backreference_does_not_fold_to_i", pat: "(İ)\\1", input: "İi"},
		{name: "named_backreference_does_not_fold_dotted_capital_i", pat: `(?<x>.)\k<x>`, input: "iİ"},
		{name: "named_backreference_does_not_fold_reversed", pat: `(?<x>.)\k<x>`, input: "İi"},
	})
}
