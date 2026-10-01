// Command unicodegen generates supplemental ECMAScript /u property data and
// exact aliases. Categories, scripts, and Go's binary properties aren't copied.
//
// Download https://www.unicode.org/Public/17.0.0/ucd/UCD.zip, then run from
// the repository root:
//
//	go run ./internal/unicodegen -ucd /path/to/UCD.zip
//
// The archive checksum pins all inputs. Generation needs no network access.
package main

import (
	"archive/zip"
	"bufio"
	"bytes"
	"crypto/sha256"
	"flag"
	"fmt"
	"go/format"
	"io"
	"log"
	"maps"
	"os"
	"slices"
	"strconv"
	"strings"
	"unicode"
)

const version = "17.0.0"
const archiveSHA256 = "2066d1909b2ea93916ce092da1c0ee4808ea3ef8407c94b4f14f5b7eb263d28e"

// ECMA-262 restricts property names to its non-binary and binary property tables.
// This list deliberately excludes other UCD properties.
// https://tc39.es/ecma262/#table-binary-unicode-properties
var binaryProperties = strings.Fields(`
ASCII ASCII_Hex_Digit Alphabetic Any Assigned Bidi_Control Bidi_Mirrored
Case_Ignorable Cased Changes_When_Casefolded Changes_When_Casemapped
Changes_When_Lowercased Changes_When_NFKC_Casefolded Changes_When_Titlecased
Changes_When_Uppercased Dash Default_Ignorable_Code_Point Deprecated Diacritic
Emoji Emoji_Component Emoji_Modifier Emoji_Modifier_Base Emoji_Presentation
Extended_Pictographic Extender Grapheme_Base Grapheme_Extend Hex_Digit
IDS_Binary_Operator IDS_Trinary_Operator ID_Continue ID_Start Ideographic
Join_Control Logical_Order_Exception Lowercase Math Noncharacter_Code_Point
Pattern_Syntax Pattern_White_Space Quotation_Mark Radical Regional_Indicator
Sentence_Terminal Soft_Dotted Terminal_Punctuation Unified_Ideograph Uppercase
Variation_Selector White_Space XID_Continue XID_Start
`)

type interval struct{ lo, hi rune }
type ranges []interval

type database struct{ *zip.Reader }

func (db database) records(name string) [][]string {
	f := must(db.Open(name))
	defer f.Close()
	var result [][]string
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line, _, _ := strings.Cut(scanner.Text(), "#")
		if line = strings.TrimSpace(line); line == "" {
			continue
		}
		fields := strings.Split(line, ";")
		for i := range fields {
			fields[i] = strings.TrimSpace(fields[i])
		}
		result = append(result, fields)
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("%s: %v", name, err)
	}
	return result
}

func parseInterval(s string) interval {
	lo, hi, found := strings.Cut(s, "..")
	if !found {
		hi = lo
	}
	return interval{rune(must(strconv.ParseInt(lo, 16, 32))), rune(must(strconv.ParseInt(hi, 16, 32)))}
}

func merge(input ranges) ranges {
	input = slices.Clone(input)
	slices.SortFunc(input, func(a, b interval) int { return int(a.lo - b.lo) })
	var result ranges
	for _, r := range input {
		if len(result) > 0 && r.lo <= result[len(result)-1].hi+1 {
			result[len(result)-1].hi = max(result[len(result)-1].hi, r.hi)
		} else {
			result = append(result, r)
		}
	}
	return result
}

func generate(db database) []byte {
	aliases := map[string]map[string]string{
		"Binary": {}, "Category": {}, "Script": {},
	}
	binary := make(map[string]bool)
	for _, name := range binaryProperties {
		binary[name] = true
		aliases["Binary"][name] = name
	}
	for _, fields := range db.records("PropertyAliases.txt") {
		if binary[fields[1]] {
			for _, alias := range fields {
				// ECMA-262 lists White_Space and space, but not UCD's WSpace.
				if alias != "WSpace" {
					aliases["Binary"][alias] = fields[1]
				}
			}
		}
	}
	// Property and value matching is exact, rather than UAX #44 loose matching.
	// https://tc39.es/ecma262/#sec-runtime-semantics-unicodematchpropertyvalue-p-v
	for _, fields := range db.records("PropertyValueAliases.txt") {
		var kind, canonical string
		switch fields[0] {
		case "gc":
			kind, canonical = "Category", fields[1]
		case "sc":
			kind, canonical = "Script", fields[2]
		default:
			continue
		}
		for _, alias := range fields[1:] {
			aliases[kind][alias] = canonical
		}
	}

	// Go owns general categories, scripts, and built-in binary properties.
	// Only emit binary data unavailable there or in our existing Unicode tables.
	local := make(map[string]bool)
	for _, name := range strings.Fields("Emoji Emoji_Component Emoji_Modifier Emoji_Modifier_Base Emoji_Presentation Extended_Pictographic Math") {
		local[name] = true
	}
	data := make(map[string]ranges)
	for _, name := range []string{
		"PropList.txt", "DerivedCoreProperties.txt", "DerivedNormalizationProps.txt",
		"extracted/DerivedBinaryProperties.txt", "emoji/emoji-data.txt",
	} {
		for _, fields := range db.records(name) {
			property := fields[1]
			if binary[property] && unicode.Properties[property] == nil && !local[property] {
				data[property] = append(data[property], parseInterval(fields[0]))
			}
		}
	}
	data["ASCII"] = ranges{{0, unicode.MaxASCII}}
	data["Any"] = ranges{{0, unicode.MaxRune}}
	for _, name := range binaryProperties {
		if name == "Assigned" || unicode.Properties[name] != nil || local[name] {
			continue
		}
		if _, ok := data[name]; !ok {
			log.Fatalf("missing binary property %q", name)
		}
	}

	// Store only the explicit Script_Extensions overrides. At runtime the base
	// script follows Go's tables, rather than embedding a full copy of each set.
	// Overrides replace the default, rather than simply adding to Script:
	// https://www.unicode.org/reports/tr24/#Script_Extensions_Def
	extensions := make(map[string]ranges)
	var overridden ranges
	for _, fields := range db.records("ScriptExtensions.txt") {
		r := parseInterval(fields[0])
		overridden = append(overridden, r)
		for _, alias := range strings.Fields(fields[1]) {
			name, ok := aliases["Script"][alias]
			if !ok {
				log.Fatalf("unknown script alias %q", alias)
			}
			extensions[name] = append(extensions[name], r)
		}
	}

	var out bytes.Buffer
	fmt.Fprintln(&out, "// Code generated by go run ./internal/unicodegen; DO NOT EDIT.")
	fmt.Fprintf(&out, "// Unicode %s; see internal/unicodegen/UNICODE-LICENSE.txt.\n", version)
	fmt.Fprintln(&out, "// Categories, scripts, and built-in binary properties come from the consumer's Go toolchain.")
	fmt.Fprintln(&out, "package syntax")
	for _, kind := range []string{"Binary", "Category", "Script"} {
		fmt.Fprintf(&out, "\nvar ecma%sAliases = map[string]string{\n", kind)
		for _, key := range slices.Sorted(maps.Keys(aliases[kind])) {
			fmt.Fprintf(&out, "%q: %q,\n", key, aliases[kind][key])
		}
		fmt.Fprintln(&out, "}")
	}

	writeRanges := func(name string, values map[string]ranges) {
		fmt.Fprintf(&out, "\nvar %s = map[string][]SingleRange{\n", name)
		for _, key := range slices.Sorted(maps.Keys(values)) {
			fmt.Fprintf(&out, "%q: {\n", key)
			for _, r := range merge(values[key]) {
				fmt.Fprintf(&out, "{0x%X, 0x%X},\n", r.lo, r.hi)
			}
			fmt.Fprintln(&out, "},")
		}
		fmt.Fprintln(&out, "}")
	}
	fmt.Fprintln(&out, "\n// Binary properties unavailable in Go or our existing package tables.")
	writeRanges("ecmaPropertyRanges", data)
	fmt.Fprintln(&out, "\n// Explicit Script_Extensions membership; all other characters default to Script.")
	writeRanges("ecmaScriptExtensions", extensions)
	fmt.Fprintln(&out, "\n// All characters whose Script_Extensions overrides their Script value.")
	fmt.Fprintln(&out, "var ecmaScriptExtensionOverrides = []SingleRange{")
	for _, r := range merge(overridden) {
		fmt.Fprintf(&out, "{0x%X, 0x%X},\n", r.lo, r.hi)
	}
	fmt.Fprintln(&out, "}")
	log.Printf("Unicode %s supplements: %d binary sets and %d script-extension override sets", version, len(data), len(extensions))
	return must(format.Source(out.Bytes()))
}

func must[T any](value T, err error) T {
	if err != nil {
		log.Fatal(err)
	}
	return value
}

func main() {
	log.SetFlags(0)
	input := flag.String("ucd", "", "path to the Unicode "+version+" UCD.zip")
	output := flag.String("out", "syntax/ecma_unicode_tables.go", "output Go file")
	flag.Parse()
	if *input == "" {
		flag.Usage()
		os.Exit(2)
	}
	f := must(os.Open(*input))
	defer f.Close()
	hash := sha256.New()
	must(io.Copy(hash, f))
	if fmt.Sprintf("%x", hash.Sum(nil)) != archiveSHA256 {
		log.Fatalf("expected the unmodified Unicode %s UCD.zip", version)
	}
	info := must(f.Stat())
	db := database{must(zip.NewReader(f, info.Size()))}
	if err := os.WriteFile(*output, generate(db), 0644); err != nil {
		log.Fatal(err)
	}
}
