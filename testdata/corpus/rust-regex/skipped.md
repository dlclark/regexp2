# Skipped Rust Regex Corpus Sections

Raw corpus source: https://github.com/rust-lang/regex/tree/master/testdata

The raw TOML corpus files are checked in unchanged. The runner skips these categories at execution time:

- `utf8 = false`: byte/invalid-UTF8 mode is not exposed by regexp2.
- `unicode = false`: Rust ASCII-only behavior is not supported.
- `regex-lite.toml`: Rust regex-lite ASCII-only behavior is not supported.
- `anchored = true`: Rust anchored automata mode is not exposed by regexp2.
- `bounds = [...]`: bounded search end is not exposed by regexp2.
- `line-terminator = ...` and `(?mR)`: Rust CRLF/line-terminator mode is not supported.
- `(?U)` and `(?-U)`: Rust swap-greed mode is not supported.
- `\b{start}`, `\b{end}`, `\b{start-half}`, and `\b{end-half}`: Rust special word-boundary assertions are not supported.
- `search-kind = "overlapping"` and `search-kind = "earliest"`: these Rust search modes are not exposed by regexp2.
- `match-kind = "all"`: Rust all-match mode is not equivalent to normal leftmost-first regexp matching.
- Regex set tests with expected `id = ...` output: Rust regex-set id reporting is not equivalent to a single alternation regexp.
- Empty regex set tests: an empty Rust regex set has no single-regexp alternation equivalent.

Notes:

- Regex set tests that behave like normal leftmost-first single-regexp matching are projected as `(?:regex1)|(?:regex2)|...` by the runner. The raw corpus input is not modified.
- Compile-failure tests with `compiles = false` are included.
- regexp2 Unicode character classes are backed by Go's `unicode.Scripts`, `unicode.Categories`, `unicode.CategoryAliases`, and `unicode.Properties` tables from Unicode 15.0.0 in the supported Go toolchain, plus package-local property and property-value tables generated from Unicode 17.0.0 UCD emoji and text-segmentation data.
