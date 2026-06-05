# Skipped RE2 Corpus Sections

Raw corpus source: https://github.com/golang/go/blob/master/src/regexp/testdata/basic.dat

The raw corpus file is checked in unchanged as `basic.dat`. The runner skips these categories at execution time:

- `B`-only rows: POSIX BRE mode is not supported and is not exposed by regexp2.
- Compile-error expectation rows such as `BADBR`: these are parser diagnostics rather than matching-correctness cases.
