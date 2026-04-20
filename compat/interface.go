package compat

import (
	"io"
	"regexp"
)

// Matcher is the common matching interface implemented by regexp.Regexp and
// this package's Regexp adapter.
//
// It includes the standard-library matching surface: Match, MatchString,
// MatchReader, and all Find(All)?(String)?(Submatch)?(Index)? methods.
type Matcher interface {
	Match(b []byte) bool
	MatchString(s string) bool
	MatchReader(r io.RuneReader) bool

	Find(b []byte) []byte
	FindIndex(b []byte) []int
	FindReaderIndex(r io.RuneReader) []int
	FindReaderSubmatchIndex(r io.RuneReader) []int
	FindString(s string) string
	FindStringIndex(s string) []int
	FindStringSubmatch(s string) []string
	FindStringSubmatchIndex(s string) []int
	FindSubmatch(b []byte) [][]byte
	FindSubmatchIndex(b []byte) []int

	FindAll(b []byte, n int) [][]byte
	FindAllIndex(b []byte, n int) [][]int
	FindAllString(s string, n int) []string
	FindAllStringIndex(s string, n int) [][]int
	FindAllStringSubmatch(s string, n int) [][]string
	FindAllStringSubmatchIndex(s string, n int) [][]int
	FindAllSubmatch(b []byte, n int) [][][]byte
	FindAllSubmatchIndex(b []byte, n int) [][]int
}

var (
	_ Matcher = (*regexp.Regexp)(nil)
	_ Matcher = (*Regexp)(nil)
)
