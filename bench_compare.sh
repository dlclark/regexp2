#!/usr/bin/env bash
#
# bench_compare.sh — compare benchmark memory and performance between two git refs.
#
# This script backports the benchmark files from the head ref onto the base ref
# so that the same benchmarks run on both old and new code, producing a true
# apples-to-apples comparison via benchstat.
#
# Usage:
#   ./bench_compare.sh [base_ref] [head_ref] [count]
#
# Defaults:
#   base_ref = HEAD~1   (pre-change)
#   head_ref = HEAD     (post-change)
#   count    = 6        (iterations per benchmark for statistical significance)
#
# Requires: go, benchstat (golang.org/x/perf/cmd/benchstat)
#
# Output files (in .bench/):
#   base.txt       — raw benchmark output for base ref
#   head.txt       — raw benchmark output for head ref
#   comparison.txt — benchstat comparison
#
set -euo pipefail

BASE_REF="${1:-HEAD~1}"
HEAD_REF="${2:-HEAD}"
COUNT="${3:-6}"
BENCH_PATTERN="${BENCH_PATTERN:-.}"
BENCH_TIMEOUT="${BENCH_TIMEOUT:-20m}"

BENCHSTAT="$(command -v benchstat 2>/dev/null || echo "${GOPATH:-$HOME/go}/bin/benchstat")"
if [ ! -x "$BENCHSTAT" ]; then
    echo "ERROR: benchstat not found. Install with: go install golang.org/x/perf/cmd/benchstat@latest"
    exit 1
fi

BENCH_DIR=".bench"
mkdir -p "$BENCH_DIR"

ORIG_BRANCH="$(git rev-parse --abbrev-ref HEAD 2>/dev/null || git rev-parse --short HEAD)"
BASE_SHA="$(git rev-parse --short "$BASE_REF")"
HEAD_SHA="$(git rev-parse --short "$HEAD_REF")"

echo "=== Benchmark Memory Comparison ==="
echo "  base: $BASE_REF ($BASE_SHA)"
echo "  head: $HEAD_REF ($HEAD_SHA)"
echo "  count: $COUNT iterations per benchmark"
echo "  pattern: $BENCH_PATTERN"
echo ""

# --- Collect benchmark file list from head ref ---
# These are the test files we'll backport onto the base ref.
BENCH_FILES=(
    regexp_workload_benchmark_test.go
    regexp_performance_test.go
)

# Stash any uncommitted changes
STASH_NEEDED=false
if ! git diff --quiet || ! git diff --cached --quiet; then
    echo "Stashing uncommitted changes..."
    git stash push -m "bench_compare: auto-stash" --quiet
    STASH_NEEDED=true
fi

cleanup() {
    # Delete temp branch if it exists
    git checkout --quiet "$ORIG_BRANCH" 2>/dev/null || true
    git branch -D bench-compare-tmp 2>/dev/null || true
    if [ "$STASH_NEEDED" = true ]; then
        echo "Restoring stashed changes..."
        git stash pop --quiet 2>/dev/null || true
    fi
}
trap cleanup EXIT

# --- Save benchmark files from head ref ---
echo ">>> Saving benchmark files from head ref ($HEAD_SHA)..."
for f in "${BENCH_FILES[@]}"; do
    if git show "$HEAD_SHA:$f" > "$BENCH_DIR/$f.head" 2>/dev/null; then
        echo "    saved $f"
    else
        echo "    $f not found in $HEAD_SHA, skipping"
    fi
done
echo ""

# --- Create a temp branch at base ref and backport benchmarks ---
echo ">>> Creating baseline branch at $BASE_REF ($BASE_SHA)..."
git checkout --quiet -b bench-compare-tmp "$BASE_SHA"

# Remove any untracked test files that reference APIs not in the base
# (e.g. regexp_indices_test.go referencing FindStringMatchIndices)
for f in *_test.go; do
    if [ -f "$f" ] && ! git ls-files --error-unmatch "$f" >/dev/null 2>&1; then
        echo "    removing untracked test file: $f"
        rm "$f"
    fi
done

# Copy benchmark files from head onto base code
for f in "${BENCH_FILES[@]}"; do
    if [ -f "$BENCH_DIR/$f.head" ]; then
        cp "$BENCH_DIR/$f.head" "$f"
        echo "    backported $f"
    fi
done

# Quick compile check — strip benchmarks that reference missing APIs
if ! go test -run='^$' -bench=NONE ./... >/dev/null 2>&1; then
    echo ""
    echo "WARNING: benchmark files don't compile on base ref."
    echo "This likely means some benchmarks reference APIs added in the head ref."
    echo "Attempting to build with available benchmarks..."
    echo ""
    # Show the actual error for debugging
    go test -run='^$' -bench=NONE ./... 2>&1 || true
    echo ""
    echo "Please manually create a compatible benchmark file for the base ref."
    exit 1
fi

echo ""
echo ">>> Running benchmarks on base (count=$COUNT)..."
go test -run='^$' -bench="$BENCH_PATTERN" -benchmem -count="$COUNT" -timeout="$BENCH_TIMEOUT" 2>&1 | tee "$BENCH_DIR/base.txt"
echo ""

# --- Run benchmarks on head ref ---
echo ">>> Switching to head ref: $HEAD_REF ($HEAD_SHA)..."
git checkout --quiet "$ORIG_BRANCH"
git branch -D bench-compare-tmp --quiet

echo ">>> Running benchmarks on head (count=$COUNT)..."
if [ "$STASH_NEEDED" = true ]; then
    # Apply stash temporarily for the head run (don't pop yet — cleanup does that)
    git stash apply --quiet 2>/dev/null || true
fi
go test -run='^$' -bench="$BENCH_PATTERN" -benchmem -count="$COUNT" -timeout="$BENCH_TIMEOUT" 2>&1 | tee "$BENCH_DIR/head.txt"
echo ""

# --- Compare ---
echo ">>> Running benchstat comparison..."
echo ""
"$BENCHSTAT" "$BENCH_DIR/base.txt" "$BENCH_DIR/head.txt" | tee "$BENCH_DIR/comparison.txt"

echo ""
echo "=== Memory Regression Check ==="
echo ""

# Scan the B/op section for regressions > 10%
IN_BOP_SECTION=false
REGRESSIONS=0
while IFS= read -r line; do
    # Detect B/op section header
    if echo "$line" | grep -q 'B/op'; then
        if echo "$line" | grep -q 'vs base'; then
            IN_BOP_SECTION=true
            continue
        fi
    fi
    # Detect section boundary (empty line or new header)
    if [ -z "$line" ] || echo "$line" | grep -qE '^\s*(allocs/op|sec/op|B/s)'; then
        IN_BOP_SECTION=false
    fi

    if [ "$IN_BOP_SECTION" = true ]; then
        if echo "$line" | grep -qE '\+[0-9]+\.[0-9]+%'; then
            pct="$(echo "$line" | grep -oE '\+[0-9]+\.[0-9]+%' | head -1 | tr -d '+%')"
            if awk "BEGIN {exit !($pct > 10.0)}"; then
                echo "  REGRESSION: $line"
                REGRESSIONS=$((REGRESSIONS + 1))
            fi
        fi
    fi
done < "$BENCH_DIR/comparison.txt"

if [ "$REGRESSIONS" -gt 0 ]; then
    echo ""
    echo "WARNING: $REGRESSIONS potential memory regression(s) detected (>10% B/op increase)."
    echo "Review $BENCH_DIR/comparison.txt for details."
    exit 1
else
    echo "  No significant memory regressions detected (all B/op changes <= 10%)."
    echo ""
    echo "Full results saved to:"
    echo "  $BENCH_DIR/base.txt"
    echo "  $BENCH_DIR/head.txt"
    echo "  $BENCH_DIR/comparison.txt"
fi
