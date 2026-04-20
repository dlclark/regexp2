package regexp2

import (
	"slices"
	"testing"
)

func TestPooledSliceBuffersSortsSizeClasses(t *testing.T) {
	pool := newPooledSliceBuffers[byte](64, 4, 16)
	if !slices.Equal(pool.sizes, []int{4, 16, 64}) {
		t.Fatalf("sizes = %v, want [4 16 64]", pool.sizes)
	}
}

func TestPooledSliceBuffersPoolIndex(t *testing.T) {
	pool := newPooledSliceBuffers[byte](4, 16)

	tests := []struct {
		name       string
		neededSize int
		maxSize    int
		want       int
	}{
		{name: "disabled", neededSize: 1, maxSize: 0, want: -1},
		{name: "first class", neededSize: 4, maxSize: 4, want: 0},
		{name: "next class", neededSize: 5, maxSize: 16, want: 1},
		{name: "max below class", neededSize: 5, maxSize: 4, want: -1},
		{name: "too large", neededSize: 17, maxSize: -1, want: -1},
		{name: "unbounded class", neededSize: 16, maxSize: -1, want: 1},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := pool.poolIndex(tt.neededSize, tt.maxSize); got != tt.want {
				t.Fatalf("poolIndex(%d, %d) = %d, want %d", tt.neededSize, tt.maxSize, got, tt.want)
			}
		})
	}
}

func TestPooledSliceBuffersGet(t *testing.T) {
	pool := newPooledSliceBuffers[rune](4, 16)

	buf, pooled := pool.get(5, 4)
	if pooled != nil {
		t.Fatalf("buffer was pooled despite max below required class")
	}
	if len(buf) != 5 || cap(buf) != 5 {
		t.Fatalf("non-pooled buffer len/cap = %d/%d, want 5/5", len(buf), cap(buf))
	}

	buf, pooled = pool.get(5, 16)
	if pooled == nil {
		t.Fatalf("buffer was not pooled within size limit")
	}
	if len(buf) != 5 || cap(buf) != 16 {
		t.Fatalf("pooled buffer len/cap = %d/%d, want 5/16", len(buf), cap(buf))
	}
}

func TestPooledSliceBuffersPutReusesMatchingClass(t *testing.T) {
	pool := newPooledSliceBuffers[byte](4, 16)
	buf := make([]byte, 2, 4)

	pool.put(&buf)

	got, pooled := pool.get(3, 4)
	if pooled == nil {
		t.Fatalf("buffer was not pooled")
	}
	if cap(got) != 4 {
		t.Fatalf("buffer cap = %d, want 4", cap(got))
	}
}

func TestPooledSliceBuffersPutDropsMismatchedCapacity(t *testing.T) {
	pool := newPooledSliceBuffers[byte](4, 16)
	buf := make([]byte, 2, 8)

	pool.put(&buf)

	got, pooled := pool.get(5, 16)
	if pooled == nil {
		t.Fatalf("buffer was not pooled")
	}
	if cap(got) != 16 {
		t.Fatalf("buffer cap = %d, want 16", cap(got))
	}
}

func TestPooledReplaceBufferHonorsSizeLimit(t *testing.T) {
	buf, pooled := getPooledReplaceBuffer(100, 8)
	if pooled != nil {
		t.Fatalf("replace buffer was pooled despite max below smallest size class")
	}
	if buf.Cap() < 100 {
		t.Fatalf("replace buffer cap = %d, want at least requested size", buf.Cap())
	}
}
