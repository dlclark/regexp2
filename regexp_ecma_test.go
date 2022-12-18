package regexp2_test

import (
	"testing"

	"github.com/dlclark/regexp2"
	"github.com/stretchr/testify/require"
)

func TestECMA_charset(t *testing.T) {
	tests := map[string]struct {
		expr    string
		data    string
		opt     regexp2.RegexOptions
		want    []string
		wantErr string
	}{
		"basic": {
			expr: `[a-c]`,
			data: "abcd",
			want: []string{"a", "b", "c"},
		},
		"in-range": {
			expr: `[a-\s\b]`,
			data: "a-b cd",
			want: []string{"a", "-", " "},
		},
		"space": {
			expr: `[a-\s]`,
			data: "a-b cd",
			want: []string{"a", "-", " "},
		},
		"word": {
			expr: `[a-\w]`,
			data: "a-b cd",
			want: []string{"a", "-", "b", "c", "d"},
		},
		"digit": {
			expr: `[a-\d]`,
			data: "a-b1 cd",
			want: []string{"a", "-", "1"},
		},
		"slash-p": {
			expr: `[a-\p]`,
			data: "a-bq cd",
			want: []string{"a", "b", "c", "d"},
		},
		"slash-p-literal": {
			expr: `[a-\p{x}]`,
			data: "a-bq cdx",
			want: []string{"a", "b", "c", "d", "x"},
		},
		"invalid-unicode": {
			expr:    `[a-\p]`,
			opt:     regexp2.Unicode,
			wantErr: "error parsing regexp: incomplete \\p{X} character escape in `[a-\\p]`",
		},
		"invalid-unicode-letter": {
			expr:    `[a-\p{L}]`,
			opt:     regexp2.Unicode,
			wantErr: "error parsing regexp: cannot create range with shorthand escape sequence \\p in `[a-\\p{L}]`",
		},
		"invalid-slash-P": {
			expr:    `[a-\P]`,
			wantErr: "error parsing regexp: cannot create range with shorthand escape sequence \\P in `[a-\\P]`",
		},
		"invalid-space": {
			expr:    `[\s-z]`,
			wantErr: "error parsing regexp: cannot create range with shorthand escape sequence \\s in `[\\s-z]`",
		},
		"invalid-word": {
			expr:    `[\w-z]`,
			wantErr: "error parsing regexp: cannot create range with shorthand escape sequence \\w in `[\\w-z]`",
		},
		"invalid-digit": {
			expr:    `[\d-z]`,
			wantErr: "error parsing regexp: cannot create range with shorthand escape sequence \\d in `[\\d-z]`",
		},
		"invalid-point": {
			expr:    `[\p-z]`,
			wantErr: "error parsing regexp: cannot create range with shorthand escape sequence \\p in `[\\p-z]`",
		},
	}

	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			re, err := regexp2.Compile(tt.expr, tt.opt|regexp2.ECMAScript)
			if tt.wantErr != "" {
				require.EqualError(t, err, tt.wantErr)
				return
			}
			require.NoError(t, err)

			match, err := re.FindStringMatch(tt.data)
			require.NoError(t, err)

			var res []string
			for match != nil {
				for _, g := range match.Groups() {
					for _, c := range g.Captures {
						res = append(res, c.String())
					}
				}

				match, err = re.FindNextMatch(match)
				require.NoError(t, err)
			}
			require.Equal(t, tt.want, res)
		})
	}
}
