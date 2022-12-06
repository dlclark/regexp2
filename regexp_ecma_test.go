package regexp2_test

import (
	"testing"

	"github.com/dlclark/regexp2"
	"github.com/stretchr/testify/require"
)

func TestECMA_basic(t *testing.T) {
	tests := map[string]struct {
		expr string
		data string
		want []string
	}{
		"charset": {
			expr: `[a-c]`,
			data: "abcd",
			want: []string{"a", "b", "c"},
		},
		"charset-set": {
			expr: `[a-\s]`,
			data: "a-b cd",
			want: []string{"a", "-", " "},
		},
	}

	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			re, err := regexp2.Compile(tt.expr, regexp2.ECMAScript)
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
