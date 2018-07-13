package nanocc

import (
	"bufio"
	"bytes"
	"io"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	"github.com/stretchr/testify/require"
)

func getOutput(f func()) []byte {
	buf := new(bytes.Buffer)
	out = buf
	f()
	out = os.Stdout
	return buf.Bytes()
}

func outputEqual(t *testing.T, expected, actual io.Reader) {
	addr := regexp.MustCompile(`(^( {4}|\d+> )(IMM|JMP|BZ|BNZ|JSR) +)(\d{2,12}|4|8)`)
	expScan := bufio.NewScanner(expected)
	actScan := bufio.NewScanner(actual)
	line := 0
	for expScan.Scan() && actScan.Scan() {
		line++
		expLine := addr.ReplaceAllString(expScan.Text(), "$1<addr>")
		actLine := addr.ReplaceAllString(actScan.Text(), "$1<addr>")
		require.Equal(t, expLine, actLine, "line    : %d", line)
	}
	require.True(t, expScan.Scan() == actScan.Scan())
}

func Test_main(t *testing.T) {
	tests := []struct {
		file string
		args string
	}{
		{"hello.c(comp)", "-s -d testdata/hello.c"},
		{"hello.c(run)", "-d testdata/hello.c"},
		{"c4.c(comp)", "-s -d testdata/c4.c"},
		{"c4.c(run)", "-d testdata/c4.c"},
		{"c4.c_hello.c(run)", "-d testdata/c4.c testdata/hello.c"},
		{"c4.c_-d_hello.c(run)", "testdata/c4.c -d testdata/hello.c"},
		{"c4.c_c4.c_hello.c(run)", "testdata/c4.c testdata/c4.c testdata/hello.c"},
		{"arginc.c(comp)", "-s -d testdata/arginc.c"},
		{"arginc.c_1_2(run)", "-d testdata/arginc.c 1 2"},
		{"fib.c(comp)", "-s -d testdata/fib.c"},
		{"fib.c_5(run)", "-d testdata/fib.c 5"},
		{"inc.c(comp)", "-s -d testdata/inc.c"},
		{"inc.c_1_2(run)", "-d testdata/inc.c 1 2"},
		{"while.c(comp)", "-s -d testdata/while.c"},
		{"while.c(run)", "-d testdata/while.c"},
	}
	for _, tt := range tests {
		t.Run(tt.file, func(t *testing.T) {
			file, err := os.Open(filepath.Join("testdata", tt.file+".txt"))
			require.NoError(t, err)
			defer file.Close()
			src = false
			debug = false
			os.Args = strings.Split("nanocc "+tt.args, " ")
			var output []byte
			require.NotPanics(t, func() {
				output = getOutput(main)
			})
			outputEqual(t, file, bytes.NewReader(output))
		})
	}
}
