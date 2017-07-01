#!/bin/sh

# weaves
# A script to remove lines from a CSV file that have more than the number of fields in the header row.
# writes to stdout.

test $# -ge 1 || exit 1
file0="$1"
shift

test -f "$file0" || exit 2

cols=$(awk -F, 'NR == 1 { print NF; exit(0) }' "$file0")

$nodo awk -F, -v ncols=$cols 'NF == ncols { printf("%s\n", $0) }' $file0 
