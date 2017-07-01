#!/bin/sh

# weaves
# A script to sample a file, by default 0.01
# writes to stdout.

test $# -ge 1 || exit 1
file0="$1"
shift
f0=0.01

if test $# -ge 1
then
  f0="$1"
  shift
fi

test -f "$file0" || exit 2

( head -1 "$file0" ; $nodo perl -n -e"print if rand() < ${f0}" "$file0" ) 
