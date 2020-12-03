#!/bin/sh

n=$1

usage() {
  echo "Usage: $0 number"
  exit 1
}

[[ -n $n ]] || usage

echo "Downloading day $n"

mkdir -p day$n
curl "https://adventofcode.com/2020/day/$n/input" -H "cookie: session=$session"  --compressed > day$n/input.txt
