#!/bin/bash

if [[ $# -eq 0 ]]; then echo "Usage: $0 <outdir> <rc options...>"; exit 1; fi
out="$1"
shift

mkdir -p "$out/build"

../../rc -M FHMAVSO -n 10000 -d non-repeating.pddl -p non-repeating-p.pddl -O -w "$out/build" -f "$out/p" -C "-DUSE_ACTION_ESTIMATION" "$@"
if [[ $? -ne 0 ]]; then exit 1; fi

echo "Running analyzer ..."

"$out"/p-analyzer -o "$out"/p.states 2>/dev/null
