#!/bin/bash

if [[ $# -eq 0 ]]; then echo "Usage: $0 <outdir> <rc options...>"; exit 1; fi
out="$1"
shift

mkdir -p "$out/build"

../../rc -M FHMAVSO -n 10000 -d d.pddl -p p.pddl -o d-observations -w "$out/build" -f "$out/p" -C "-DUSE_ACTION_ESTIMATION" "$@"
if [[ $? -ne 0 ]]; then exit 1; fi

echo "Running filters ..."

"$out"/p-analyzer -o "$out"/p.states 2>/dev/null
"$out"/p-filter -l "$out"/p.states -p "$out"/p.stats  <data/obs_3.dat >"$out"/p-filter.dat 2>"$out"/p-filter.err
"$out"/p-marginal -l "$out"/p.states -p "$out"/p-marginal.stats -S "$out"/p-marginal.smooth -V "$out"/p-marginal.viterbi <data/obs_3.dat >"$out"/p-marginal.dat 2>"$out"/p-marginal.err
