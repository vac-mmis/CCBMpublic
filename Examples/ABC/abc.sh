#!/bin/bash

if [[ $# -eq 0 ]]; then echo "Usage: $0 <outdir> <rc options...>"; exit 1; fi
out="$1"
shift

mkdir -p "$out/build"
cp Models/* "$out/build"

../../rc -M FMHAVSOT -n 100 -d Models/abc-d.pddl -p Models/abc-p1.pddl -o Models/abc-d-observations -w "$out/build" -f "$out/abc-p1" "$@"
if [[ $? -ne 0 ]]; then exit 1; fi

echo "Running filters ..."

"$out"/abc-p1-analyzer -o "$out"/abc-p1.states
"$out"/abc-p1-filter -l "$out"/abc-p1.states -p "$out"/abc-p1.stats  <Data/abc.data >"$out"/abc-p1-filter.dat 2>"$out"/abc-p1-filter.err
"$out"/abc-p1-marginal -l "$out"/abc-p1.states -p "$out"/abc-p1-marginal.stats -S "$out"/abc-p1-marginal.smooth -V "$out"/abc-p1-marginal.viterbi <Data/abc.data >"$out"/abc-p1-marginal.dat 2>"$out"/abc-p1-marginal.err
"$out"/abc-p1-hmm -l "$out"/abc-p1.states <Data/abc.data >"$out"/abc-p1-hmm.dat 2>"$out"/abc-p1-hmm.err
