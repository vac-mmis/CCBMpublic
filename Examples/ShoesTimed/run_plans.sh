#!/bin/sh

if [[ $# -eq 0 ]]; then echo "Usage: $0 <outdir>"; exit 1; fi
out="$1"

P=p

if [[ ! -x "$out/${P}-simulator" || ! -x "$out/${P}-validator" ]]; then
    echo "binaries not found in $out, please build them first"
    exit 1
fi

for S in `seq 1 9`; do
    # create a set of plans for different random seeds
    "$out/${P}-simulator" -R $S > "$out/$P-seed$S.plan"
    # validate the plans and compute the underlying state sequences
    "$out/${P}-validator" -a "$out/$P-seed$S.states" -v1 "$out/$P-seed$S.plan"
done
