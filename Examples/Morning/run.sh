#!/bin/sh

P=p

make $P-validator $P-simulator
for S in `seq 1 9`; do
  # create a set of plans for different random seeds
  make P=$P tmp/$P-seed$S.plan
  # validate the plans and compute the underlying state sequences
  make P=$P tmp/$P-seed$S.states
done
