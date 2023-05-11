#!/bin/bash

NCORES=5 # number cores
NRUNS=10 # number runs;   total runs = NCORES*NRUNS
totalruns=$((NCORES*NRUNS))
#informed=informed
informed=blind

# compile state set
if [ ! -f abc-3.states ]; then
    rc -d Models/abc-d.pddl -p Models/abc-p1.pddl -o Models/abc-d-observations.h 
    ./abc-p1-analyzer -o abc-3.states 2>/dev/null
fi

# Build filters
for np in 100 1000 10000 100000 200000 300000; do
    f=abc3-${np}-filter
    if [ ! -x  $f ]; then
	echo Building $f
	rc -n $np -d Models/abc-d.pddl -p Models/abc-p1.pddl -o Models/abc-d-observations.h
	mv abc-p1-filter $f
    fi
done

rm -Rf abc3-output-$informed
mkdir abc3-output-$informed

for((runid=0;runid<NCORES;runid++)); do
    echo MASTER: Starting slave $runid
    ./abc3slave.sh $runid $NRUNS $informed 100 1000 10000 &
done

echo MASTER: Waiting for simple runs to finish

wait

echo MASTER: Simple runs done. Starting hard runs.

#
# Unfortunately, my machine has not enough memory (4GB)
# to do the x00k particle runs successfully on multiple
# cores. (A multi core run will cause memory thrashing)
# So we do this one on a single slave.
#

./abc3slave.sh 0 $totalruns $informed 100000

./abc3slave.sh 0 $totalruns $informed 200000

./abc3slave.sh 0 $totalruns $informed 300000

echo MASTER: Hard run done.
