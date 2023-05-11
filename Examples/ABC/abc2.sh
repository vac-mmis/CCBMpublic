#!/bin/bash


N=50      # Number of runs
NP=100    # Number of particles per run

mktools() {
    if [ "$1" == u ]; then
	W=""
    else
	W="-W"
    fi
    rc -n $NP $W -d Models/abc-d.pddl -p Models/abc-p1.pddl -o Models/abc-obs2.h
    rc -n $NP $W -d Models/abc-d.pddl -p Models/abc-p2.pddl -o Models/abc-obs2.h
    mv abc-p1-analyzer abc-${1}1-analyzer
    mv abc-p1-filter abc-${1}1-filter
    mv abc-p2-analyzer abc-${1}2-analyzer
    mv abc-p2-filter abc-${1}2-filter
    ./abc-${1}1-analyzer -o abc-${1}1.states 2>/dev/null
    ./abc-${1}2-analyzer -o abc-${1}2.states 2>/dev/null
}

if [ "$1" == redo ]; then
    mktools u
    mktools w
fi


dorun() {
 
    if [ "$3" == informed ]; then
	infopt="-l abc-${1}${2}.states"
    else
	infopt=""
    fi

    filter=./abc-${1}${2}-filter
    datfile=${1}${2}-${3}.dat
    echo Computing $datfile

    ${filter} -R0${2}123 $infopt <Data/abc.data 2>/dev/null >$datfile
    for((i=1;i<${N};i++)); do
	echo $1 $i
	${filter} -R${i}123${i} $infopt <Data/abc.data 2>/dev/null | cut -d' ' -f2 | cat - filler.dat | head -n 27 | lam $datfile -S " " - >tmp.dat
	mv tmp.dat $datfile
    done
}

dorun2() {
    if [ "$3" == informed ]; then
	infopt="-l abc-${1}${2}.states"
    else
	infopt=""
    fi

    filter=./abc-${1}${2}-filter
    datfile=${1}${2}-${3}.dat
    echo Computing $datfile

    k=0
    until ${filter} -R01${k} $infopt <Data/abc.data 2>/dev/null >$datfile; do k=$((k+1)); echo Retry 0-$k; done
    for((i=1;i<${N};i++)); do
	echo $1 $i
	k=0
	until ${filter} -R${i}1${k} $infopt <Data/abc.data 2>/dev/null >tmp1.dat; do k=$((k+1)); echo Retry $i-$k; done
	cut -d' ' -f2 <tmp1.dat | lam $datfile -S " " - >tmp.dat
	mv tmp.dat $datfile
    done
}

dorun2 u 1 informed
dorun2 u 2 informed
dorun2 u 1 blind
dorun2 u 2 blind
dorun2 w 1 informed
dorun2 w 2 informed
dorun2 w 1 blind
dorun2 w 2 blind
