#!/bin/bash

runid=$1
nruns=$2
informed=$3
shift
shift
shift

NPS=$*

startrun=$((runid*nruns))
lastrun=$((startrun+nruns))

echo SLAVE $runid parameters: nruns=$nruns startrun=$startrun lastrun=$lastrun "NPS"=$NPS
#exit

runfilter() {
    local filter=$1
    if [ "$4" == informed ]; then
	info="-l abc-3.states"
    else
	info=""
    fi
    local k=$(($3*100))
    local cmdline="./${filter} -R${k} -p $2 $info <Data/abc.data 1>/dev/null 2>&1"
    echo SLAVE $runid: Running "$cmdline"
    until ./${filter} -R${k} -p $2 $info <Data/abc.data 1>/dev/null 2>&1; do
	k=$((k+1))
	echo SLAVE $runid: $filter Retry $k
    done
}

dorun() {
    local filter=abc3-${1}-filter
    local file
    local i
    for((i=$startrun;i<$lastrun;i++)); do
	file=$(printf "abc3-output-%s/%d-%02d.dat" $informed $np $i)
	runfilter $filter $file $i $informed
    done
}

for np in $NPS; do
    dorun $np
done
