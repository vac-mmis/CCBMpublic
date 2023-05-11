#!/bin/sh

if [[ $# -eq 0 ]]; then
    echo "Usage: $0 <output directory> [RC options ...]"
    exit 1
fi

dir="$1"
shift

# Find true location of script
scriptpath=$(readlink $0)
if [ -z $scriptpath ]; then scriptpath=$0; fi
MYHOME=$(cd $(dirname $scriptpath); pwd)

RC=${MYHOME}/../../rc

mkdir -p "$dir/build"
${RC} -d d.pddl -p p.pddl -w ${MYHOME} -W -MAH -w "$dir/build" -f "$dir/" "$@"

"$dir"/p-analyzer -o "$dir"/p.states
"$dir"/p-hmm -l "$dir/"p.states < llll.dat
