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
${RC} -d ${MYHOME}/d.pddl -p ${MYHOME}/p.pddl -O -MTVS -f "$dir/" -w "$dir/build" "$@"
if [[ $? -ne 0 ]]; then exit 1; fi


for I in `seq 1 9`; do
    "$dir"/p-simulator -R${I} > "$dir"/plan${I}.txt
    "$dir"/p-validator -a "$dir"/states${I}.txt -v1 "$dir"/plan${I}.txt > /dev/null 2> /dev/null 
done 

"$dir"/p-ann-train -e 10000 -f "$dir"/nn.net "$dir"/states*.txt
"$dir"/p-ann-train -d -e 10000 -f "$dir"/nnd.net "$dir"/states*.txt

for I in `seq 1 9`; do
    "$dir"/p-ann-train -l "$dir"/nn.net -t "$dir"/states${I}.txt
    "$dir"/p-ann-train -d -l "$dir"/nnd.net -t "$dir"/states${I}.txt
done 
