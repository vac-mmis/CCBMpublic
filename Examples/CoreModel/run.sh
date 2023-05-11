#!/bin/sh

if [[ $# -eq 0 ]]; then
    echo "Usage: $0 <output directory> [RC options ...]"
    exit 1
fi

dir="$1"
shift

#the script executes the following steps
# 1. runs analyzer to get full state space
# 2. simulates n plans (see SEEDS)
# 3. extract state sequence with validator
# 4. runs analyzer with restriction to extracted state space
# 5. runs mariginal filter with full  and restricted state space
# 6. runs particle filter with full and restricted state space
# As result the likelihood using the restricted state space should be much higher

# Be carefull by increasing the number of seeds, this also increases the number of visited states and results in a restricted state space equal to the full

# Find true location of script
scriptpath=$(readlink $0)
if [ -z $scriptpath ]; then scriptpath=$0; fi
MYHOME=$(cd $(dirname $scriptpath); pwd)

#SEEDS="565 693 252  28 177 411  96 735 485 558"
SEEDS="565"

RC=${MYHOME}/../../rc


# compile model
mkdir -p "$dir/build"
${RC} -d xyrouterd.pddl -p xyrouterp.pddl -O -W -w "$dir/build" -f "$dir/" -MSAMVF -C"-DUSE_ACTION_OBSERVATION" -r1 "$@"
if [[ $? -ne 0 ]]; then exit 1; fi

pushd "$dir"

# get full state table
./xyrouterp-analyzer -o full.states 2> /dev/null

# simulate some plans
for SEED in ${SEEDS}; do
  ./xyrouterp-simulator -R ${SEED} -l full.states  > ${SEED}.plan
  ./xyrouterp-validator -v1 -a ${SEED}.dat ${SEED}.plan > /dev/null 2> /dev/null

  sed 1d ${SEED}.dat | awk '{$1="";$2="";print}' >> core.states.txt
done

./xyrouterp-analyzer -e core.states.txt -o part.states 2> /dev/null

for SEED in ${SEEDS}; do
  echo "Running Filter on ${SEED}.plan"
  ./xyrouterp-marginal -e core.states.txt -l part.states < ${SEED}.plan > /dev/null 2> tmp.txt
   grep "Final" tmp.txt
   rm tmp.txt
  ./xyrouterp-marginal -l full.states < ${SEED}.plan > /dev/null 2> tmp.txt
  grep "Final" tmp.txt
  rm tmp.txt

  # particle filter
  ./xyrouterp-filter -e core.states.txt -l part.states < ${SEED}.plan > /dev/null 2> tmp.txt
    grep "Final" tmp.txt
    rm tmp.txt
  ./xyrouterp-filter -l full.states < ${SEED}.plan > /dev/null  2> tmp.txt
   grep "Final" tmp.txt
   rm tmp.txt

done

popd
