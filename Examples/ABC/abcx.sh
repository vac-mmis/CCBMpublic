#!/bin/bash
#
# The same as abc.sh, just using exact mode

rc -X -d Models/abc-d.pddl -p Models/abc-p1.pddl -o Models/abc-d-observations.h
rc -X -d Models/abc-d.pddl -p Models/abc-p2.pddl -o Models/abc-d-observations.h

./abc-p1-analyzer -o abc-p1.states 2>/dev/null
./abc-p1-filter -l abc-p1.states <Data/abc.data >abc-p1-exact.dat

./abc-p2-analyzer -o abc-p2.states 2>/dev/null
./abc-p2-filter -l abc-p2.states <Data/abc.data >abc-p2-exact.dat

rm abc-p1-analyzer abc-p1-filter abc-p1.states
rm abc-p2-analyzer abc-p2-filter abc-p2.states

exit
## Expected Results:

cat <<EOF
./abc-p1-filter: Read 64386 state records from abc-p1.states
./abc-p1-filter: Created 16317 HMM States
./abc-p1-filter: Final log-likelihood: -29.4859
./abc-p1-filter: Finishing time:       26
./abc-p2-filter: Read 59976 state records from abc-p2.states
./abc-p2-filter: Created 32095 HMM States
./abc-p2-filter: Final log-likelihood: -30.8868
./abc-p2-filter: Finishing time:       26
EOF
