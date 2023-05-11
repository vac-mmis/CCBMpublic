#!/bin/sh

cat <<EOF
***
*** Example problem and domain for repetitive actions
***
EOF

rm -f tmp-p.pddl tmp-d.pddl tmp-p-analyzer tmp-p-filter

cat <<EOF >tmp-p.pddl
(define (problem repeat)
	(:domain repeat)
	(:goal convinced))
EOF

cat <<EOF >tmp-d.pddl
(define (domain repeat)
	(:types issue)
	(:constants 1 2 3 4 5 6 7 8 9 - issue)
	(:action raise
		 :parameters (?i - issue)
		 :precondition (not (raised ?i))
		 :effect (raised ?i))
	(:action discharge
		 :parameters (?i - issue)
		 :precondition (raised ?i)
		 :effect (discharged ?i))
	(:action convinced
		 :precondition (forall (?i - issue)
		 	       	       (imply (raised ?i) (discharged ?i)))
	         :effect convinced)
)
EOF

cat <<EOF >tmp-observations.h
#define fetchObservation() 1.0
#define observe(x) 1.0
EOF

rc -t 10 -d tmp-d.pddl -p tmp-p.pddl -o tmp-observations.h
./tmp-p-filter

rm -f tmp-p.pddl tmp-d.pddl tmp-p-analyzer tmp-p-filter tmp-observations.h

