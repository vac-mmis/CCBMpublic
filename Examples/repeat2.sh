#!/bin/sh

cat <<EOF
***
*** Example problem and domain for repetitive actions
*** -- this version does a sequential discussion

Note that this basically is a pddl-model for loop:

Initialization: 'raise-first'
Step: 'raise-next'
Break: 'convinced'
Body: 'discharge'

EOF

rm -f tmp-p.pddl tmp-d.pddl tmp-p-analyzer tmp-p-filter tmp-observations.h

cat <<EOF >tmp-p.pddl
(define (problem repeat)
	(:domain repeat)
        (:init (succ 1 2)
               (succ 2 3)
               (succ 3 4)
               (succ 4 5)
               (succ 5 6)
               (succ 6 7)
               (succ 7 8)
               (succ 8 9))
	(:goal convinced))
EOF

cat <<EOF >tmp-d.pddl
(define (domain repeat)
	(:types issue)
	(:constants 1 2 3 4 5 6 7 8 9 - issue)
        (:action raise-first
                 :precondition (not (raised 1))
                 :effect (raised 1))
	(:action raise-next
		 :parameters (?last ?this - issue)
		 :precondition (and (succ ?last ?this)
                                    (discharged ?last)
                                    (not (raised ?this)))
		 :effect (raised ?this))
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
rc -t 100 -d tmp-d.pddl -p tmp-p.pddl -o tmp-observations.h
./tmp-p-filter

rm -f tmp-p.pddl tmp-d.pddl tmp-p-analyzer tmp-p-filter tmp-observations.h
