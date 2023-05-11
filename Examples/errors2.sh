#!/bin/sh

cat <<EOF
Example model of forgetul person
EOF

rm -f tmp-p.pddl tmp-d.pddl tmp-p-analyzer tmp-p-filter tmp-observations.h

cat <<EOF >tmp-p.pddl
(define (problem errors)
	(:domain errors)
        (:init at-seat)
	(:goal presented))
EOF

cat <<EOF >tmp-d.pddl
(define (domain errors)
        (:types attempt)
        (:constants 1 2 3 4 5 - attempt)

        (:action take-outline
                :precondition (and (not believe-have-outline)
                                   at-seat)
                :effect (and have-outline
                             believe-have-outline))

        (:action dont-remember-to-take-outline
                :saliency 10
                :precondition (and (not believe-have-outline)
                                   at-seat)
                :effect (and (not have-outline)
                             believe-have-outline))

        (:action walk-to-stage
                :precondition believe-have-outline
                :effect (and (not at-seat)
                             at-stage))

        (:action discover-outline-missing
                :parameters (?t - attempt)
                :precondition (and at-stage
                                   (not have-outline)
                                   (not (retry ?t))
                                   believe-have-outline)
                :effect (and (not believe-have-outline)
                             (retry ?t)))

        (:action walk-to-seat
                :precondition at-stage
                :effect (and (not at-stage)
                             at-seat))

        (:action do-presentation
                :precondition (and have-outline
                                   at-stage)
                :effect presented)
)
EOF

cat <<EOF >tmp-observations.h
#define fetchObservation() 1.0
#define observe(x) 1.0
EOF
rc -t 100 -d tmp-d.pddl -p tmp-p.pddl -o tmp-observations.h
./tmp-p-filter -R5
./tmp-p-filter -R1

rm -f tmp-p.pddl tmp-d.pddl tmp-p-analyzer tmp-p-filter tmp-observations.h
