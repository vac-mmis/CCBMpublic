#!/bin/sh


rm -f tmp-p.pddl tmp-d.pddl tmp-p-analyzer tmp-p-filter tmp-observations.h

withPauses() {
cat <<EOF >tmp-p.pddl
(define (problem pause)
	(:domain pause)
        (:init)
	(:goal (and (not recovering) ; don't finish while taking a break
                    ; recovered ; <-- interesting alternative
                    done))
)
EOF

cat <<EOF >tmp-d.pddl
(define (domain pause)

        ;;; Normal actions, slightly extended

        (:action start
                :precondition (and (not recovering) ; check not taking a break
                                   (not working))
                :effect (and (not recovered) ; enable pauses
                             working))

        (:action finish
                :precondition (and (not recovering) ; check not taking a break
                                   working)
                :effect (and (not recovered) ; enable pauses
                             done))

        ;;; Additional actions managing pauses
        
        (:action --begin-short-pause
                 :duration (exponential 0.1)
                 :precondition (and (not recovering) (not recovered))
                 :effect recovering)

        (:action --begin-long-pause
                 :duration (exponential 0.01)
                 :precondition (and (not recovering) (not recovered))
                 :effect recovering)

        (:action --end-pause
                 :precondition recovering
                 :effect (and (not recovering) recovered))
)
EOF
}

withoutPauses() {
cat <<EOF >tmp-p.pddl
(define (problem pause)
	(:domain pause)
        (:init)
	(:goal (and done))
)
EOF

cat <<EOF >tmp-d.pddl
(define (domain pause)

        (:action start
                :precondition (and (not working))
                :effect (and working))

        (:action finish
                :precondition (and working)
                :effect (and done))
)
EOF
}

if [ -z $1 ]; then
    echo "*** WITH pauses ***"
    withPauses
else
    echo "--- without pauses ---"
    withoutPauses
fi

cat <<EOF >tmp-observations.h
#define fetchObservation() 1.0
#define observe(x) 1.0
EOF
rc -t 100 -d tmp-d.pddl -p tmp-p.pddl -o tmp-observations.h
./tmp-p-filter

#rm -f tmp-p.pddl tmp-d.pddl tmp-p-analyzer tmp-p-filter tmp-observations.h
