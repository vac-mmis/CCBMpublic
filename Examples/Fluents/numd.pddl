(define (domain numfluent)

(:types foo - object)

(:constants
    baz bar - foo
    a b c
)

(:functions
  (myint) - number
  (myrestricted) - (number 2)
  (f ?x - (number 2)) - (number -2 6)
  (myfloat) - float
  (small) - (number 1 2)
  (big) - (number 5 10)
  (obj) - foo
)

(:action setf
    :parameters (?x - (number 2))
    ;:parameters (?x - (number -1 3))
    :effect (and
        (assign (myint) ?x)
        (assign (myrestricted) ?x)
        (when (> (myint) 1)
            (assign (f ?x) (myrestricted))
        )
        (assign (small) 1)
        (assign (big) 6)
        (assign (obj) bar)
    )
)

(:action incfloat
    :precondition (and
        (< (myfloat) (+ (myrestricted) 1))
        (< (big) 10)
    )
    :effect (and
        (increase (myfloat) 0.1)
        (increase (big) 0.1)
    )
)

; test resolving static comparisons
(:action testfloatcmp
    :parameters (?x - (number 2))
    :precondition (and
        (> (myint) ?x) ; not static, always run-time
        (> (big) ?x) ; always true
        (< ?x (small)) ; always true for ?x=0, undecided for ?x=1, always false for ?x = 2
    )
    :effect (bla a)
)

(:action bla
    :effect (bla a)
)

)
