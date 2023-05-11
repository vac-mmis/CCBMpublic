(define (domain object-fluent)

(:types t1 - t2)

(:constants
    a c - t1
    b - t2
)

(:functions
  (f1) - t1
  (f2) - t2
)

(:action setf1
    :parameters (?x - t1)
    :effect (assign (f1) ?x)
)

(:action setf2
    :parameters (?x - t2)
    :effect (assign (f2) ?x)
)

(:action test
    :precondition (and
        (= (f1) c)
        (= (f2) c)
    )
    :effect (equal)
)

(:action test2
    :precondition (and
        (= (f1) c)
        (= (f1) (f2))
    )
    :effect (equal)
)


)
