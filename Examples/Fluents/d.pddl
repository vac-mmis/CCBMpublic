(define (domain fluent)

(:types num - object)

(:constants
    a b c
)

(:functions
  (f) - num
  (f' ?x - num) - num
  (g ?x - num) - object
)

(:action setf
    :parameters (?x - num)
    :effect (and
        (assign (f) ?x)
        (assign (f' ?x) (f))
        (when (= (f' ?x) 2)
            (and 
                (assign (g 0) b)
                (when (= (f) ?x)
                    (assign (g 1) b)
                )
            )
        )
    )
)

(:action bla
    :effect (bla a)
)

)
