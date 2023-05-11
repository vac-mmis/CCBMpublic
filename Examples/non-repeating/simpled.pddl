(define (domain non-repeating-simple)

(:types int myflag)

(:constants
    0 1 - int
    *partial* *final* - myflag
)

(:action a0
    :non-repeating
    :effect (a0)
)

(:action a1
    :parameters (?x - int)
    :non-repeating
    :effect (a1)
)

(:action a2
    :parameters (?x - int)
    :non-repeating true
    :effect (a2)
)

(:action a3
    :parameters (?x ?y - int)
    :non-repeating (= ?x ??x)
    :effect (a3)
)

; partial actions may not be followed after the same partial action
; final actions may not be followed after partial actions (there must be something in between)
(:action a4
    :parameters (?f - myflag ?x - int)
    :non-repeating
         (and
            (= ?x ??x) ; the same ground action
            (= ??f *partial*) ; has currently been executed as a *partial* action
         )
    :effect (a4)
)

)
