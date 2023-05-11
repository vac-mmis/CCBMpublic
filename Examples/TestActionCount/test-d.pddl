(define (domain test)

    (:types task)

    (:predicates (done ?t - task))

    (:constants a b - task)

    (:action do
        :parameters (?t - task)
        :precondition (not (done ?t))
        :effect (done ?t))

    (:action strange-b
        :precondition (and (done a) (not (done b)))
        :effect (done b)))
