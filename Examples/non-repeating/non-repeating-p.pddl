(define (problem from-l1-to-l2)
    (:domain non-repeating-test)
    (:objects p1 p2 - person)
    (:init (at p1 l1) (at p2 l2))
    (:goal (and
        (at p1 l2)
        (waited-at p1 l2)
        (at p2 l1)
        (waited-at p2 l1))))
