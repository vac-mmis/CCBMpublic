(define (domain non-repeating-test)
   (:types person location)
   (:constants l1 l2 revolving-door - location)
   (:predicates (at ?p - person ?l - location)
                (waited-at ?p - person ?l - location))
   
   ; the same person can't go to two places before doing a stop in between
   ; unless he/she is going to the revolving door. Because revolving doors are fun.
   ; (also, when he/she is coming from the revolving door - because you can hardly wait
   ;  when you're in a revolving door right?)
   (:action go-to 
    :irrational
    :parameters (?p - person ?l - location)
    :non-repeating (and (= ?p ??p) (not (= revolving-door ?l)) (not (= revolving-door ??l)))
    :precondition (or (= ?l revolving-door) (not (at ?p ?l)))
    :effect (at ?p ?l))

    ; the same person can't wait twice at the same location in a row
    (:action wait
      :irrational
      :parameters (?p - person ?l - location)
      :non-repeating
      ; you can't wait in revolving doors.
      :precondition (and (at ?p ?l) (not (= ?l revolving-door)))
      :effect (waited-at ?p ?l))
)


	   

