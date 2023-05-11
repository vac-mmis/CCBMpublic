(define (domain xyrouter)
   (:types position)

   (:action step
      :parameters (?x ?y - position)
      :precondition (and (connected ?x ?y) (at ?x))
      :effect (and (not (at ?x)) (at ?y))
   )
   
   ; (:callbacks
   ;    (forall (?l - position)
   ;       (when (at ?l)
   ;          (setposition ?l)
   ;       )
   ;    )
   ; )
)

