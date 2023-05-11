(define (domain shoes)
  (:types
    foot - object
  )

  (:predicates
    (hasSock ?f - foot)
    (hasShoe ?f - foot)
  )
  
  (:action putOnSock
   :parameters (?f - foot)
   :duration (normal 5 2)
   :precondition (not (hasSock ?f))
   :effect (hasSock ?f)
   :observation ( setSide ?f )
  )

  (:action putOnShoe
   :parameters (?f - foot)
   :duration (normal 2 1)
   :precondition (and (hasSock ?f) (not (hasShoe ?f)) )
   :effect (hasShoe ?f)
   :observation ( setSide ?f )
  )

)
