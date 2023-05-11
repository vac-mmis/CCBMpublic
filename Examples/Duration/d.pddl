;This example shows how to use durations as parameters for actions

(define (domain carrots)
   (:types
      tool - object
      location - object
      time - object
   )

   (:predicates
      (isat ?l - location) ; the person can be at a location
      (at ?t - takeable ?p - place) ; things can lie around
   )
  
   (:constants
      counter - location
      knife - tool
      ; fork - tool
      ; spoon - tool
      ; plate - tool
      
      2 5 8 10 15 - time
   )
	
   (:action take
      :parameters (?t - tool ?from - location ?durmean - time ?dursd - time)
      :duration (normal (val ?durmean) (val ?dursd))
      :precondition (and
         (at ?t ?from)
         (isat ?from)
         )
      :effect (and
         (not (at ?t ?from))
      )
   )
)
