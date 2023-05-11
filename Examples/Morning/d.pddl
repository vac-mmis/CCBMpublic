(define (domain morning)
  (:types
    user - object
    location - object
  )

  (:predicates
    (location ?u - user ?l - location)
    (is_dirty ?u - user)
  )
  
  (:action visit_bath
    :parameters (?u - user)
    :precondition (and (location ?u hall) (is_dirty ?u))
    :effect (and (not (is_dirty ?u)) (location ?u bathroom) (not (location ?u hall)))
    :observation ( setLocation bathroom )
  )
  
)
