(define (domain day)
  (:types
    user - object
  )
  
  (:predicates
    (isDressed ?u - user)
    (drankCoffee ?u - user)
    (hadBreakfast ?u - user)
    (isClean ?u - user)
    (hasLeft ?u - user)
  )
  
  (:action dress
    :parameters (?u - user)
    :precondition (not (isDressed ?u))
    :effect (isDressed ?u)
    :observation (setActivity (activity-id dress)) 
  )
  
  (:action coffee
    :parameters (?u - user)
    :precondition (not (drankCoffee ?u))
    :effect (drankCoffee ?u)
    :observation (setActivity (activity-id coffee)) 
  )
  
  (:action breakfast
    :parameters (?u - user)
    :precondition (not (hadBreakfast ?u))
    :effect (hadBreakfast ?u)
    :observation (setActivity (activity-id breakfast)) 
  )
  
  (:action clean
    :parameters (?u - user)
    :precondition (and (hadBreakfast ?u) (drankCoffee ?u) (not (isClean ?u)))
    :effect (isClean ?u)
    :observation (setActivity (activity-id clean)) 
  )
  
  (:action leave
    :parameters (?u - user)
    :precondition (and (isClean ?u) (isDressed ?u) (hadBreakfast ?u) (drankCoffee ?u) (not (hasLeft ?u)))
    :effect (hasLeft ?u)
    :observation (setActivity (activity-id leave)) 
  )
)
