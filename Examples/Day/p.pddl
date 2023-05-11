(define (problem day)
  (:domain day)
  
  (:init
    
    (= (activity-id dress) 1)
    (= (activity-id coffee) 2)
    (= (activity-id breakfast) 3)
    (= (activity-id clean) 4)
    (= (activity-id leave) 5)
  )
    
  (:objects
    bob - user
  )
  
  (:goal (and
    (hasLeft bob)
    )
  )
)
