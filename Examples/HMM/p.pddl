(define (problem shoes)
  (:domain shoes)
  (:init  ; nothing holds in the initial state
    (= (id-of right) 0)
    (= (id-of left) 1)
  )
  
  (:objects
    left right - foot
  )
  

  (:goal (and
    (hasShoe left)
    (hasShoe right)
    )
  )
)