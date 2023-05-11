(define (problem morning)
  (:domain morning)

  (:objects
    max - user
    hall bathroom - location
  )
  

  (:init
    (location max hall)
    (is_dirty max)
  )
  
  (:goal (and
    (not (is_dirty max))
    )
  )
)