(define (problem fluent)
(:domain fluent)

(:objects 0 1 2 - num)

(:init
    (bla a)
    (= (f) 0)
    (= (f' 1) 0)
    (= (g 0) a)
)

(:goal (and
  (= (f) 1)
  (= (f) (f' 1))
  (= (g 0) b)
  (bla a)
))

)
