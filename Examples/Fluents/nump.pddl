(define (problem numfluent)
(:domain numfluent)

(:init
    (= (myint) 3)
    (= (f 1) 2)
    (= (myfloat) 0.0)
)

(:goal (and
  (= (myint) 1)
  (= (f 1) (f 2))
  (bla a)
  (and (> (myfloat) 0.39) (< (myfloat) 0.41) )
))

)
