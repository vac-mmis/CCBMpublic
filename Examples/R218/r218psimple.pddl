(define (problem r218psimple)
    (:domain r218dsimple)
  (:init
   (is-visible-from scr2 seatA)
   (is-visible-from scr3 seatA)
   (is-visible-from scr1 seatB)
   (is-visible-from scr2 seatB)
   (is-visible-from scr3 seatB)
   (is-visible-from scr1 seatC)
   (is-visible-from scr2 seatC)
   (is-projector-for prj1 scr1)
   (is-projector-for prj2 scr2)
   (is-projector-for prj3 scr3)
   (has nb1 doc1)
   (has nb2 doc2)
   (has nb3 doc3)
   (is-at alice seatB)
   (is-at bob seatA)
   (is-at charlie seatC)
   )
  (:goal (and (can-see alice doc1)
	      (can-see bob doc1)
	      (can-see alice doc2)
	      (can-see alice doc3)
	      (can-see charlie doc3)
	      )
	 )
  )

	 