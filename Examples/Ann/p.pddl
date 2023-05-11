(define (problem carrots2)
	(:domain carrots)
;	(:objects cupboard carrot glass plate bottle knife spoon wooden_spoon stove hands)
	(:init
;initial state

		(isat sink)
		
; distribution of objects
	(handsfree)
		(forall (?t - tool)
			(at ?t cupboard)
		)
		
		(forall (?c - container)
			(and
				(at ?c cupboard)
				(reachable counter ?c)
			)
		)
		
; room layout specific
		(reachable counter counter)
		(reachable counter stove)
		(reachable counter cupboard)
		(reachable table table)
		(reachable sink sink)


  	
	)
	(:goal
		(forall (?t - takeable)
			(at ?t sink))
	)
)