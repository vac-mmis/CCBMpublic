(define (problem carrots2)
	(:domain carrots)
	(:init
		(is-at sink)
		(handsfree)
	
		(forall (?t - takeable)
			(at ?t counter)
		)
  	
	)
	(:goal
		(forall (?t - takeable)
			(at ?t sink))
	)
)