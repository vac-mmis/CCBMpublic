; take
; 


(define (domain carrots)
  (:types
	location - object
	takeable - object
	)

  
   (:constants
   	sink counter - location
	carrot knife - takeable
   	)
	
	(:action move
		:parameters (?from ?to - location)
		:precondition (and
			(is-at ?from)
			(not (= ?from ?to))
			)
		:effect (and
			(is-at ?to)
			(not (is-at ?from)))
	)
	
	(:action take
	:parameters (?t - takeable ?from - location)
		:precondition (and
				(not (taken ?t))
				(at ?t ?from)
				(is-at ?from)
				(handsfree)
			)
		:effect (and
			(taken ?t)
			(not (at ?t ?from))
			(not (handsfree))
			)
		)
		
		(:action put
			:parameters (?t - takeable ?to - location)
			:precondition (and
				(taken ?t)
				(not (at ?t ?to))
				(is-at ?to)
			)
			:effect (and
				(not (taken ?t))
				(at ?t ?to)
				(handsfree)
			)
		)
	)
