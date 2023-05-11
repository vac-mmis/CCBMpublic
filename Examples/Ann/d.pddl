; take
; 


(define (domain carrots)
  (:types
	place - object
	takeable - object
	container - place
	fixedplace - place
	container - takeable ; we can move things inside containers
	tool - takeable
	location - object
	)

	(:predicates
		(reachable ?l - location ?p - place)
		(isat ?l - location) ; the person can be at a location
		(at ?t - takeable ?p - place) ; things can lie around
	)
  
   (:constants
		plate cutting_board  - container
   	sink counter - location
  		carrot knife wooden_spoon spoon sponge - tool
  		sink cupboard - fixedplace
   	)
	
	(:action move
		:parameters (?from ?to - location)
		:precondition (and
			(isat ?from)
			(not (= ?from ?to))
			)
		:effect (and
			(isat ?to)
			(not (isat ?from)))
	)
	
	(:action take
		:parameters (?t - tool ?from - place)
		:precondition (and
				(not (taken ?t))
				(at ?t ?from)
				(handsfree)
				(forall (?l - location)
					(iff 
						(isat ?l)
						(reachable ?l ?from)
					)
				)
			)
		:effect (and
			(taken ?t)
			(not (at ?t ?from))
			(not (handsfree))
			)
		)
		
		(:action put
			:parameters (?t - tool ?to - place)
			:precondition (and
				(taken ?t)
				(not (at ?t ?to))
				(forall (?l - location)
					(iff
						(isat ?l)
						(reachable ?l ?to)
					)
				)
			)
			:effect (and
				(not (taken ?t))
				(at ?t ?to)
				(handsfree)
			)
		)
		
		(:action put
			:parameters (?c - container ?to - fixedplace)
			:precondition (and
				(taken ?c)
				(not (at ?c ?to))
				(forall (?l - location)
					(iff
						(isat ?l)
						(reachable ?l ?to)
					)
				)
			)
			:effect (and
				(not (taken ?c))
				(at ?c ?to)
				(handsfree)
				(forall (?l - location)
					(when (isat ?l) (reachable ?l ?c))
				)
			)
		)
		
		(:action take
			:parameters (?c - container ?from - fixedplace)
			:precondition (and
					(not (taken ?c))
					(at ?c ?from)
					(handsfree)
					(forall (?l - location)
						(iff 
							(isat ?l)
							(reachable ?l ?c)
						)
					)
				)
			:effect (and
				(taken ?c)
				(not (handsfree))
				(not (at ?c ?from))
				(forall (?l - location)
					(not (reachable ?l ?c))
				)
			)
		)
)
