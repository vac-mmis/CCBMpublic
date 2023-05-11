(define (domain smartlab)
    (:types device data limiter position - *object*
            document format number  - data
	    notebook projector surface lamp - device
    )

(:predicates
 (isLocked ?d - device)
 (isLockedIP ?n - number)
 (isLockedOP ?n - number)
 (isActive ?d - document ?device - device)
 (isAvailable ?d - data ?device - device)
 (isDown ?s - surface)
 (isMovable ?p - projector)
 (isPointingTo ?s - surface ?p - projector)
 (isLightOn ?l - lamp)
 (hasFormat ?f - format ?d - document)
 (supportsFormat ?f - format ?dev - device)
 (canConvert ?d - device ?ffrom ?fto - format)
 (connected ?k ?l - number)
 (ip ?n - notebook ?r - number)
 (op ?p - projector ?r - number)
 (opposite ?a ?b - position)
 (at-position ?s - surface ?p - position)
)

(:action move-canvas
	 :parameters (?s - surface ?a - position ?xxx - limiter ?b - position)
	 :precondition (and (not (isLocked ?s))
			    (not (at-position ?s ?a))
			    (opposite ?a ?b))
	 :effect (and (at-position ?s ?a)
		      (not (at-position ?s ?b))))

(:action light-on
	 :parameters (?l - lamp)
	 :precondition (not (isLightOn ?l))
	 :effect (isLightOn ?l))
(:action light-off
	 :parameters (?l - lamp)
	 :precondition (isLightOn ?l)
	 :effect (not (isLightOn ?l)))
(:action DUMMY-showDocument
	 :parameters (?d - document ?f - format ?n - notebook)
	 :precondition (and (isAvailable ?d ?n)
			    (hasFormat ?f ?d)
			    (supportsFormat ?f ?n)
			    (not (isLocked ?n)))
	 :effect (and (isActive ?d ?n)
		      (isLocked ?n)))

(:action bindInputToOutput
	 :parameters ( ?k ?l - number ?xxx - limiter ?n - notebook ?p - projector ?d - document)
	 :precondition (and (not (isLocked ?p))
			    (ip ?n ?k)
			    (op ?p ?l)
			    (isActive ?d ?n))
	 :effect (and (isLocked ?p)
		      (not (isActive ?d ?n))
		      (isActive ?d ?p)))

(:action switch-on
	 :parameters (?p - projector
		      ?xxx - limiter
		      ?s - surface
		      ?d - document)
	 :precondition (and (not (isLocked ?s))
			    (isActive ?d ?p)
			    (at-position ?s down)
			    (isPointingTo ?s ?p))
	 :effect (and (isLocked ?s)
		      (not (isActive ?d ?p))
		      (isActive ?d ?s)))
(:action move-projector
	 :parameters (?p          - projector
		      ?sfrom ?sto - surface)
	 :precondition (and (not (isLocked ?p))
			    (isMovable ?p)
			    (not (isLocked ?sfrom))
			    (not (isLocked ?sto))
			    (isPointingTo ?sfrom ?p))
	 :effect (and (not (isPointingTo ?sfrom ?p))
		      (isPointingTo ?sto ?p)))
(:action move-document
	 :parameters (?d         - document
		      ?f         - format
		      ?src ?dest - notebook)
	 :precondition (and (not (= ?src ?dest))
	 	       	    (not (isLocked ?src))
			    (hasFormat ?f ?d)
			    (supportsFormat ?f ?dest)
			    (isAvailable ?d ?src))
	 :effect (and (not (isAvailable ?d ?src))
		      (isAvailable ?d ?dest)))
(:action convert-document
	 :parameters (?d          - document
		      ?ffrom ?fto - format
		      ?dev        - device)
	 :precondition (and (not (= ?ffrom ?fto))
	 	       	    (canConvert ?dev ?ffrom ?fto)
			    (not (isLocked ?dev))
			    (isAvailable ?d ?dev)
			    (hasFormat ?ffrom ?d))
	 :effect (hasFormat ?fto ?d))
)
