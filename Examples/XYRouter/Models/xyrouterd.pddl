(define (domain xyrouter)
	(:types position)
	(:action step
		 :parameters (?x ?y - position)
;		 :saliency (x-pos ?y)
		 :agent TheAgent
		 :duration (exponential 0.1)
		 :precondition (and (connected ?x ?y) (at ?x))
		 :effect (and (not (at ?x)) (at ?y)))
	(:callbacks (forall (?l - position)
			      (when (at ?l)
				(setXYcoordinates (x-pos ?l) (y-pos ?l))))))

