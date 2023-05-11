(define (domain abc)
    (:types glocation - location agent job)
  (:constants door printer paper-stack coffee-machine water-tap coffee-jar - glocation
	      outside - location
	      water paper ground-coffee coffee - item)
  (:action print
	   :parameters (?j - job)
	   :precondition (and (not (printed ?j))
	   		      (not printer-jammed)
			      (has paper printer))
	   :effect (printed ?j))

  (:action *****JAM-PRINTER******
	   :precondition (not printer-jammed)
	   :effect (printer-jammed))

  (:action repair-printer
	   :parameters (?a - agent)
	   :precondition (and (at printer ?a)
			      (hands-free ?a)
			      (printer-jammed))
	   :effect (not printer-jammed))

  (:action get-coffee
	   :parameters (?a - agent)
	   :precondition (and (at coffee-machine ?a)
			      (has water coffee-machine)
			      (has ground-coffee coffee-machine)
			      (hands-free ?a))
	   :effect (and (holds coffee ?a)
			(not (hands-free ?a))))

  (:action fetch
	   :parameters (?a - agent ?i - item ?l - location)
	   :precondition (and (hands-free ?a)
			      (provides ?i ?l)
			      (at ?l ?a))
	   :effect (and (not (hands-free ?a))
			(holds ?i ?a)))

  (:action replenish
	   :parameters (?a - agent ?i - item ?l - location)
	   :precondition (and (holds ?i ?a)
			      (at ?l ?a)
			      (resource-of ?i ?l)
			      (not (has ?i ?l)))
	   :effect (and (has ?i ?l)
			(not (holds ?i ?a))
			(hands-free ?a)))
 
  (:action goto
	   :parameters (?a - agent ?x ?y - glocation)
	   :precondition (and (not (= ?x ?y))
			      (at ?x ?a))
	   :effect (and (not (at ?x ?a))
			(at ?y ?a)))

  (:action enter
	   :parameters (?a - agent)
	   :precondition (at outside ?a)
	   :effect (and (at door ?a)
			(not (at outside ?a))))

  (:action exit
	   :parameters (?a - agent)
	   :precondition (at door ?a)
	   :effect (and (at outside ?a)
			(not (at door ?a))))

  (:observation (forall (?l - location)
			(forall (?a - agent)
				(when (at ?l ?a) (setAgentLoc (agId ?a) (locId ?l))))))

  (:observation (forall (?l - location)
			(when (and (not (= ?l outside))
				   (exists (?a - agent) (at ?l ?a)))
			  (signalExpectedFromSensor (locId ?l)))))
)


	   

