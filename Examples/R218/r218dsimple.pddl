(define (domain r218dsimple)
    (:types projector screen notebook - device
	    person device - agent
	    port - resource
	    seat - small-place
	    big-place small-place - location)
  (:predicates
   (is-active ?d - device)
   (in-use ?d - device)
   (is-locked-by ?server - agent ?resource - resource ?client - agent)
   (is-visible-from ?s - screen ?l - location)
   (is-projector-for ?p - projector ?s - screen)
   (has ?n - notebook ?d - document)
   (is-sending ?n - notebook ?d - document ?p - resource)
   (is-connected ?sd - device ?sp - port ?dd - device ?dp - port)
   (is-projecting-by ?p - projector ?d - document ?n - notebook)
   (is-projecting ?p - projector ?d - document)
   (is-showing-by ?s - screen ?d - document ?p - projector)
   (is-showing ?s - screen ?d - document)
   (is-at ?p - person ?l - location)
   (can-see ?p - person ?d - document)
   (has-seen ?p - person ?d - document)
   )
  (:constants alice bob charlie - person
	      nb1 nb2 nb3 - notebook
	      prj1 prj2 prj3 - projector
	      scr1 scr2 scr3 - screen
	      crossbar - device
	      doc1 doc2 doc3 - document
	      vga-in-port vga-out-port - port
	      vga-out-signal picture attention - resource
	      seatA seatB seatC - seat
	      door - big-place)

  (:action activate
	   :parameters (?d - device)
	   :precondition (not (is-active ?d))
	   :effect (is-active ?d))
  (:action deactivate
	   :parameters (?d - device)
	   :precondition (and (is-active ?d)
			      (not (in-use ?d))
			      (not (exists (?client - agent ?resource - resource)
					   (is-locked-by ?d ?resource ?client))))
	   :effect (not (is-active ?d)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notebook

  (:action begin-send-document
	  :parameters (?n - notebook ?d - document)
	  :precondition (and (is-active ?n)
			     (not (exists (?d1 - document) (is-sending ?n ?d vga-out-signal)))
			     (not (exists (?client - agent) (is-locked-by ?n vga-out-signal ?client)))
			     (has ?n ?d))
	  :effect (and (in-use ?n)
		       (is-sending ?n ?d vga-out-signal)))
  (:action end-send-document
	   :parameters (?n - notebook ?d - document)
	   :precondition (and (is-sending ?n ?d vga-out-signal)
			      (not (exists (?client - agent) (is-locked-by ?n vga-out-signal ?client))))
	   :effect (and (not (in-use ?n))
                        (not (is-sending ?n ?d vga-out-signal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Crossbar

  (:action begin-connect
	   :parameters (?n - notebook ?p - projector)
	   :precondition (and (not (is-connected ?n vga-out-port ?p vga-in-port))
			      (not (exists (?client - agent) (is-locked-by ?p vga-in-port ?client))))
	   :effect (and (is-connected ?n vga-out-port ?p vga-in-port)
			(is-locked-by ?n vga-out-port crossbar)
			(is-locked-by ?p vga-in-port crossbar)))
  (:action end-connect
	   :parameters (?n - notebook ?p - projector)
	   :precondition (and (is-connected ?n vga-out-port ?p vga-in-port)
			      (forall (?client - agent) (imply (is-locked-by ?p vga-in-port ?client)
							       (= ?client crossbar))))
	   :effect (and (not (is-connected ?n vga-out-port ?p vga-in-port))
			(not (is-locked-by ?n vga-out-port crossbar))
			(not (is-locked-by ?p vga-in-port crossbar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projector

  (:action begin-project-document
	   :parameters (?p - projector ?n - notebook ?d - document)
	   :precondition (and (is-active ?p)
			      (not (is-projecting ?p ?d))
			      (not (exists (?client - agent) (is-locked-by ?p picture ?client)))
			      (is-sending ?n ?d vga-out-signal)
			      (is-connected ?n vga-out-port ?p vga-in-port))
	   :effect (and (in-use ?p)
                        (is-locked-by ?n vga-out-signal ?p)
			(is-locked-by ?p vga-in-port ?p)
		        (is-projecting-by ?p ?d ?n)
			(is-projecting ?p ?d)))

  (:action end-project-document
	   :parameters (?p - projector ?n - notebook ?d - document)
	   :precondition (and (is-projecting-by ?p ?d ?n)
			      (not (exists (?client - agent) (is-locked-by ?p picture ?client))))
	   :effect (and (not (in-use ?p))
                        (not (is-projecting-by ?p ?d ?n))
			(not (is-projecting ?p ?d))
			(not (is-locked-by ?n vga-out-signal ?p))
			(not (is-locked-by ?p vga-in-port ?p))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Screen

  (:action begin-show-document
	   :parameters (?s - screen ?p - projector ?d - document)
	   :precondition (and (is-active ?s)
			      (not (is-showing ?s ?d))
			      (is-projecting ?p ?d)
			      (is-projector-for ?p ?s)
			      (not (exists (?client - agent) (is-locked-by ?s picture ?client))))
	   :effect (and (in-use ?s)
		        (is-locked-by ?p picture ?s)
			(is-showing-by ?s ?d ?p)
			(is-showing ?s ?d)))

  (:action end-show-document
	   :parameters (?s - screen ?p - projector ?d - document)
	   :precondition (and (is-showing-by ?s ?d ?p)
			      (not (exists (?client - agent) (is-locked-by ?s picture ?client))))
	   :effect (and (not (in-use ?s))
		        (not (is-locked-by ?p picture ?s))
			(not (is-showing-by ?s ?d ?p))
			(not (is-showing ?s ?d))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User

  (:action begin-looking
	   :parameters (?p - person ?s - screen ?d - document ?l - location)
	   :precondition (and (is-showing ?s ?d)
			      (is-at ?p ?l)
			      (is-visible-from ?s ?l))
	   :effect (and (is-locked-by ?s picture ?p)
			(is-locked-by ?p attention ?p)
			(can-see ?p ?d)))

  )


