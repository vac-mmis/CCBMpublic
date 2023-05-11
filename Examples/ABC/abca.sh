#!/bin/sh

if [ "$1" = "-?" ]; then
    cat <<EOF 
$0: Alice, Bob, and Charlie based on anonymous modeling.

	Rather than using 'names' of agents for agent identification,
	we use property sets. This way, the state explosion created by
	different tpermutations of 'names' for agents doing the same
	thing can be avoided.

	(Open issue: How do we connect this to the multi-agent execution
	strategy used by the particle filter ... ?)
EOF
    exit
fi

## This will generate 199500 states; abc model of same size produces 1957000 states
maxagent=3 # maximum number of agents possible
nastart=3  # number of agents starting
nagoal=3   # number of agents achieving agent goal state
nprint=3   # number of printjobs (not greater than maxagent)

## This will generate 18480 states; abc model of same size produces 64386 states
#nastart=2  # number of agents starting
#nagoal=2   # number of agents achieving agent goal state

rm -f abca-p-analyzer abca-p-filter abca.states abca-p.pddl abca-d.pddl

if [ "$1" == "clean" ]; then
    echo "*** just cleaned ***"
    exit
fi

GLocations="door printer paper-stack water-tap coffee-jar coffee-machine"
Locations="outside ${GLocations}"
Items="nothing paper water ground-coffee coffee"
Agid=0

props=""
agts=""

showprop() {
    props="${props}$4(has-prop $1 $2 $3)"
}


for loc in $Locations; do
    for item in $Items; do
	agname=ag${Agid}; Agid=$((Agid+1))
	n=0
	if [ $loc == "outside" ]; then
	    if [ $item == "nothing" ]; then
		n=$nastart
	    elif [ $item == "coffee" ]; then
		aggoal=$agname
	    fi
	fi
	props="${props}\n\t(arity $agname $n)"
	showprop $agname "holds" $item " "
	showprop $agname "loc"   $loc  " "
	agts="${agts} $agname"
    done
done

succs="(= (succ 0) 1)"
preds="(= (pred 1) 0)"
for((i=1;i<maxagent;i++)); do
    inner="$inner $i"
    succs="$succs (= (succ $i) $((i+1)))"
    preds="$preds (= (pred $((i+1))) $i)"
done
pjobs=""
for((i=1;i<=nprint;i++)); do
    pjobs="$pjobs $i"
done

nonzero="$inner $maxagent"
nonmax="0$inner"
arity="0$inner $maxagent"
echo Arity = "$arity"
echo Succs = "$succs"
echo Preds = "$preds"
echo Print = "$pjobs"

cat >abca-p.pddl <<EOF
(define (problem abca1)
   (:domain abca)
   (:objects${agts} - agent
            ${arity} - arity
            ${pjobs} - jobnum
            ${nonzero}    - nonzero
            ${nonmax}     - nonmax
            ${Locations}  - location
            ${GLocations} - glocation
            ${Items}      - item
            )
   (:init ${succs}
          ${preds}
          (njobs ${nprint})
	 (provides water water-tap)
	 (provides paper paper-stack)
	 (provides ground-coffee coffee-jar)
	 (resource-of paper printer)
	 (resource-of water coffee-machine)
	 (resource-of ground-coffee coffee-machine)
          printer-jammed
          $(echo ${props}))
    (:goal (and (njobs 0)
                (arity ${aggoal} ${nagoal})))
)
EOF

cat >abca-d.pddl <<EOF
(define (domain abca)
    (:action enter
	     :parameters (?nfrom - nonzero
			  ?nto   - nonmax
			  ?afrom ?ato - agent)
	     :precondition (and (arity ?afrom ?nfrom)
				(arity ?ato ?nto)
				(has-prop ?afrom loc outside)
				(has-prop ?ato loc door)
				(exists (?i - item)
					(and
					 (has-prop ?afrom holds ?i)
					 (has-prop ?ato holds ?i))))
	     :effect (and (not (arity ?afrom ?nfrom))
			  (not (arity ?ato ?nto))
			  (arity ?afrom (pred ?nfrom))
			  (arity ?ato (succ ?nto))))

    (:action exit
	     :parameters (?nfrom - nonzero
			  ?nto   - nonmax
			  ?afrom ?ato - agent)
	     :precondition (and (arity ?afrom ?nfrom)
				(arity ?ato ?nto)
				(has-prop ?afrom loc door)
				(has-prop ?ato loc outside)
				(exists (?i - item)
					(and
					 (has-prop ?afrom holds ?i)
					 (has-prop ?ato holds ?i))))
	     :effect (and (not (arity ?afrom ?nfrom))
			  (not (arity ?ato ?nto))
			  (arity ?afrom (pred ?nfrom))
			  (arity ?ato (succ ?nto))))

    (:action goto
	     :parameters (?nfrom - nonzero
			  ?nto   - nonmax
			  ?afrom ?ato - agent
			  ?lfrom ?lto - glocation)
	     :precondition (and (not (= ?lfrom ?lto))
                                (arity ?afrom ?nfrom)
				(arity ?ato ?nto)
				(has-prop ?afrom loc ?lfrom)
				(has-prop ?ato loc ?lto)
				(exists (?i - item)
					(and
					 (has-prop ?afrom holds ?i)
					 (has-prop ?ato holds ?i))))
	     :effect (and (not (arity ?afrom ?nfrom))
			  (not (arity ?ato ?nto))
			  (arity ?afrom (pred ?nfrom))
			  (arity ?ato (succ ?nto))))

  (:action print
	   :parameters (?n - jobnum)
	   :precondition (and (not printer-jammed)
			      (has paper printer)
			      (njobs ?n))
	   :effect (and (not (njobs ?n))
			(njobs (pred ?n))))

  (:action *****JAM-PRINTER******
	   :precondition (not printer-jammed)
	   :effect (printer-jammed))

  (:action repair-printer
	   :parameters (?a - agent)
	   :precondition (and printer-jammed
			      (has-prop ?a loc printer)
			      (has-prop ?a holds nothing)
			      (exists (?n - nonzero) (arity ?a ?n)))
	   :effect (not (printer-jammed)))

  (:action get-coffee
	   :parameters (?a ?b - agent
			?na - nonzero
			?nb - nonmax)
	   :precondition (and (has-prop ?a holds nothing)
			      (has-prop ?b holds coffee)
			      (has-prop ?a loc coffee-machine)
			      (has-prop ?b loc coffee-machine)
			      (arity ?a ?na)
			      (arity ?b ?nb)
			      (has water coffee-machine)
			      (has ground-coffee coffee-machine))
	   :effect  (and (not (arity ?a ?na))
			 (not (arity ?b ?nb))
			 (arity ?a (pred ?na))
			 (arity ?b (succ ?nb))))

  (:action fetch
	   :parameters (?a ?b - agent
			?na - nonzero
			?nb - nonmax
			?l - location
			?i - item)
	   :precondition (and (has-prop ?a holds nothing)
			      (has-prop ?b holds ?i)
			      (has-prop ?a loc ?l)
			      (has-prop ?b loc ?l)
			      (arity ?a ?na)
			      (arity ?b ?nb)
			      (provides ?i ?l))
	   :effect  (and (not (arity ?a ?na))
			 (not (arity ?b ?nb))
			 (arity ?a (pred ?na))
			 (arity ?b (succ ?nb))))

  (:action replenish
	   :parameters (?a ?b - agent
			?na - nonzero
			?nb - nonmax
			?l - location
			?i - item)
	   :precondition  (and (has-prop ?a holds ?i)
			       (has-prop ?b holds nothing)
			       (has-prop ?a loc ?l)
			       (has-prop ?b loc ?l)
			       (arity ?a ?na)
			       (arity ?b ?nb)
			       (resource-of ?i ?l)
			       (not (has ?i ?l)))
	   :effect  (and (not (arity ?a ?na))
			 (not (arity ?b ?nb))
			 (arity ?a (pred ?na))
			 (arity ?b (succ ?nb))
			 (has ?i ?l)))


)
EOF

rc -s -p abca-p.pddl -d abca-d.pddl  -O
./abca-p-analyzer -o abca.states
./abca-p-filter -l abca.states
