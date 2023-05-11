# CCBM Quickstart Guide
The purpose of the CCBM system is to describe the behavior of dynamic systems, that is systems whose state changes over time. To achieve this, CCBM follows a symbolic approach that uses precondition-effect rules to define the causal model of the domain it operates on. The user may specifiy this domain with its possible actions, preconditions and effects as well as the problem, via the planning domain definition language (PDDL). Also specified by the user is a sequence of observations which is read by the system to estimate possible action trajectories via Bayesian filtering. In essence, the system provides probability estimates for actions over time and allows to incorporate prior knowledge on the domain. Further insights into how the system works as well as prerequisites and the theoretical background are given in the [RC3 paper](rc.pdf).
 
This quickstart guide offers a step-by-step introduction on how to implement a new domain for CCBM. All corresponding code that was created for this instruction can be found under CCBM/Examples/Day. Important points when working with CCBM will be illustrated via this example.
The simple domain ‘day’ describes a setting where a user gets ready in the morning to leave the house. Five actions are defined for this domain. Namely, ‘breakfast’, ‘coffee’ and ‘dress’ are those actions which have no further preconditions and can be executed at any time step. The action ‘clean’ refers to the user cleaning their teeth, this action requires as precondition that both ‘coffee’ and ‘breakfast’ have been performed. Lastly, the goal state is achieved by the action ‘leave’ which requires as precondition that the user is dressed and has cleaned their teeth.

To implement and run your own domain, follow the six step procedure outlined below and create the necessary files. To achieve this, create a new directory under CCBM/Experiments for your domain and place the required files within. You should also have the out/build sub directory as described in the readme file.

## Domain file (d.pddl)
The domain file defines types, predicates and actions according to standard PDDL. However, there is one extra step which is necessary. For each action an observation has to be defined. This observation calls the corresponding function of the observation code (3) and thus the names of both have to match exactly. The following image shows this for the action ‘dress’. Note that the this ‘observation’ clause is not standard PDDL but rather if the given action is performed, the function defined in this clause is called with the given integer parameter. This sets the current activity in the code that handles the observations (3). It can be given any other name, but the names here and in the rest of the code have to match.

```cpp
	(define (domain day)
	  (:types
	    user - object
	  )
	  (:predicates
		(isDressed ?u - user)
		(drankCoffee ?u - user)
		(hadBreakfast ?u - user)
		(isClean ?u - user)
		(hasLeft ?u - user)
	  )
	  (:action dress
		:parameters (?u - user)
		:precondition (not (isDressed ?u))
		:effect (isDressed ?u)
		:observation (setActivity (activity-id dress)
	  )
	)
```

## Problem file (p.pddl)
The problem file also follows standard pddl syntax, where the corresponding domain, objects, goal and initial state have to be defined. In the _:init_ scope it is important to define the activity-id of each action  which is the parameter of the setActivity function. The function takes an integer parameter which should be defined in the following way:

```cpp
	(define (problem day)
		(:domain day)
		(:init
		    (= (activity-id dress) 1)
		    (= (activity-id coffee) 2)
		    (= (activity-id breakfast) 3)
		    (= (activity-id clean) 4)
		    (= (activity-id leave) 5)
		)
    
		(:objects
		  bob - user
		)
		
		(:goal (and
		    (hasLeft bob)
		    )
		)
	)
```

This code snippet shows the problem definition of the domain ‘day’. Each action is given an activity-id, starting from 1. By default, upon calling the function _setActivity(int id)_ the actual activity will be set to _activity = id-1_ to use zero based indexing.

## Observation code (d-observations.cpp)
This file is needed for reading the given observation data and should return the probability of the current activity given the state. Several functions need to be implemented in this file. Firstly, the function that sets the activity needs to be defined:

```cpp
    void setActivity(int agid, int i) {
	    std::cerr << "## setActivity(" << agid << ", " << i << ")" << std::endl;
	    (void) agid; // single-agent model
	    // use zero-based activity
	    theActivity = i - 1;
    }
```

Again, it is necessary that the name of this function and the one in the domain file are the same. In the single agent case the agid parameter can be ignored but the function can also be adapted to handle multi-agent models. The function sets the Activity given as the parameter and writes logging information to the .err files that are created in the /out folder when running a filter.

The function _specialOpCallback(int agid, int opid)_ is called for the special operations INITALIZE, BLOCKED, and FINISHED and writes logging information to the .err files that are created for each filter. In a simple form it is defined as:

```cpp
    void specialOpCallback(int agid, int opid) {
	    //if (opid == -1) {
	    //	setActivity(agid, 6); // set unknown as initial activity
	    //}
	    std::cerr << "## specialOpCallback(" << agid << ", " << opid << ")" << std::endl;
    }
```

One can also define a special action ‘unknown’ that handles the current activity when such a special state occurs.

The next function is _bool fetchObservation()_ which reads the probabilities of activities in the observation data one line (i.e. one state/time step) at a time and stores them in an array of size NUM_ACTIVITIES (defined in header file). The function returns true if it succeeds and false if an error occurs. The boolean firstObs can be set in this file as well. By default its value is true which means that the first observation (i.e. the first row with values in the observation file) is ignored. More specifically the probability of each activity will be set to 1 for the Initial activity. Only the consecutive observations are actually used. If it is set to false the first row will also be used. In the case that observations depend on actions as is the case in this example the first observation corresponds to the special action INITIALIZE which is the first action anyway and will always have the probbaility 1 while all others have 0. Therefore in this case it is insignificant if the first observation is used or not. However, if state estimation is used instead, the first observation corresponds to a distribution over the initial states which may make sense to use by setting firstObs to false. In any case, due to the need of an observation for the intitial state or action the observation file contains an additional observation in the first row.

```cpp
    bool fetchObservation() {
	    std::cin.ignore(100000,'\n'); //ignore the first line
	    if (firstObs) {
	        // the original lisp model does no observation for the initial state, so simply return 1 in the observations
	        for (int i = 0; i < NUM_ACTIVITIES; ++i) {
	            obs[i] = 1;
	        }
	        firstObs = false;
	        return true;
	    }
	    //	std::cin.ignore(100000,'\n');
	
	    for (int i = 0; i < NUM_ACTIVITIES; ++i) {
	        double o;
	        std::cin >> o;
	        std::cerr << "bla" << std::endl;
	        if (!std::cin.good())
	            return false;
					obs[i] = o;
	        std::cerr << o;
	    }
	    std::cerr << std::endl;
	    return true;
	}
```

This function is called at every time step to read an observation from the file specified in the shell script (6).

Lastly, the functions that are called for the update step of particle filtering are given by  _void pre_observe_action(size_t /*agid*/)_ and _Probability post_observe_action(size_t /*agid*/)_ respectively. The inner workings of this are as follows: During the update step of particle filtering first pre_observe() of the model is called, then observations are made, and finally post_observe() is called to compute p(y|x). This can be seen in the file CCBM/src/marginal/particle_single.cpp where the update function of a Particle is defined:

```cpp
	void Particle<Model>::update() {
		...
		// call pre_observe_*(), then do observations, then call post_observe_*() to update the weight
		for (size_t i = 0; i < Model::n_agents; i++)
		    model::pre_observe_action(i);
		model::StatePtr s = &state.modelState->first;
		model::pre_observe_state(s);

		doCallbacksAndObservations();

		// let the model compute pyx (must be independent of action duration!)
		Probability pyx = 1;
		for (size_t i = 0; i < Model::n_agents; i++)
		    pyx *= model::post_observe_action(i);
		pyx *= model::post_observe_state(s);

		set_pyx(pyx);
		updateWeight(pyx);
	}
```

Both, pre- and post- observe functions need to be defined. The former writes  the most likely activity from the observations and the current activity  to the .err output file while the latter also does this but more importantly returns the probability of the activity in the current state that is used to update the particle weight as illustrated above. Note, that these simple examples use action estimation and not the entire state, thus the pre/post_observe _state functions are placeholders.

```cpp
    void pre_observe_action(size_t /*agid*/) {
	    //theActivity = 5;
	    const int N = sizeof(obs) / sizeof(int);
	    int o = std::distance(obs, std::max_element(obs, obs + N));
	    std::cerr << "## pre_observe() (obs="<<o<< ", activity="<< theActivity <<")" << std::endl;
    }
    
    void pre_observe_state(StatePtr /*x*/) { }
    
    Probability post_observe_action(size_t /*agid*/) {
	    const int N = sizeof(obs) / sizeof(int);
	    int o = std::distance(obs, std::max_element(obs, obs + N));
	    std::cerr << "## post_observe() (obs="<< o << ", activity="<< theActivity <<")" << std::endl;
	    return obs[theActivity];
    }
    
    Probability post_observe_state(StatePtr /*x*/) { return 1; }
```

## Observation file header (d-observations.h)
This file defines the number of activities which has to match the number of possible actions in the pddl file. It also contains the function declaration of the _setActivity_ function.

```cpp
    #pragma once
    #define NUM_ACTIVITIES 5
    //#define SUPPORTS_PYA 1
    //#define SUPPORTS_OBS_EQUIV 1
    //#define SUPPORTS_OBS_HASH 1
    
    void setActivity(int agid, int i);
    extern int theActivity;
    
    #define HAS_ESTIMATOR
    
    template <class Particle>
    class Estimate : public ActivityEstimator<Particle, NUM_ACTIVITIES> {};
```

## Observation data (.dat)
These are the observations which are read in by the fetch_observation function and will be used for the filtering process. Several files with different observations can be created here. There are a few things to note: The format should be whitespace separated probability values. The first row contains the names of the actions and will be ignored by the parser. Consequently the table has as many columns as there are actions. The second line will also be ignored if firstObs is set to true in the .cpp file. After this, there have to be as many lines as time steps are needed to reach the goal if no duration model is specified, or the filter will fail. If no duration model is implemented each action takes one time step. The following image shows an example of uniformly distributed observations from the file ‘obs_3.dat’. These observation files may also be placed in a sub folder called ‘data’. These are the values that are read by the _fetchObservation()_ function.

```
    "dress" "coffee" "breakfast" "clean" "leave"
    0.2 0.2 0.2 0.2 0.2
    0.1 0.1 0.1 0.1 0.1
    0.1 0.1 0.1 0.1 0.1
    0.2 0.2 0.2 0.2 0.2
    0.15 0.15 0.15 0.15 0.15
    0.1 0.1 0.1 0.1 0.1
```

## Script to run everything (run.sh)
This is the standard script which is also used for other examples like /Shoes, it can be run by executing the command below, as outlined in the readme file.

```shell
    ./run.sh out
```

It should be noted that this script sets the compiler flag -C "-DUSE_ACTION_ESTIMATION" so if state estimation is required the _post_observe_state_ function in the observations.cpp file should be used and not _post_observe_action_. In this file also the path to the observation data is defined, and the action selection heuristic can be set.

```shell
    #!/bin/bash
    if [[ $# -eq 0 ]]; then echo "Usage: $0 <outdir> <rc options...>"; exit 1; fi
    out="$1"
    shift
    mkdir -p "$out/build"
    ../../rc -M FHMAVSO -n 10000 -d d.pddl -p p.pddl -o d-observations -w "$out/build" -f "$out/p" -C "-DUSE_ACTION_ESTIMATION" "$@"
    if [[ $? -ne 0 ]]; then exit 1; fi
    echo "Running filters ..."
    "$out"/p-analyzer -o "$out"/p.states 2>/dev/null
    "$out"/p-filter -l "$out"/p.states -p "$out"/p.stats <data/obs_4.dat>"$out"/p-filter.dat 2>"$out"/p-filter.err
    "$out"/p-marginal -l "$out"/p.states -p "$out"/p-marginal.stats -S "$out"/p-marginal.smooth -V "$out"/p-marginal.viterbi <data/obs_4.dat >"$out"/p-marginal.dat 2>"$out"/p-marginal.err
```

Upon compiling the code by calling the rc script, we obtain some binaries according to the options we set via the -M command. The available options are: -M [opt]: *F filter, *M marginal, *H HMM, *A analyzer, *V validator, *S simulator, *O obs. Tester, *T ANN training. Then, both the marginal and the particle filter are run. For these also multiple options exist:

If you run the filter with “-?” you get information about the available parameter options:

```
	-l [statefile] load states and goal distances from [statefile]  
	--specificity [file] load specificity table from [file] 
	-A [factor] set weight factor for the saliency to [factor] (currently: 0 )  
	-B [factor] set weight factor for the specificity to [factor] (currently: 0 )  
	--restrict-states [filename] restrict states to be visited to states in file  
	-f [factor] set weight factor to [factor] (currently: -1 )  
	--load-ann [file] loads trained neural network for action selection (created with train tool)  
	-p [statesfile] saves states distribution for all particles  
	-X [distfile] saves full X-State distribution for all particles in [distfile][t].gz, where [t] is the time-step  
	-r set random seed to current time  
	-R [seed] set random seet to [seed]  
	-b [bias] set weight bias to [bias] (currently: 0 )  
	-N, --nparticles [N] set the number of particles to use to [N] (currently: 50000)  
	-h [factor] set the current heuristic to [factor] (default=0: blind heuristic, 1: landmarks, 2: ann)  
	--goal-prior [goal:prob] sets the a-priori goal probability (#goals=1)  
	--init-prior [istate:prob] sets the a-priori probability for initial states (#istates=1)  
	-q [factor] calculates the probability of the next action by taking the [factor] root of the real probability  
	-S [file] runs smoothing after forward filtering and saves estimates to [file]  
	-V [file] runs viterbi after forward filtering and saves estimates to [file]  
	-P [SsTO] Use an alternative particle representation (s sequential (default), S single, T starting times, O observation-equivalent)  
	--shuffle [seed] shuffle the particles prior to pruning  
	-?, --help prints this screen
```

To extend this let us take a closer look at the action selection heuristics. A variety of methods can be chosen from:

-   Goal distance (-l option). Explores the state space and finds the shortest path to goal. You have to run the analyser for that before running the filter
-   Landmarks (-h1 option). Approximates the goal distance based on the preconditions of actions
-   Cognitive heuristics. To use the cognitive heuristics, you have to enable them during filter compilation (-a enable the ACT-R heuristics saliency, recency, refactoriness, specificity).   
	- saliency (-A), saliency assigns weights to actions. You can assign saliency to each action in the domain file in the action template. This is done by adding ':saliency (sal action)' to an action in that file and then specifying the saliencies of each action in the :init clause of the problem file via '(= (sal action) value)'. For example for an action named 'dress' put :saliency (sal dress) in the domain file and then (= (sal dress) 10) in the problem file if you want to assign a saliency of 10 to the action dress.    
	- specificity (-B), how specific an action is based on the number of predicates in the action precondition.  
	- (Only available for filter not marginal filter) recency (-C)  
	- (Only available for filter not marginal filter) refractoriness (-D)
-   (Only available for filter not marginal filter) Revisiting factor (-v option). Allows you to visit already visited states (or to visit only new states)

The output of the marginal filter when using these action selection heuristics with the uniform observations introduced above is illustrated in the following figure. Note that the numbers next to each action in the saliency plot correspond to the assigned saliency of that action. 
![Figure 1](EffectOfASH.png)
## Results of Running the Filters
Running these filters generates a variety of output files in the created folder /out. For example the file _p-marginal.dat_ gives a probability distribution over the actions in each time step. In the case of the uniform observations shown above with using the goal distance action selection (-l option) this output should look as follows:

```
    Time	"FINISHED" "BLOCKED" "INITIALIZE" "(dress bob)" "(coffee bob)" "(breakfast bob)" "(clean bob)" "(leave bob)"
    0	0 0 1 0 0 0 0 0
    1	0 0 0 0.33333333333333331 0.33333333333333331 0.33333333333333331 0 0
    2	0 0 0 0.33333333333333331 0.33333333333333331 0.33333333333333331 0 0
    3	0 0 0 0.16666666666666669 0.33333333333333337 0.33333333333333337 0.16666666666666669 0
    4	0 0 0 0.16666666666666666 0 0 0.83333333333333337 0
    5	0 0 0 0 0 0 0 1
```
