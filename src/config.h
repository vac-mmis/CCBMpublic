#pragma once

#ifdef USE_ACTR_HEURISTICS
const static int REFRACTORINESS_ACTIONS = 10;
const static int REFRACTORINESS_TIME = 60;
#endif // USE_ACTR_HEURISTICS


// USE_LOG_PROBS forces all Probailities to be represented in the log-Domain
// USE_LOG_WEIGHTS forces only the Weights to use LogProbabilities
#if defined(USE_LOG_PROBS) && defined(USE_LOG_WEIGHTS)
#error "Using USE_LOG_PROBS includes USE_LOG_WEIGHTS, you must define only one."
#endif

#if !defined(HAS_STATE_SPACE_ANALYZER)
#define HAS_STATE_SPACE_ANALYZER 0
#endif

#if !defined(HAS_MARGINAL_FILTER)
#define HAS_MARGINAL_FILTER 0
#endif


// define some default preprocessor macros

// ===============================================
// macros influencing filtering behaviour

// In multi-agent models, what should happen if some agent is blocked, i.e. can neither continue (pStop=1) nor expand (0 applicable actions)?
// In any case, of *all* agents are blocked, this particle is thrown away
// This macro defines what happens if only some are blocked, but at least one is not:
// Usually (no ALLOW_AGENT_BLOCKING), this particle is thrown away (gets zero weight)
// If this is defined (old, deprecated behaviour), this agent is executing a dummy NoOp marking that it is blocked
#ifndef ALLOW_AGENT_BLOCKING
#define ALLOW_AGENT_BLOCKING 0
#endif

// spare particles for the marginal filter
#if defined(SPARE_PARTICLES_N) && defined(SPARE_PARTICLES)
#error "Must only define one of SPARE_PARTICLES_N and SPARE_PARTICLES"
#endif

#ifndef SPARE_PARTICLES_N
// by default, use at most 10 times more spare particles than particles
#define SPARE_PARTICLES_N 10
#endif

#ifndef SPARE_PARTICLES
#define SPARE_PARTICLES SPARE_PARTICLES_N * Global::nparticles
#endif

// Limit the number of distinct <S,A> states for the marginal filter
// this forces the particles to concentrate on different <I, G, T> states
// (used for building distributions of starting times T)
#ifndef LIMIT_STATES
#define LIMIT_STATES 0
#endif



// macros defining the behaviour when writing state statistics:

// if non-zero, print how many particles (or marginal states) are currently active
#ifndef STATESTAT_NUM_PARTICLES
#define STATESTAT_NUM_PARTICLES 1
#endif

// if non-zero, print how many states currently have non-zero probability
#ifndef STATESTAT_NUM_STATES
#define STATESTAT_NUM_STATES 1
#endif

// count how many sets of obsEquiv states there are (only valid for obsEquiv Particles)
// this set is smaller than nparticles, because some particles are in the same state,
// but execute different actions
#ifndef STATESTAT_OBSEQUIV_NSETS
#define STATESTAT_OBSEQUIV_NSETS 0
#endif

// if non-zero, print how many X-states (individual starting times) are currently active
#ifndef STATESTAT_NUM_XSTATES
#define STATESTAT_NUM_XSTATES 1
#endif

// if non-zero, print the minimum non-zero probability of a current state
// This adds an additional field (for summing over all particles) to the StateDataRec.
#ifndef STATESTAT_MIN_PX
#define STATESTAT_MIN_PX 0
#endif

// if non-zero, for each active state print how many particles support this state
#ifndef STATESTAT_STATE_PARTICLES
#define STATESTAT_STATE_PARTICLES 0
#endif

// if non-zero, for each active state x print p(y|'x')
#ifndef STATESTAT_STATE_PYX
#define STATESTAT_STATE_PYX 0
#endif

#if STATESTAT_STATE_PYX && defined(HAS_CALLBACKS)
#error "Invalid use of HAS_CALLBACKS and STATESTAT_STATE_PYX"
#endif

// if non-zero, count the number of unique particles
// This is only useful for particle filter, the marginal filter's STATESTAT_NUM_PARTICLES is exactly this number
#ifndef STATESTAT_UNIQ_PARTICLES
#define STATESTAT_UNIQ_PARTICLES 0
#endif

// print each particle's weights
#ifndef STATESTAT_PARTICLE_WEIGHTS
#define STATESTAT_PARTICLE_WEIGHTS 0
#endif


// ===============================================
// statistics to be output at the end of the filtering run

// if non-zero, print the total number of states reached by any particle
// This adds an additional boolean field to StateDataRec
#ifndef ENDSTAT_REACH_STATES
#define ENDSTAT_REACH_STATES 0
#endif

// if non-zero, print statistics of the state-hashtable at the end
#ifndef ENDSTAT_STATEHASH
#define ENDSTAT_STATEHASH 1
#endif

// ===============================================
// define which fields are required

// if we need the StateStatMap at all
#if STATESTAT_STATE_PARTICLES || STATESTAT__NPARTICLES || STATESTAT_MIN_PX || STATESTAT_NUM_STATES
#define STATESTAT__STATMAP 1
#else
#define STATESTAT__STATMAP 0
#endif

// when we need the nParticles field
#if STATESTAT_NUM_PARTICLES || STATESTAT_STATE_PYX
#define STATESTAT__NPARTICLES 1
#else
#define STATESTAT__NPARTICLES 0
#endif


// ===============================================
// implementation-related quirks

// By default, custom allocators are used for better performance
// However, this can complicate debugging (e.g. using valgrind), so it can be disabled by defining MEMPOOL=0
#ifndef MEMPOOL
#define MEMPOOL 1
#endif
