#pragma once

#include <iosfwd>

#include <boost/functional/hash.hpp>

#include "util/array.h"

/*
Notation:

<S, A, T, I, G> is a tuple representing a single <State, Action, action start Time, Initial state, Goal index> state
*/

//      Forward declarations
// #############################################################################

// Basic class which represents one single estimate
// Note: particles may represent more than one <S, A, T, I, G> state, e.g. when compactly storing all possible starting times
template <class Model>
struct State;

// Classes representing only subsets of the ::State variables, e.g. only <S, A>
// This can be useful if an estimator requires only such a subset and the particle class can efficiently marginalise over the other variables
// We only provide the actually required, most-often used cases
// select: a combination of flags which states to select (e.g. StateVar_A)
template <class Model, int vars>
struct SubState;

// These equal and hash methods for Particle classes are based solely on the Particle's state
// These are are used for merging particles with equal states during prediction
// Only the State class defines the identity of particles for these methods

template<class ParticlePtr>
class ParticlePtrState_Equal;

template<class ParticlePtr>
class ParticlePtrState_Hash;


// compare particles by their weight
template<class Particle>
inline bool compareParticlesDesc(Particle const * const a, Particle const * const b);

// compare particles by their weight
template<class Particle>
inline bool compareParticlesAsc(Particle const * const a, Particle const * const b);


//      Declarations
// #############################################################################



//      Helper classes for ::State
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

namespace detail {




// State_value holds a single value of type T iff enable is true
// otherwise, the get and set methods do nothing
// here: case for enable = false, specialisation below handles enable = true
template<typename T, bool enable>
struct State_value {
    static T value; // a single static value to be returned by operator()

    State_value(T) { } // ignore constructor argument

    operator T() const { return value; }

    T& operator()() {
        return value;
    }

    T const & operator()() const {
        return value;
    }

    void set(T) { }

    bool operator==(const State_value<T, enable> &) const {
        return true;
    }

    bool operator!=(const State_value<T, enable> &) const {
        return false;
    }
}; // struct State_value

template<typename T, bool enable>
T State_value<T, enable>::value = 0;


// template specialisation for State_value with enabled = true
template<typename T>
struct State_value<T, true> {
private:
    T value;
public:
    State_value(T value) :
        value(value) {
    }

    T& operator()() {
        return value;
    }

    operator T() const { return value; }

    T const & operator()() const {
        return value;
    }

    void set(T v) {
        value = v;
    }

    bool operator==(const State_value<T, true> &other) const {
        return value == other.value;
    }

    bool operator!=(const State_value<T, true> &other) const {
        return value != other.value;
    }
}; // struct State_value<T, true>


template<typename T, bool enable>
inline size_t hash_value(const State_value<T, enable> &value) {
    boost::hash<T> hasher;
    return hasher(value());
}

template<>
inline size_t hash_value<int, false>(const State_value<int, false> &) {
    return 1;
}

template<typename T, bool enable>
inline std::ostream& operator<<(std::ostream &os, const State_value<T, enable> &value) {
    os << value();
    return os;
}


// the following classes introduce the actual fields used within a State
// the State class inherits from these classes to include the fields

template <class Model, bool enable = true>
struct State_modelState {
    StateTablePtr modelState;
    State_modelState(StateTablePtr modelState) :
        modelState(modelState)
    { }
    State_modelState(const State_modelState &s) :
        modelState(s.modelState)
    {}
    bool operator==(const State_modelState &rhs) const { return modelState == rhs.modelState; }
    bool operator!=(const State_modelState &rhs) const { return !(*this == rhs); }
};

template <class Model, bool enable = true>
struct State_opid {
    typename Model::opid_t opid;
    State_opid(const typename Model::opid_t &opid) :
        opid(opid)
    { }
    State_opid(const State_opid &s) :
        opid(s.opid)
    {}
    bool operator==(const State_opid &rhs) const { return opid == rhs.opid; }
    bool operator!=(const State_opid &rhs) const { return !(*this == rhs); }
};

template <class Model, bool enable = true>
struct State_startTime {
    typedef util::array<double, Model::n_agents> startTime_t;
    startTime_t startTime;
    State_startTime(const startTime_t &startTime) :
        startTime(startTime)
    { }
    State_startTime(const State_startTime &s) :
        startTime(s.startTime)
    {}
    bool operator==(const State_startTime &rhs) const { return startTime == rhs.startTime; }
    bool operator!=(const State_startTime &rhs) const { return !(*this == rhs); }
};

template <class Model, bool enable = true>
struct State_initials {
    State_value<int, (Model::num_initials > 1)> initialIndex;

    State_initials() :
        initialIndex(0) {
    }

    State_initials(int initialIndex) :
        initialIndex(initialIndex) {
    }
    State_initials(const State_initials &s) :
        initialIndex(s.initialIndex)
    {}
    bool operator==(const State_initials &rhs) const { return initialIndex == rhs.initialIndex; }
    bool operator!=(const State_initials &rhs) const { return !(*this == rhs); }
}; // struct State_initials

template <class Model, bool enable = true>
struct State_goals {
    State_value<int, (Model::num_goals > 1)> goalIndex;

    State_goals() :
        goalIndex(0) {
    }

    State_goals(int goalIndex) :
        goalIndex(goalIndex) {
    }
    State_goals(const State_goals &s) :
        goalIndex(s.goalIndex)
    {}
    bool operator==(const State_goals &rhs) const { return goalIndex == rhs.goalIndex; }
    bool operator!=(const State_goals &rhs) const { return !(*this == rhs); }
}; // struct State_goals


// empty specializations

template <class Model>
struct State_modelState<Model, false> {
    State_modelState(StateTablePtr) { }
    template <bool enable>
    State_modelState(const State_modelState<Model, enable>&) { }

    constexpr bool operator==(const State_modelState&) const { return true; }
    constexpr bool operator!=(const State_modelState&) const { return false; }
};
template <class Model>
struct State_opid<Model, false> {
    State_opid(const typename Model::opid_t&) { }
    template <bool enable>
    State_opid(const State_opid<Model, enable>&) { }

    constexpr bool operator==(const State_opid&) const { return true; }
    constexpr bool operator!=(const State_opid&) const { return false; }
};
template <class Model>
struct State_startTime<Model, false> {
    typedef util::array<double, Model::n_agents> startTime_t;
    State_startTime(const startTime_t&) { }
    template <bool enable>
    State_startTime(const State_startTime<Model, enable>&) { }

    constexpr bool operator==(const State_startTime&) const { return true; }
    constexpr bool operator!=(const State_startTime&) const { return false; }
};
template <class Model>
struct State_initials<Model, false> {
    State_initials(int) { }
    template <bool enable>
    State_initials(const State_initials<Model, enable>&) { }

    constexpr bool operator==(const State_initials&) const { return true; }
    constexpr bool operator!=(const State_initials&) const { return false; }
};
template <class Model>
struct State_goals<Model, false> {
    State_goals(int) { }
    template <bool enable>
    State_goals(const State_goals<Model, enable>&) { }

    constexpr bool operator==(const State_goals&) const { return true; }
    constexpr bool operator!=(const State_goals&) const { return false; }
};
} // namespace detail



//      struct ::State
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// Basic class which represents one single estimate <S, A, T, I, G>
// Note: particles may represent more than one <S, A, T, I, G> state, e.g. when compactly storing all possible starting times
template <class Model>
struct State:
        detail::State_modelState<Model>,
        detail::State_opid<Model>,
        detail::State_startTime<Model>,
        detail::State_initials<Model>,
        detail::State_goals<Model> {

public: // static fields and typedefs
    typedef std::integral_constant<bool, true> has_S;
    typedef std::integral_constant<bool, true> has_A;
    typedef std::integral_constant<bool, true> has_T;
    typedef std::integral_constant<bool, true> has_I;
    typedef std::integral_constant<bool, true> has_G;

    static const size_t n_agents = Model::n_agents;
    static const size_t num_initials = Model::num_initials;
    static const size_t num_goals = Model::num_goals;

    typedef detail::State_modelState<Model> State_modelState;
    typedef detail::State_opid<Model> State_opid;
    typedef detail::State_startTime<Model> State_startTime;
    typedef detail::State_initials<Model> State_initials;
    typedef detail::State_goals<Model> State_goals;

    typedef typename Model::opid_t opid_t;
    typedef typename detail::State_startTime<Model>::startTime_t startTime_t;

public: // methods

    State(int initialIndex, int goalIndex, StateTablePtr modelState, opid_t opid, startTime_t startTime) :
        State_modelState(modelState),
        State_opid(opid),
        State_startTime(startTime),
        State_initials(initialIndex),
        State_goals(goalIndex)
    { }

    bool operator==(const State &other) const {
        if (this->modelState != other.modelState)
            return false;
        if (this->opid != other.opid)
            return false;
        if (this->startTime != other.startTime)
            return false;
        if (this->initialIndex != other.initialIndex)
            return false;
        if (this->goalIndex != other.goalIndex)
            return false;
        return true;
    }

    bool operator!=(const State &other) const {
        return !(*this == other);
    }
}; // struct State

template <class Model>
inline size_t hash_value(const State<Model> &state) {
    size_t hash = 0;
    boost::hash_combine(hash, state.modelState);
    boost::hash_combine(hash, state.opid);
    boost::hash_combine(hash, state.startTime);
    boost::hash_combine(hash, state.initialIndex);
    boost::hash_combine(hash, state.goalIndex);
    return hash;
}



//      struct SubState
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


constexpr int StateVar_S = 1 << 1;
constexpr int StateVar_A = 1 << 2;
constexpr int StateVar_T = 1 << 3;
constexpr int StateVar_I = 1 << 4;
constexpr int StateVar_G = 1 << 5;


template <class Model, int vars = StateVar_S | StateVar_A | StateVar_T | StateVar_I | StateVar_G >
struct SubState:
        detail::State_modelState<Model, (vars & StateVar_S) != 0>,
        detail::State_opid<Model, (vars & StateVar_A) != 0>,
        detail::State_startTime<Model, (vars & StateVar_T) != 0>,
        detail::State_initials<Model, (vars & StateVar_I) != 0>,
        detail::State_goals<Model, (vars & StateVar_G) != 0> {

public: // static fields and typedefs
    typedef std::integral_constant<bool, (vars & StateVar_S) != 0> has_S;
    typedef std::integral_constant<bool, (vars & StateVar_A) != 0> has_A;
    typedef std::integral_constant<bool, (vars & StateVar_T) != 0> has_T;
    typedef std::integral_constant<bool, (vars & StateVar_I) != 0> has_I;
    typedef std::integral_constant<bool, (vars & StateVar_G) != 0> has_G;

    typedef detail::State_modelState <Model, has_S::value> State_modelState;
    typedef detail::State_opid       <Model, has_A::value> State_opid;
    typedef detail::State_startTime  <Model, has_T::value> State_startTime;
    typedef detail::State_initials   <Model, has_I::value> State_initials;
    typedef detail::State_goals      <Model, has_G::value> State_goals;

    typedef typename Model::opid_t opid_t;
    typedef typename detail::State_startTime<Model>::startTime_t startTime_t;

public: // methods

    // construct from a full ::State
    SubState(const State<Model> &s) :
        State_modelState(s.modelState),
        State_opid(s.opid),
        State_startTime(s.startTime),
        State_initials(s.initialIndex),
        State_goals(s.goalIndex)
    { }

    // construct from a SubState with more variables
    template <int vs>
    SubState(const SubState<Model, vs> &s) :
        State_modelState(static_cast<State_modelState>(s)),
        State_opid(static_cast<State_opid>(s)),
        State_startTime(static_cast<State_startTime>(s)),
        State_initials(static_cast<State_initials>(s)),
        State_goals(static_cast<State_goals>(s))
    {
        static_assert((vars & vs) == vars, "the other SubState must have at least the variables of this SubState");
    }

    bool operator==(const SubState &other) const {
        // the base classes implement the comparison operator even if not enabled
        // (in which case they return always false for !=)
        if (static_cast<State_modelState>(*this) != static_cast<State_modelState>(other))
            return false;
        if (static_cast<State_opid>(*this) != static_cast<State_opid>(other))
            return false;
        if (static_cast<State_startTime>(*this) != static_cast<State_startTime>(other))
            return false;
        if (static_cast<State_initials>(*this) != static_cast<State_initials>(other))
            return false;
        if (static_cast<State_goals>(*this) != static_cast<State_goals>(other))
            return false;
        return true;
    }

    bool operator!=(const SubState &other) const {
        return !(*this == other);
    }
}; // struct SubState



//      struct StepResult
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


struct StepResult {
    // how many particles are dead after prediction
    size_t numDead = 0;
    // the sum of the weights of the dead particles
    Sum<Probability> weightDead;
};




/*
The filters may run on different implementations for a Particle (particle filter, marginal filter).
The common base for all particles is the following interface

class Particle {

    // typedefs and static constants
    // =======================

    // some struct containing parameters to configure this particles behaviour
    typedef ... Options;

    // a class which can collect particle-specific "state statistics"
    // needs to provide method header, collect and operator<<
    typedef ... Statistics;

    // the model this particle is for
    typedef ... Model;

    // the state type that represents the particle's internal state
    // Particle::State is supposed to have equality and inequality to be defined, along with hash_value
    typedef ... State;

    // Particles equality based on their state
    typedef ... State_Equal;

    // a ParticleList to build a list of successor particles
    typedef ... ParticleList;

    // A suitable set implementation that collects instances of this Particles
    // a ParticleSet is used to collect and merge identical particles - this is the actual marginalisation in the marginal filter
    typedef ... ParticleSet;

    // A suitable map implementation that takes Particles* as a key (where Particles are considered equivalent based solely on their state), and T as a value
    // Note: this is necessary because not every Particle class may define a suitable hash function for their state
    template <class T>
    struct State_Map;

    // a range (pair of iterators) over std::pair<SubState, Weight>, where State may be ::State or any ::SubState
    // if a particle represents more states, it may (not must) marginalise over all variables not contained in the SubState (more efficiently than you may do)
    // it is thus recommended to request only the state variables you need (e.g. for estimates)
    template <class State>
    struct dist_range;


    // static fields
    // =======================

    // we probably do not need different parameterisation for different particles,
    // at most for different filters (but even that not right now)
    // therefore, we can simply make this static
    // these options are required to be set before particle creation
    // behaviour is currently undefined if these are changed during the lifetime of a particle
    static Options options;


    // static methods
    // =======================

    // Add a new particle to the set and particle list, possibly merging this particle into another existing particle
    static void addParticle(Particle* p, ParticleList &particles, ParticleSet &set);


    // methods
    // =======================

    // constructors
    Particle(::State state, Weight weight);
    Particle(const Particle &particle);


    // a sequence (represented by a range) of weighted states this particle represents
    // these states are X-States <S, A, T, I, G> = ::State, or any SubState in which case
    // the particle may (not must) marginalise over all variables not in the SubState
    dist_range getDistribution() const;

    // return the number of X-States this particle represents
    // static_cast<size_t>(-1) denotes infinity
    size_t getStateCount() const;

    Probability get_pyx() const;

    Weight getTotalWeight() const;

    // set the weight for the total particle
    // This is used in resampling, where this particle gets some (constant) weight
    // The current strategy is to scale all starting times weights' such that the totalWeight is set
    // (alternatively, the starting times can get the same weight w/n, or we may even delete some starting times, but this only approximates the density)
    void setTotalWeight(Weight newWeight);

    // call all observation callbacks
    void doCallbacksAndObservations();

    // Predict all next states, computing their corresponding probabilities.
    // Observations are not applied.
    static StepResult step(ParticleList &particles);

    // Update the weight
    // Multiplies this particle's weight(s) with the given probability
    void updateWeight(Probability p);

    // Divides the particle's weight(s) by the given probability
    void normalizeWeight(Probability p);

    // Update step of particle filtering.
    // Computes p(y|x) (by calling doCallbacksAndObservations()) and update the weight.
    // That means, first pre_observe() of the model is called, then observations are made,
    // and finally post_observe() is called to compute p(y|x).
    // p(y|x) can be accessed via get_pyx()
    void update();
}


*/



//      Equal/Hash class for Particle states
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

template<class ParticlePtr>
class ParticlePtrState_Equal {
public:
    bool operator()(ParticlePtr const a, ParticlePtr const b) const {
        return a->state == b->state;
    }
};

template<class ParticlePtr>
class ParticlePtrState_Hash {
public:
    size_t operator()(ParticlePtr const p) const {
        boost::hash<typename ParticlePtr::element_type::State> hasher;
        return hasher(p->state);
    }
};

// partial specialisation for raw pointers
template<class Particle>
class ParticlePtrState_Hash<Particle*> {
public:
    size_t operator()(Particle* const p) const {
        boost::hash<typename Particle::State> hasher;
        return hasher(p->state);
    }
};



//      Definitions
// #############################################################################




// compare particles by their weight
template<class Particle>
inline bool compareParticlesDesc(Particle const * const a, Particle const * const b) {
    // particles with biggest total weight (over all starting times) go first
    return prob(a->getTotalWeight()) > prob(b->getTotalWeight());
}

// compare particles by their weight
template<class Particle>
inline bool compareParticlesAsc(Particle const * const a, Particle const * const b) {
    // particles with smallest total weight (over all starting times) go first
    return prob(a->getTotalWeight()) < prob(b->getTotalWeight());
}
