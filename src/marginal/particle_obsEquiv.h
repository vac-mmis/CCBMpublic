#pragma once

/*
 * This header defines a particle class that stores all observation-equivalent <S, A> states for one <T, I, G> state.
 * Observation-equivalence is defined in terms of the model::observationEquivalent predicate
 * The main class is marginal::particle::obsEquiv::Particle
 *
 * We want obsEquiv only in terms of "sequences" p(x_1:t | y_1:t)
 * → Each particle collects all X-states which *necessarily* have the same weight for the current time step
 * This also implies the following:
 * 1)  If particle a leads to state s1 and particle b lead to state s2, and s1,s2 are obsEquiv (but not identical), we do not want to merge s1,s2 into one particle,
 *      because s1 and s2 have (potentially) different weights
 *
 * 2)  We have to separate different "multiplicities" of states into different obsEquiv particles:
 *      For example, let a particle consist of three states {s1, s2, s3},
 *      and we have four successor particles with states s1', s2', s2', s3'
 *      - we separate s2' from s1' and s3', because it has twice the weight (actions could be applied in two states)
 *      as a result, we have two obsEquiv particles {s1', s3'} and {s2'}
 *
 * 3)  If actions have different weights (as per calcWeight), the resulting states cannot be obsEquiv.
 *      Action weights depend on a number of factors: total number of applicable actions in the previous state,
 *      :non-repeating settings, goal distance, action's saliency, …
 *        For example, if we have one particle with obsEquiv states {s1, s2}, and s1 leads to {s3, s4} and s2 to {s5}
 *        Let s3 and s5 be obsEquiv.
 *        Then s5 has a higher probability than s3, and thus both cannot be merged:
 *            if we are in s2, we know with certainty we reach s5; if in s1, we reach s3 only with one of two actions
TODO obsEquiv if weight is the same (and factors may differ), or every factor is the same (and hence the weight)?
 *      For now, we consider only the total actions weight and actually use the particle's weight to find these asymmetries
 *
 * 4)  If Particles have overlapping (i.e. non-empty intersection) sets of X states, we have to separate these X states.
 *      Having identical X states in some particles implies that this X state has a different weight.
 *      For example, let particle a consist of states {s1, s2, s3} and particle b consist of states {s2, s3, s4}.
 *      Then we separate {s2, s3} into a new particle c and let a = {s1} and b={s4}.
 *      Note that s2, s3 in c have the same weight, because they had both in a and b.
 *      If we kept identical X states in different particles, this would also lead to a
 *      potential exponential blowup of obsEquiv particles.
 *
 * Thus, the basic strategy during prediction is:
 * Given one obsEquiv-particle, predict all its successor X-states and merge (only these) according to obsEquiv
 * Thus, one obsEquiv-particle predicts several obsEquiv-particles (up to now, this was independent of other obsEquiv particles)
 * Only *identical* obsEquiv-particles are merged, i.e. where the set of X-states is identical (obsEquiv is not sufficient here)
 *
 * Note: in the current implemantation, this is in no way more efficient than with single particles.
 * It is only used to actually count the number of obsEquiv states
 */

#include <cassert>
#include <iosfwd>
#include <type_traits>
#include <deque>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <algorithm>

#include <boost/functional/hash.hpp>

#include "../util/iterators.h"
#include "../util/distribution.h"
#include "../util/allocator.h"
#include "../util/hash.h"

#include "../stateFunctions.h"
#include "../weight.h"

#include "particle_single.h"

namespace marginal {
namespace particle {
namespace obsEquiv {

//      Forward declarations of relevant classes
// #############################################################################


template <class Model>
class Particle;


namespace detail {

template <class Model>
struct State;

} // namespace marginal::particle::obsEquiv:: detail




//      Class declarations
// #############################################################################



namespace detail {

// equivalence for ::State<Model> wrt. observation-equivalence
template <class Model>
struct Particle_equal_obsEquiv {
    bool operator()(const particle::single::Particle<Model> *s1, const particle::single::Particle<Model> *s2) const;
};

// hash for ::State<Model> wrt. observation-equivalence
template <class Model>
struct Particle_hash_obsEquiv {
    size_t operator()(const particle::single::Particle<Model> *s) const;
};



//      class SA
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// individual <S, A> pair that gets mapped to one Weight in the State
// the equal/hash functions operate on the raw <S, A> state and do not take observation equivalence into account
// - because SA is the key in the map from obsEquiv states to weight
// instead, the method observationEquivalent checks this
template <class Model>
struct SA {
    // the model state
    StateTablePtr modelState;

    // the actions executed by every agent
    typename Model::opid_t opid;

    SA(StateTablePtr modelState, typename Model::opid_t opid) :
        modelState(modelState),
        opid(opid)
    { }

    bool observationEquivalent(const SA &other) const {
        return model::observationEquivalent(&modelState->first, opid, &other.modelState->first, other.opid);
    }

    bool operator==(const SA &other) const {
        if (modelState != other.modelState)
            return false;
        if (opid != other.opid)
            return false;
        return true;
    }

    bool operator!=(const SA &other) const {
        return !(*this == other);
    }
}; // struct SA

template <class Model> size_t hash_value(const SA<Model> &sa);
template <class Model> bool observationEquivalent(const SA<Model> &s1, const SA<Model> &s2);
// convenience function for printing, mainly used for debugging
template <class Model> std::ostream& operator<<(std::ostream &os, const SA<Model> &sa);



//      class State
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// This particle's state consists of <T, I, G> and a set of all observation-equivalent <S, A> states
// All particles with identical <T, I, G> states and observation-equivalent <S, A> states are considered equal and should be merged
// (obsEquiv states shall only be merged if they come from the same previous particle - otherwise, the sequence up to now was not obsEquiv)
// This class's definition is based on ::State<Model>
template <class Model>
struct State:
        ::detail::State_initials<Model>,
        ::detail::State_goals<Model> {

public: // static fields and typedefs
    static const size_t n_agents = Model::n_agents;
    static const size_t num_initials = Model::num_initials;
    static const size_t num_goals = Model::num_goals;

    typedef ::detail::State_initials<Model> State_initials;
    typedef ::detail::State_goals<Model> State_goals;

    typedef typename Model::opid_t opid_t;
    typedef typename ::State<Model>::startTime_t startTime_t;

    typedef detail::SA<Model> SA;

    typedef std::unordered_set<SA, boost::hash<SA>, std::equal_to<SA>, pool_allocator<SA>> sa_set;

public: // fields

    sa_set states;

    // the starting time of the action
    startTime_t startTime;

public: // methods

    // construct a state that represents a single ::State
    State(const ::State<Model> &otherState) :
        State_initials(otherState.initialIndex()),
        State_goals(otherState.goalIndex()),
        startTime(otherState.startTime)
    {
        states.insert(SA(otherState.modelState, otherState.opid));
    }

    // copy from another state, but replace the SA states with a copy of the parameter
    State(const State &other, sa_set states) :
        State_initials(other.initialIndex()),
        State_goals(other.goalIndex()),
        states(std::move(states)),
        startTime(other.startTime)
    { }

    // checks if this State is identical to the other state (esp. same set of states)
    bool operator==(const State &other) const;
    bool operator!=(const State &other) const { return !(*this == other); }
}; // struct State

// hash_value of this state, esp. incorporating all SA states
template <class Model>
size_t hash_value(const State<Model> &state);

// a list of particles, which additionally tracks for every X-state which particle represents it
// Every Particle can represent more X-states, but one X-state should only be represented by one Particle
// (we need this to speed-up finding particles with overlapping X-states)
template <class Model>
class ParticleList {
protected:

    typedef std::deque<Particle<Model>*, pool_allocator<Particle<Model>*>> list_t;
    typedef std::unordered_map< ::State<Model>, Particle<Model>*,
        boost::hash< ::State<Model>>, std::equal_to< ::State<Model>>,
        pool_allocator<std::pair<const ::State<Model>, Particle<Model>*>>> state_map_t;

    list_t list;
    state_map_t state_map;

public:

    typedef Particle<Model>* value_type;
    typedef typename list_t::iterator iterator;
    typedef typename list_t::const_iterator const_iterator;
    typedef typename list_t::reverse_iterator reverse_iterator;
    typedef typename list_t::const_reverse_iterator const_reverse_iterator;

    typedef typename list_t::pointer pointer;
    typedef typename list_t::const_pointer const_pointer;
    typedef typename list_t::reference reference;
    typedef typename list_t::const_reference const_reference;

    ParticleList() = default;
    ParticleList(std::initializer_list<value_type> il) :
        list(il)
    {
        for (Particle<Model> *p : il)
            addStates(p);
    }

    iterator begin() { return list.begin(); }
    iterator end() { return list.end(); }
    const_iterator begin() const { return list.begin(); }
    const_iterator end() const { return list.end(); }

    size_t size() { return list.size(); }
    bool empty() { return list.empty(); }

    reference operator[](size_t n) { return list[n]; }
    reference front() { return list.front(); }
    reference back() { return list.back(); }

    void clear() { list.clear(); state_map.clear(); }

    void push_back(Particle<Model> *particle) {
        list.push_back(particle);
        // new particle inserted → map all its states to it
        addStates(particle);
    }

    // marks all states represented by this particle as such in the internal map
    // This requires this particle is already in the list, but without any state associations
    void addStates(Particle<Model>* particle) {
        for (auto state_w : particle->template getDistribution<::State<Model>>()) {
            const ::State<Model> &state = state_w.first;
            assert(state_map.count(state) == 0);
            state_map[state] = particle;
        }
    }


    void pop_back() { iterator last = end(); --last; erase(last); }
    void pop_front() { erase(begin()); }

    iterator erase(const_iterator pos) {
        Particle<Model> *particle = *pos;
#if __GNUC__ == 4 && __GNUC_MINOR__ < 9
        // work around gcc bug https://gcc.gnu.org/bugzilla/show_bug.cgi?id=54577
        // use iterator arithmetic to convert to a non-const iterator
        iterator pos_ = list.begin() + (pos - list.cbegin());
        iterator result = list.erase(pos_);
#else
        iterator result = list.erase(pos);
#endif

        // particle removed → also remove the state associations
        for (auto state_w : particle->template getDistribution<::State<Model>>()) {
            const ::State<Model> &state = state_w.first;
            assert(state_map.count(state) == 1);
            state_map.erase(state);
        }
        return result;
    }

    // returns the Particle that represents the given state
    // returns NULL if no Particle represents that state
    Particle<Model>* getParticle(const ::State<Model> &state) const {
        auto it = state_map.find(state);
        if (it == state_map.end())
            return NULL;
        return it->second;
    }

    // remove one state from a particle
    void removeState(Particle<Model>* particle, const ::State<Model> &state) {
        auto it = state_map.find(state);
        assert(it != state_map.end() && it->second == particle);
        state_map.erase(it);
    }

    void swap(ParticleList &x) {
        using std::swap;
        swap(list, x.list);
        swap(state_map, x.state_map);
    }

}; // class ParticleList

template <class Model>
void swap(ParticleList<Model> &x, ParticleList<Model> &y) { x.swap(y); }

} // namespace marginal::particle::obsEquiv:: detail



//      class Particle
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


template <class Model_>
class Particle {
    // typedefs and static constants
    // =======================

public:

    struct Options { };

    // no additional statistics
    struct Statistics {
        typedef std::false_type enabled;
        void collect(const Particle&) { }
        static std::ostream& header(std::ostream &os) { return os; }
        friend std::ostream& operator<<(std::ostream &os, const Statistics&) { return os; }
    };

    typedef Model_ Model;

    typedef detail::State<Model> State;
    typedef typename State::SA SA;
    typedef typename State::sa_set sa_set;

    // a ParticleList to build a list of successor particles
    typedef detail::ParticleList<Model> ParticleList;

    // A suitable set implementation that collects instances of this Particle
    // a ParticleSet is used to collect and merge identical particles - this is the actual marginalisation in the marginal filter
    template <class P = Particle*>
    struct ParticleSet : std::unordered_set<P, ParticlePtrState_Hash<P>, ParticlePtrState_Equal<P>, pool_allocator<P> > { };

    // A suitable map implementation that takes a Particle pointer (mostly Particle*) as a key (where Particles are considered equivalent based solely on their state), and T as a value
    // Note: this is necessary because not every Particle class may define a suitable hash function for their state
    // C++11: we might want to use a template <> using directive here
    template <class T, class P = Particle*>
    struct State_Map : std::unordered_map<P, T, ParticlePtrState_Hash<P>, ParticlePtrState_Equal<P>, pool_allocator<std::pair<const P, T> > > { };

    // helper classes and typedefs for returning distributions (as ranges)

    // transform one weighted <S, A> state into some other SubState
    template <class SubState, class Dummy = void>
    struct distfunc_State {
        const Particle::State &state;
        Weight single_weight;
        distfunc_State(const Particle::State &state, Weight single_weight);
        std::pair<SubState, Weight> operator()(const SA &sa) const;
    };

    // transform one weighted <S, A> state into a full weighted ::State
    template <class Dummy>
    struct distfunc_State<::State<Model>, Dummy> {
        const Particle::State &state;
        Weight single_weight;
        distfunc_State(const Particle::State &state, Weight single_weight);
        std::pair<::State<Model>, Weight> operator()(const SA &sa) const;
    };

    template <class SubState>
    using dist_iterator = boost::transform_iterator<distfunc_State<SubState>, typename sa_set::const_iterator, std::pair<SubState, Weight>>;

    // case when full distribution must be returned
    template <class SubState, bool full = (SubState::has_A::value || SubState::has_S::value)>
    struct dist_range : boost::iterator_range<dist_iterator<SubState>> {
        dist_range(const State &state, Weight single_weight) :
            boost::iterator_range<dist_iterator<SubState>>(make_transform_iterator_range<dist_iterator<SubState>>(state.states, distfunc_State<SubState>(state, single_weight)))
        { }
    };
    // ... specialisation when state neither as state nor action and we can marginalise
    template <class SubState>
    struct dist_range<SubState, false> : distribution::single_estimate<SubState> {
        dist_range(const State &state, Weight single_weight) :
            distribution::single_estimate<SubState>(SubState(::State<Model>(state.initialIndex, state.goalIndex, nullptr, typename Model::opid_t(), state.startTime)), single_weight * Weight(state.states.size()))
        { }
    };
    // ... specialisation for full ::State
    template <bool full>
    struct dist_range<::State<Model>, full> : boost::iterator_range<dist_iterator<::State<Model>>> {
        dist_range(const State &state, Weight single_weight) :
            boost::iterator_range<dist_iterator<::State<Model>>>(make_transform_iterator_range<dist_iterator<::State<Model>>>(state.states, distfunc_State<::State<Model>>(state, single_weight)))
        { }
    };


    // static fields
    // =======================

    static Options options;


    // static methods
    // =======================

    // particles are frequently dynamically created and deleted
    // operator new overload that uses the pool_small_allocator for more efficient allocation
    static void* operator new(size_t sz);
    static void operator delete(void* ptr);

    // Add a new particle to the set and particle list
    // The set must contain only particles from the same time step
    static void addParticle(Particle* p, ParticleList& particles, ParticleSet<Particle*>& set);


protected: // hidden fields, some private, other only to be accessed by getters/setters

#if DO_MULTI_AGENT && ALLOW_AGENT_BLOCKING
    bool allBlocked; // if all agents considered so far (during prediction) are blocked, i.e. the agents could neither continue (pStop=1) nor expand (0 applicable actions)
#endif

#if (defined(HAS_CALLBACKS) || defined(USE_ACTION_OBSERVATION))
    // Depending on the observation model, pyx must be cached per particles, or can be saved per state
    Probability pyx;
#endif

    // the weight of the individual states in this particle, i.e. the total weight is single_weight * <number SA states>
    Weight single_weight;

public:
    State state;

public:

    Particle(const State &state, Weight single_weight = Weight(1)) :
#if DO_MULTI_AGENT && ALLOW_AGENT_BLOCKING
        allBlocked(false),
#endif
        single_weight(single_weight),
        state(state)
    { }

    // create a particle that represents a single State from ::State
    Particle(const ::State<Model> &state, Weight weight) :
#if DO_MULTI_AGENT && ALLOW_AGENT_BLOCKING
        allBlocked(false),
#endif
        single_weight(weight),
        state(Particle::State(state)) // explicit qualified Particle::State to make the distinction from ::State clear
    { }

    bool operator==(Particle const & other) const {
        if (state != other.state)
            return false;
        return true;
    }

    bool operator!=(Particle const & other) const { return !(*this == other); }


    Probability get_pyx() const;

    Weight getTotalWeight() const {
        return single_weight * Weight(state.states.size());
    }

    // set the weight for the total particle
    // This is used in resampling, where this particle gets some (constant) weight
    void setTotalWeight(Weight newWeight) {
        Weight factor = newWeight / getTotalWeight();
        updateWeight(factor);
    }

    // returns a distribution that marginalises over all variables not represented by SubState
    // if SubState is ::State, returns the full distribution
    template <class SubState>
    dist_range<SubState> getDistribution() const { return dist_range<SubState>(state, single_weight); }

    size_t getStateCount() const { return state.states.size(); }

    // call all observation callbacks
    void doCallbacksAndObservations();

    // Predict all next states, computing their corresponding probabilities.
    // Observations are not applied.
    static StepResult step(typename Particle<Model>::ParticleList &particles, States &states);

    // Update the weight
    // Multiplies the weights of all <S, A> states with the given probability
    // Basic assumption: all states are observation-equivalent
    void updateWeight(Probability p) { single_weight *= p; }

    // Normalize the weight
    // Divides the weights of all <S, A> states by the given probability
    // Basic assumption: all states are observation-equivalent
    void normalizeWeight(Probability p) { single_weight /= p; }

    // Update step of particle filtering.
    // Computes p(y|x) by calling doCallbacksAndObservations() and update the weight.
    // That means, first pre_observe() of the model is called, then observations are made,
    // and finally post_observe() is called to compute p(y|x).
    // p(y|x) can be accessed via get_pyx()
    void update();

    void set_pyx(Probability _pyx);

    // cannot define isGoalState, because this particle represent different states
    // bool isGoalState() const { return ::isGoalState(state.goalIndex(), state.modelState); }

    // similar for calcWeight
    // static double calcWeight(const Particle& particle, int i, int agid);

protected:
    // Adds particles of the next step to the given list (possibly including the initial
    // particle, if executing no new action is also possible).
    // The caller has to take care of properly deleting the new instances.
    //
    // In either way, the particle will be multiplied with 1-pStop and either
    // * deleted, or
    // * added to the newParticles list
    static void step(Particle* particle, ParticleList& newParticles, ParticleSet<Particle*>& set, States &states);

}; // class Particle



//      Definitions
// #############################################################################


//      classes Particle_equal/hash_obsEquiv
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

namespace detail {

template <class Model>
bool Particle_equal_obsEquiv<Model>::operator()(const particle::single::Particle<Model> *p1, const particle::single::Particle<Model> *p2) const {
    const ::State<Model> &s1 = p1->state;
    const ::State<Model> &s2 = p2->state;
    if (s1.initialIndex != s2.initialIndex)
        return false;
    if (s1.goalIndex != s2.goalIndex)
        return false;
    if (s1.startTime != s2.startTime)
        return false;
    if (!model::observationEquivalent(s1.modelState->first, s1.opid, s2.modelState->first, s2.opid))
        return false;

    return true;
}

template <class Model>
size_t Particle_hash_obsEquiv<Model>::operator()(const particle::single::Particle<Model> *p) const {
    const ::State<Model> &s = p->state;
    size_t hash = model::observation_hash_value(s.modelState->first, s.opid);
    boost::hash_combine(hash, s.initialIndex);
    boost::hash_combine(hash, s.goalIndex);
    boost::hash_combine(hash, s.startTime);
    return hash;
}

} // namespace detail



//      class SA
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


namespace detail {

template <class Model>
size_t hash_value(const SA<Model> &sa) {
    size_t hash = 0;
    boost::hash_combine(hash, sa.modelState);
    boost::hash_combine(hash, sa.opid);
    return hash;
}


template <class Model>
bool observationEquivalent(const SA<Model> &s1, const SA<Model> &s2) {
    return s1.observationEquivalent(s2);
}


template <class Model>
std::ostream& operator<<(std::ostream &os, const SA<Model> &sa) {
    os << '(' << sa.modelState << ", " << sa.opid << ')';
    return os;
}

} // namespace detail


//      class State
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

namespace detail {

template <class Model>
size_t hash_value(const State<Model> &state) {
    size_t hash = 0;
    boost::hash_combine(hash, state.initialIndex);
    boost::hash_combine(hash, state.goalIndex);
    boost::hash_combine(hash, state.startTime);
    boost::hash_combine(hash, hash_unordered<typename State<Model>::sa_set>()(state.states));
    return hash;
}

} // namespace detail


//      class Particle::distfunc_State
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


template <class Model>
template <class SubState, class Dummy>
Particle<Model>::distfunc_State<SubState, Dummy>::distfunc_State(const Particle::State &state, Weight single_weight) :
    state(state),
    single_weight(single_weight)
{ }

template <class Model>
template <class SubState, class Dummy>
std::pair<SubState, Weight> Particle<Model>::distfunc_State<SubState, Dummy>::operator()(const SA &sa) const {
    static_assert(SubState::has_S::value || SubState::has_A::value, "SubState should have a modleState or opid here, otherwise distfunc_SubState should not be used");
    // convert this particle's state and a single SA state to a SubState
    SubState estState(::State<Model>(state.initialIndex(), state.goalIndex(), sa.modelState, sa.opid, state.startTime));
    return std::make_pair(estState, single_weight);
}


template <class Model>
template <class Dummy>
Particle<Model>::distfunc_State<::State<Model>, Dummy>::distfunc_State(const Particle::State &state, Weight single_weight) :
    state(state),
    single_weight(single_weight)
{ }

template <class Model>
template <class Dummy>
std::pair<::State<Model>, Weight> Particle<Model>::distfunc_State<::State<Model>, Dummy>::operator()(const SA &sa) const {
    // convert this particle's state and a single SA state to a full ::State
    ::State<Model> estState(state.initialIndex(), state.goalIndex(), sa.modelState, sa.opid, state.startTime);
    return std::make_pair(estState, single_weight);
}


// Definition of hash

template <class Model>
size_t hash_value(Particle<Model> const & p) {
    size_t hash = 0;
    boost::hash_combine(hash, p.state);
    return hash;
}



//      external explicit template instantiations
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


extern template struct detail::State<model::Model>;
extern template class Particle<model::Model>;


} // namespace marginal::particle:: obsEquiv
} // namespace marginal:: particle
} // namespace marginal
