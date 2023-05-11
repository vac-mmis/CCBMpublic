#pragma once

/*
 * This header defines a particle class that stores all possible starting times of <S, A, I, G> states.
 * It makes use of the fact, that observations and state transitions (except for their probabilities) are independent of the starting time
 * The main class is marginal::particle::startTimes::Particle
 *
 * Every particles maintains a list of log-linear models approximating the distribution of the starting times.
 */

#include <list>
#include <deque>
#include <unordered_map>
#include <unordered_set>

#include <boost/functional/hash.hpp>

#include "../util/linear_model.h"
#include "../util/iterators.h"
#include "../util/distribution.h"
#include "../util/allocator.h"

#include "../weight.h"
#include "../stateFunctions.h"


namespace marginal {
namespace particle {
namespace startTimes {

//      Forward declarations of relevant classes
// #############################################################################



template <class Model>
struct State;

template <class Model>
class Particle;



//      Class declarations
// #############################################################################


namespace detail {

//      class TimeModel
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// A distribution of starting times for one particle
// move semantics: this class may be moved (moves the segments)
template <class Model>
class TimeModel {
public:
    typedef typename Model::opid_t opid_t;
    typedef typename ::State<Model>::startTime_t startTime_t;

    // definitions to access this model as a map <startTime_t, Weight>
    typedef std::pair<startTime_t, Weight> value_type;
    typedef value_type& reference;

    // provide a const_iterator over all startTime values represented by this model
    struct const_iterator;
    // we do not prove a mutable iterator, because we cannot alter arbitrary weights at specific time-points
    typedef const_iterator iterator;


private:
    typedef linmodel::LogSegment<false> LogSegment;
    typedef std::list<LogSegment, pool_small_allocator<LogSegment>> segments_t;

    // the segments approximating the log-values of the weights for the starting times
    // we use a list to ensure O(1) merging (O(1) delete of a segment).
    // we (until now) never need random access
    segments_t segments;

    // the pStop log-values for the line segments (interpolated at both endpoints)
    // must be updated by calling compute_pStop
    segments_t pStop_segments;

    // an iterator to the last segment that was tested if it should be merged
    // std::list::iterators are only invalidated on erase
    // this gets reset when calling the move constructor
    segments_t::iterator merge_segment = segments.end();

    mutable bool totalWeightCache_valid = false;
    mutable Probability totalWeightCache;

    // computes the total weight of the segments given as arguments
    static LogProbability totalWeight(const segments_t &segments);

public:

    TimeModel() = default;
    // resets the merge_segment iterator
    TimeModel(const TimeModel &other);
    // resets the merge_segment iterator
    TimeModel& operator=(const TimeModel &other);

    // have to explicitly define move operations, because iterator is not moveable
    // when move-constructing, we reset the merge_segment
    TimeModel(TimeModel &&other);
    TimeModel& operator=(TimeModel &&other);

    // create a time model with a single starting time
    TimeModel(double t, Probability p);

    const_iterator begin() const;
    const_iterator end() const;
    const_iterator cbegin() const;
    const_iterator cend() const;

    Probability getTotalWeight() const;

    // compute the pStop probabilities for the different starting times
    // saves the result internally
    // call get_pStop to get the summed pStop probability
    void compute_pStop(const opid_t &opid);

    // return the summed probabilities of all w*pStop values (i.e. pStop * weight of the particle at that time)
    // - this is the probability that a new action can be executed
    Probability get_pStopW() const;

    // multiply all starting time probabilities with 1-pStop
    // make sure to call compute_pStop beforehand
    void pStop1m();

    // get the total number of discrete time points covered by this TimeModel
    size_t size() const;

    size_t numSegments() const { return segments.size(); }

    // add a new time t with probability p
    // depending on the configuration, this may merge and/or create line segments in the internal representation
    // zero values are simply ignored (at the end of a line segment, they do not extend the segment,
    // within the segment, they do not change the estimate)
    void add_point(double t, LogProbability p);

    // add another time model by adding the probabilities for every starting time
    // this will create an internal copy of the rhs
    TimeModel& operator+=(const TimeModel &rhs) { TimeModel copy(rhs); *this += copy; return *this; }

    // add another time model by adding the probabilities for every starting time
    // this will change the TimeModel, pass a const-ref or a copy if you want to re-use rhs
    TimeModel& operator+=(TimeModel &m);

    // scale every line segment by a constant factor
    TimeModel& operator*=(LogProbability value);

    // divide every line segment by a constant factor
    TimeModel& operator/=(LogProbability value);

}; // class TimeModel

// provide a const_iterator over all startTime values represented by this model
// we do not prove a mutable iterator, because we cannot alter arbitrary weights at specific time-points
template <class Model>
struct TimeModel<Model>::const_iterator : public std::iterator<std::bidirectional_iterator_tag, value_type> {
    const segments_t *segments;
    // iterator to current segment
    segments_t::const_iterator it_s;
    // the current value, saved as a field to be able to return reference in operator*
    value_type value;

    // default-constructor
    const_iterator();

    // construct as past-end iterator
    const_iterator(const segments_t *segments);

    // construct using dereferencable segment iterator
    const_iterator(const segments_t *segments, segments_t::const_iterator it_s);

    const value_type& operator*() const { return value; }

    const_iterator& operator++();
    const_iterator& operator--();

    const_iterator operator++(int) { const_iterator copy(*this); operator++(); return copy; }
    const_iterator operator--(int) { const_iterator copy(*this); operator--(); return copy; }

    bool operator==(const const_iterator &rhs) const { return it_s == rhs.it_s && value.first == rhs.value.first && segments == rhs.segments; }
    bool operator!=(const const_iterator &rhs) const { return !(*this == rhs); }

}; // struct TimeModel<Model>::const_iterator

} // namespace marginal::particle::startTimes :: detail



//      class Options
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// Options parameterising the particle
struct Options {
    // minimum number of points per line-segment
    unsigned min_points;
    // allowed error before creating a new line segment
    double split_error;
    // allowed error before creating a new line segment, even if min_points has not been reached
    double split_early_error;
    // allowed error when merging two existing line-segments
    double merge_error;
    // additional exponent to allow for more errors in regions of low probability
    // the error is computed by abs(true - merged)/abs(true)^merge_exp
    double merge_exp;

    // parameter influencing how many random segments to try to merge
    // if segments may be merged with a probability p (depends on model and observations),
    // and you want an expected number of line segments N, then set a to
    // a = 1/N * e^(1/p)
    double merge_a;

    // minimum log-weight of starting times to consider
    // any line segment with all points having fewer weight will be dropped
    // any value ≥ 0 will never drop segments
    double min_logWeight;

    Options() :
        min_points(10),
        split_error(0.05),
        split_early_error(0.2),
        merge_error(0.05),
        merge_exp(1.05),
        merge_a(3), // N=50 and p=0.2
        min_logWeight(-std::numeric_limits<double>::infinity())
    { }
};



//      class State
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// This particle's state consists of <S, A, I, G> and a set of all corresponding starting times
// Therefore, equality and hash functions do not take the starting times into account:
// All particles with identical <S, A, I, G> states are considered equal and should be merged
// This class's definition is based on ::State<Model>, but includes a set of startTimes and weights
// move semantics: this class may be moved
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

public: // fields
    // the model state
    StateTablePtr modelState;

    // the actions executed by every agent
    opid_t opid;

    // the line-segments approximating the log-weight
    detail::TimeModel<Model> startTimes;

public: // methods

    // construct a state that represents a single ::State
    State(const ::State<Model> &otherState, Weight weight) :
        State_initials(otherState.initialIndex()),
        State_goals(otherState.goalIndex()),
        modelState(otherState.modelState),
        opid(otherState.opid),
        startTimes()
    {
        // TODO multi-agent model
        startTimes.add_point(otherState.startTime[0], weight);
    }

    // copy the state, except for the startTimes which is copied from the second parameter
    State(const State &other, detail::TimeModel<Model> startTimes) :
        State_initials(other.initialIndex()),
        State_goals(other.goalIndex()),
        modelState(other.modelState),
        opid(other.opid),
        startTimes(std::move(startTimes))
    { }

    bool operator==(const State &other) const {
        if (modelState != other.modelState)
            return false;
        if (this->opid != other.opid)
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
    boost::hash_combine(hash, state.initialIndex);
    boost::hash_combine(hash, state.goalIndex);
    return hash;
}


//      class Statistics
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

// collects additional state statistics
template <class Model>
struct Statistics {
    typedef std::true_type enabled;
    size_t numSegments = 0;
    static std::ostream& header(std::ostream &os) { os << "numSegments"; return os; }
    void collect(const Particle<Model> &p) { numSegments += p.state.startTimes.numSegments(); }
    friend std::ostream& operator<<(std::ostream &os, const Statistics &s) { os << s.numSegments; return os; }
};


//      class Particle
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


template <class Model_>
class Particle {
public:
    // typedefs and static constants
    // =======================

    typedef Model_ Model;
    typedef startTimes::Options Options;
    typedef startTimes::State<Model> State;

    typedef startTimes::Statistics<Model> Statistics;

    typedef detail::TimeModel<Model> TimeModel;
    typedef typename TimeModel::startTime_t startTime_t;

    // a ParticleList to build a list of successor particles
    typedef std::deque<Particle*, pool_allocator<Particle*> > ParticleList;

    // A suitable set implementation that collects instances of this Particle
    // a ParticleSet is used to collect and merge identical particles - this is the actual marginalisation in the marginal filter
    // C++11: we might want to use a template <> using directive here
    template <class P = Particle*>
    struct ParticleSet : std::unordered_set<P, ParticlePtrState_Hash<P>, ParticlePtrState_Equal<P>, pool_allocator<P> > { };

    // A suitable map implementation that takes Particle* as a key (where Particles are considered equivalent based solely on their state), and T as a value
    // Note: this is necessary because not every Particle class may define a suitable hash function for their state
    // C++11: we might want to use a template <> using directive here
    template <class T, class P = Particle*>
    struct State_Map : std::unordered_map<P, T, ParticlePtrState_Hash<P>, ParticlePtrState_Equal<P>, pool_allocator<std::pair<const P, T> > > { };

    // helper classes and typedefs for returning distributions (as ranges)

    // need a Dummy template parameter, because only partial (and no explicit) nested class specialisation is allowed
    template <class SubState, class Dummy = void>
    struct distfunc_State {
        const State &state;
        distfunc_State(const State &state) : state(state) { }
        std::pair<SubState, Weight> operator()(const std::pair<const startTime_t, Weight> &startTime) const;
    };
    template <class Dummy>
    struct distfunc_State<::State<Model>, Dummy> {
        const State &state;
        distfunc_State(const State &state) : state(state) { }
        std::pair<::State<Model>, Weight> operator()(const std::pair<const startTime_t, Weight> &startTime) const;
    };

    typedef boost::transform_iterator<distfunc_State<::State<Model>>, typename TimeModel::iterator, std::pair<const ::State<Model>, const Weight>> full_dist_iterator;

    // iterator and range to return a marginalised sub-state (e.g. marginalised over all starting times of this particle)
    template <class SubState>
    using substate_dist_iterator = boost::transform_iterator<distfunc_State<SubState>, typename TimeModel::iterator, std::pair<const SubState, const Weight>>;

    // template struct dist_range<State> is the primary template for returning the state distribution of this particle
    // the specialisation dist_range<::State<Model>> returns a distribution over full ::State, i.e. all X-States represented by this model
    // the main purpose of this template is the specialisation dist_range<SubState>, where SubState is not startingTime field -
    // in this case, the distribution can be a single state with the total weight of this particle. This makes estimation much faster when
    // it does not need to iterate over every starting time explicitly (which it would with ::State)
    // the first dist_range<SubState> definition is for other SubStates which do have the startingTime field (just for completeness)

    // dist_range is either a range over all starting times (if SubState has startingTime)
    template <class SubState, bool full = SubState::has_T::value>
    struct dist_range : boost::iterator_range<substate_dist_iterator<SubState>> {
        dist_range(const State &state, Weight) :
            boost::iterator_range<substate_dist_iterator<SubState>>(make_transform_iterator_range<substate_dist_iterator<SubState>>(state.startTimes, distfunc_State<SubState>(state)))
        { }
    };
    // ... or is a single estimate if we can marginalise over the starting times (i.e. SubState does not have the startingTime)
    template <class SubState>
    struct dist_range<SubState, false> : distribution::single_estimate<SubState> {
        dist_range(const State &state, Weight totalWeight) :
            distribution::single_estimate<SubState>(SubState(::State<Model>(state.initialIndex, state.goalIndex, state.modelState, state.opid, startTime_t())), totalWeight)
        { }
    };
    // ... and in order to use a single signature to get full ::State estimates, a specialization for ::State
    template <bool full>
    struct dist_range<::State<Model>, full> : boost::iterator_range<full_dist_iterator> {
        dist_range(const State &state, Weight) :
            boost::iterator_range<full_dist_iterator>(make_transform_iterator_range<full_dist_iterator>(state.startTimes, distfunc_State<::State<Model>>(state)))
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


protected:
    // hidden fields, some private, other only to be accessed by getters/setters
    // =======================

#if DO_MULTI_AGENT && ALLOW_AGENT_BLOCKING
    bool allBlocked; // if all agents considered so far (during prediction) are blocked, i.e. the agents could neither continue (pStop=1) nor expand (0 applicable actions)
#endif

#if (defined(HAS_CALLBACKS) || defined(USE_ACTION_OBSERVATION))
    // Depending on the observation model, pyx must be cached per particles, or can be saved per state
    Probability pyx;
#endif

public:
    State state;


public:

    Particle(const State &state) :
#if DO_MULTI_AGENT && ALLOW_AGENT_BLOCKING
        allBlocked(false),
#endif
        state(state)
    { }

    // create a particle that represents a single starting-time from ::State
    Particle(const ::State<Model> &state, Weight weight) :
#if DO_MULTI_AGENT && ALLOW_AGENT_BLOCKING
        allBlocked(false),
#endif
        state(Particle::State(state, weight))
    { }

    bool operator==(Particle const & other) const {
        if (state != other.state)
            return false;
        return true;
    }

    bool operator!=(Particle const & other) const { return !(*this == other); }


    Probability get_pyx() const;

    Weight getTotalWeight() const { return state.startTimes.getTotalWeight(); }

    // set the weight for the total particle
    // This is used in resampling, where this particle gets some (constant) weight
    // The current strategy is to scale all starting times weights' such that the totalWeight is set
    // (alternatively, the starting times can get the same weight w/n, or we may even delete some starting times, but this only approximates the density)
    void setTotalWeight(Weight newWeight) {
        Weight factor = newWeight / state.startTimes.getTotalWeight();
        updateWeight(factor);
    }

    size_t getStateCount() const { return state.startTimes.size(); }

    // returns a distribution that marginalises over all variables not represented by SubState
    // if SubState is ::State, returns the full distribution
    template <class SubState>
    dist_range<SubState> getDistribution() const { return dist_range<SubState>(state, getTotalWeight()); }

    // call all observation callbacks
    void doCallbacksAndObservations();

    // Predict all next states, computing their corresponding probabilities.
    // Observations are not applied.
    static StepResult step(ParticleList &particles, States &states);

    // Update the weight
    // Multiplies the weights of all starting times with the given probability
    // Basic assumption: Observation does not depend on the duration of the action
    // All zero weights are removed
    void updateWeight(Probability p) { state.startTimes *= p; }

    // Normalize the weight
    // Divides the weights of all starting times by the given probability
    // Basic assumption: Observation does not depend on the duration of the action
    // All zero weights are removed
    void normalizeWeight(Probability p) { state.startTimes /= p; }

    // Update step of particle filtering.
    // Computes p(y|x) by calling doCallbacksAndObservations() and update the weight.
    // That means, first pre_observe() of the model is called, then observations are made,
    // and finally post_observe() is called to compute p(y|x).
    // p(y|x) can be accessed via get_pyx()
    void update();

    void set_pyx(Probability _pyx);

    bool isGoalState() const { return ::isGoalState(state.goalIndex(), state.modelState); }

    static double calcWeight(const Particle& particle, int i, int agid);

protected:
    // Adds particles of the next step to the given list (possibly including the initial
    // particle, if executing no new action is also possible).
    // The caller has to take care of properly deleting the new instances.
    //
    // the current particle will have pStop computed, but not be multiplied with 1-pStop or added
    // to the list
    static void step(Particle* particle, ParticleList& newParticles, ParticleSet<Particle*>& set, States &states);

}; // class Particle



//      Definitions
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


template <class Model>
template <class SubState, class Dummy>
std::pair<SubState, Weight> Particle<Model>::distfunc_State<SubState, Dummy>::operator()(const std::pair<const startTime_t, Weight> &startTime) const {
    static_assert(SubState::has_T::value, "SubState should have a starting time here, otherwise distfunc_SubState should not be used");
    // convert this particle's state and a single starting time to a SubState containing the startingTime
    SubState estState (::State<Model>(state.initialIndex(), state.goalIndex(), state.modelState, state.opid, startTime.first));
    return std::make_pair(estState, startTime.second);
}


template <class Model>
template <class Dummy>
std::pair<::State<Model>, Weight> Particle<Model>::distfunc_State<::State<Model>, Dummy>::operator()(const std::pair<const startTime_t, Weight> &startTime) const {
    // convert this particle's state and a single starting time to a full ::State
    ::State<Model> estState(state.initialIndex(), state.goalIndex(), state.modelState, state.opid, startTime.first);
    return std::make_pair(estState, startTime.second);
}



//      external explicit template instantiations
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


extern template class detail::TimeModel<model::Model>;
extern template struct State<model::Model>;
extern template class Particle<model::Model>;


} // namespace marginal::particle:: startTimes
} // namespace marginal:: particle
} // namespace marginal
