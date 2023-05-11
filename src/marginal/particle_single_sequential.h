#pragma once

/*
 * This header defines the a particle class that prunes states after every agent.
 * This reduces the exponential number of actions (for all combinations of all agents) to a linear number
 * Another advantage is that this implementation is identical for single and multi-agent models.
 *
 * The main class is marginal::particle::sequential::Particle
 */

#include <unordered_map>
#include <unordered_set>
#include <deque>

#include "../util/iterators.h"
#include "../util/distribution.h"
#include "../util/allocator.h"

#include "../weight.h"
#include "../stateFunctions.h"

#include "prune.h"

namespace marginal {
namespace particle {
namespace sequential {

//      Forward declarations of relevant classes
// #############################################################################


template <class Model>
class Particle;



//      Class declarations
// #############################################################################




template <class Model_>
class Particle {
public:

    // typedefs and static constants
    // =======================

    // only option: pruning strategy to use between actions
    struct Options {
        std::shared_ptr<Prune<Particle>> prune;
        // if with every additional agent more intermediate particles shall be used
        // false results in strict pruning to the maximum nparticles
        bool prune_agent_increasing = true;
    };

    // no additional statistics
    struct Statistics {
        typedef std::false_type enabled;
        void collect(const Particle&) { }
        static std::ostream& header(std::ostream &os) { return os; }
        friend std::ostream& operator<<(std::ostream &os, const Statistics&) { return os; }
    };

    typedef Model_ Model;

    typedef ::State<Model> State;

    // a ParticleList to build a list of successor particles
    typedef std::deque<Particle*, pool_allocator<Particle*> > ParticleList;

    // A suitable set implementation that collects instances of pointers to this Particle
    // a ParticleSet is used to collect and merge identical particles - this is the actual marginalisation in the marginal filter
    // C++11: we might want to use a template <> using directive here
    template <class P = Particle*>
    struct ParticleSet : std::unordered_set<P, ParticlePtrState_Hash<P>, ParticlePtrState_Equal<P>, pool_allocator<P> > { };

    // A suitable map implementation that takes Particle* as a key (where Particles are considered equivalent based solely on their state), and T as a value
    // Note: this is necessary because not every Particle class may define a suitable hash function for their state
    // C++11: we might want to use a template <> using directive here
    template <class T, class P = Particle*>
    struct State_Map : std::unordered_map<P, T, ParticlePtrState_Hash<P>, ParticlePtrState_Equal<P>, pool_allocator<std::pair<const P, T> > > { };

    // typedefs for returning distributions (as ranges)

    template <class SubState, class Dummy = void>
    struct dist_range : distribution::single_estimate<SubState> {
        dist_range(const State &s, Weight weight) :
            distribution::single_estimate<SubState>(SubState(s), weight)
        { }
    };
    // specialisation for full ::State: simply return this State (= ::State) as is, do not construct
    // a temporary
    template <class Dummy>
    struct dist_range<::State<Model>, Dummy> : distribution::single_estimate<const ::State<Model>&> {
        dist_range(const State &s, Weight weight) :
            distribution::single_estimate<const ::State<Model>&>(s, weight)
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

#if ALLOW_AGENT_BLOCKING
    bool allBlocked; // if all agents considered so far (during prediction) are blocked, i.e. the agents could neither continue (pStop=1) nor expand (0 applicable actions)
#endif

#if (defined(HAS_CALLBACKS) || defined(USE_ACTION_OBSERVATION))
        // Depending on the observation model, pyx must be cached per particle, or can be saved per S-State (::State<Model>)
    Probability pyx;
#endif

public:

    State state;
    Weight weight;


public:

    // create a particle that represents a single starting-time from ::State
    Particle(::State<Model> state, Weight weight) :
#if DO_MULTI_AGENT && ALLOW_AGENT_BLOCKING
        allBlocked(false),
#endif
        state(std::move(state)),
        weight(weight)
    { }

    bool operator==(Particle const & other) const {
        if (state != other.state)
            return false;
        return true;
    }

    bool operator!=(Particle const & other) const { return !(*this == other); }

    // returns the combined p(y|x), i.e. combined for all actions and state observations
    Probability get_pyx() const;

    Weight getTotalWeight() const { return weight; }

    // set the weight for the total particle
    // This is used in resampling, where this particle gets some (constant) weight
    // The current strategy is to scale all starting times weights' such that the totalWeight is set
    // (alternatively, the starting times can get the same weight w/n, or we may even delete some starting times, but this only approximates the density)
    void setTotalWeight(Weight newWeight) { weight = newWeight; }

    size_t getStateCount() const { return 1; }

    // returns the single <S, A, T, I, G> state (aka X-State, the ::State instance) represented by this particle
    // (or any SubState constructable from the state)
    template <class SubState>
    dist_range<SubState> getDistribution() const { return dist_range<SubState>(state, getTotalWeight()); };

    // call all observation callbacks
    void doCallbacksAndObservations();

    // Predict all next states, computing their corresponding probabilities.
    // Observations are not applied.
    static StepResult step(typename Particle<Model>::ParticleList &particles, States &states);

    // Update the weight
    // Multiplies the weights of all starting times with the given probability
    // Basic assumption: Observation does not depend on the duration of the action
    // All zero weights are removed
    void updateWeight(Probability p) { weight *= p; }

    // Normalize the weight
    // Divides the weights of all starting times by the given probability
    // Basic assumption: Observation does not depend on the duration of the action
    // All zero weights are removed
    void normalizeWeight(Probability p) { weight /= p; }

    // Update step of particle filtering
    // as action observations have already been done during the step, this includes only the state observations
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
    // In either way, the particle will be multiplied with 1-pStop and either
    // * deleted, or
    // * added to the newParticles list
    static void step(Particle* particle, size_t agid, ParticleList& newParticles, ParticleSet<Particle*>& set, States &states);

}; // class Particle


template <class Model>
size_t hash_value(Particle<Model> const & p) {
    size_t hash = 0;
    boost::hash_combine(hash, p.state);
    return hash;
}





//      external explicit template instantiations
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


extern template class Particle<model::Model>;



} // namespace marginal::particle:: sequential
} // namespace marginal:: particle
} // namespace marginal
