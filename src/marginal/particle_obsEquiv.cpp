#include "../util/summation.h"

#include "../global.h"
#include "../obsmodel.h"

#include "particle_obsEquiv.h"


namespace marginal {
namespace particle {
namespace obsEquiv {



//      class State
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


namespace detail {

template <class Model>
bool State<Model>::operator==(const State &other) const {
    if (this->initialIndex != other.initialIndex)
        return false;
    if (this->goalIndex != other.goalIndex)
        return false;
    if (this->startTime != other.startTime)
        return false;
    if (this->states != other.states)
        return false;

    return true;
}


} // namespace marginal::particle::obsEquiv:: detail



//      class Particle
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



template <class Model>
void* Particle<Model>::operator new(size_t sz) {
    (void) sz; // is exactly sizeof(Particle) per standard
    pool_small_allocator<Particle> alloc;
    return alloc.allocate(1);
}


template <class Model>
void Particle<Model>::operator delete(void* ptr) {
    pool_small_allocator<Particle> alloc;
    alloc.deallocate(reinterpret_cast<Particle*>(ptr), 1);
}


// Add the particle to the list
// If a particle with identical state is already in the list, add the weight of this particle
// (because the states are identical, they represent the same set of X states)
// If a particle exists with overlapping set of states, separate the intersection to an additional particle
// In any case, the particle will be added to the list or will be deleted
template <class Model>
void Particle<Model>::addParticle(Particle* particle, ParticleList& particles, ParticleSet<Particle*>& set) {
    // Check if this particle state was already encountered
    auto set_it = set.find(particle);
    if (set_it != set.end()) {
        // this is a known particle → update the existing particle by adding the weights
        Particle &p = **set_it;
        p.single_weight += particle->single_weight;
        delete particle;
        return;
    }

    // this exact particle is not present
    // now search for any particle where the X states overlap

    // for every particle with overlapping states, store the set of overlapping states
    // we use an ordinary unordered_map (and not State_Map) because we sometimes change the Particle under the key,
    // - State_Map hashes according to the Particle, we want to hash here according to the Pointer itself
    std::unordered_map<Particle*, sa_set,
        std::hash<Particle*>, std::equal_to<Particle*>,
        pool_allocator<std::pair<Particle* const, sa_set>>> overlap_particles;

    for (auto sa_it = particle->state.states.begin(); sa_it != particle->state.states.end(); /* inc at end of loop */) {
        const SA &sa = *sa_it;
        ::State<Model> state(particle->state.initialIndex(), particle->state.goalIndex(), sa.modelState, sa.opid, particle->state.startTime);
        Particle *p = particles.getParticle(state);
        if (p == NULL) {
            // no other particle represents that state, continue
            ++sa_it;
            continue;
        }

        // p also represents that state → extract the state from both particles

        // get the overlap record for the current particle p
        sa_set &overlap_sa = overlap_particles[p];

        // add this SA state as an overlapping state
        overlap_sa.insert(sa);

        // remove sa from p
        // because we modify p, we have to remove it from the set (using the iterator) and re-insert it
        set.erase(p);
        p->state.states.erase(sa);
        set.insert(p);
        // also update the map State → Particle
        particles.removeState(p, state);

        // remove sa state from particle
        // do this at last, because sa is a reference to particle's sa_set
        sa_it = particle->state.states.erase(sa_it); // also update iterator for next iteration
    }



    // now, create new particles from overlapping states
    for (auto &overlap_it: overlap_particles) {
        // each state in overlap_it was represented by p and particle
        Particle *p = overlap_it.first;
        sa_set &overlap_sa = overlap_it.second;

        if (p->state.states.empty()) {
            // this particle has no states left, so we can re-use it
            set.erase(p);

            // the <I, G, T> state is the same, only the <S, A> states differ between the particles
            // (because the states overlapped and one particle represents only observation-equivalent states)
            using std::swap;
            swap(p->state.states, overlap_sa);
            // the new weight is the sum of both particle's weights which represented these states
            p->single_weight += particle->single_weight;

            set.insert(p);
            particles.addStates(p);
        } else {
            // create a new particle
            State newState(particle->state, overlap_sa);
            // the new weight is the sum of both particle's weights which represented these states
            Weight newWeight = p->single_weight + particle->single_weight;
            Particle *p_overlap = new Particle(std::move(newState), newWeight);

            particles.push_back(p_overlap);
            set.insert(p_overlap);
        }
    }

    if (particle->state.states.empty())
        // this particle has no states left, delete
        delete particle;
    else {
        // insert this state into the list and set
        particles.push_back(particle);
        set.insert(particle);
    }
}



template <class Model>
void Particle<Model>::step(Particle* particle, ParticleList& newParticles, ParticleSet<Particle*>& set, States &states) {

    // for every <S, A> state, expand all successor states
    // we cannot optimise this very much
    // every action can have a different pStop value
    // expanded states for every S-state are cached nontheless
    // therefore, we can simply iterate over all <S, A> states

    // we have to group all successor particles into groups of observation-equivalence
    // However, we only group those particles whith same weight
    // For example, let particle consist of three states {s1, s2, s3}
    // We have four successor particles with states s1', s2', s2', s3'
    // - we separate s2' from s1' and s3', because it has twice the weight (action could be applied in two states)
    // as a result, we have two obsEquiv particles {s1', s3'} and {s2'}

    // Because we are lazy, we transform each state into a single particle
    // and let the particle do the step
    typedef particle::single::Particle<Model> SingleParticle;

    // sequentially expand all SA states and create two maps:
    // * The first map associates particles to obsEquiv-classes according to the obsModel
    //   it is a map from SingleParticle → set<SingleParticle>, which maps the first occuring SingleParticle of every obsEquiv-class to all occuring Particles
    // * The second map records "asymmetries" in the particles that require us to split the obsEquiv-classes
    //   it is a map from SingleParticle → asym_t
    // then, all States (from SingleParticles) within the same obsEquiv-class and the same asym_t form one set of obsEquiv states which can be put into one obsEquiv Particle
    //

    // asym_t, a map from "weight" to "multiplicity"
    // - weight is the particle's weight (based on goal distance, action saliencies, number of applicable actions, …)
    // - multiplicity is how often this particle has been expanded
    typedef std::unordered_map<Weight, unsigned,
        boost::hash<Weight>, std::equal_to<Weight>,
        pool_allocator<std::pair<const Weight, unsigned>>> asym_t;

    // hashes by obsEquiv of SingleParticle: every entry is one equivalence class
    typedef std::unordered_map<const SingleParticle*, typename SingleParticle::template ParticleSet<>,
        detail::Particle_hash_obsEquiv<Model>, detail::Particle_equal_obsEquiv<Model>,
        pool_allocator<std::pair<const SingleParticle* const, typename SingleParticle::template ParticleSet<>>>> particle_class_t;
    // hashes by identity of SingleParticle: every SingleParticle gets an asym_t
    typedef typename SingleParticle::template State_Map<asym_t> particle_asym_map_t;

    particle_asym_map_t particle_asym_map;
    particle_class_t particle_class;

    Particle::State &state = particle->state;
    // list/set to collect the expanded particles
    typename SingleParticle::ParticleList spList;

    for (const SA &sa: state.states) {
        const ::State<Model> fullState(state.initialIndex(), state.goalIndex(), sa.modelState, sa.opid, state.startTime);
        // this single particle will be either deleted or put into the spList by the step method (and then deleted at the end of this method)
        SingleParticle *state_p = new SingleParticle(std::move(fullState), particle->single_weight);

        spList.clear();
        spList.push_back(state_p);
        SingleParticle::step(spList, states);

        // assign the SingleParticle to its obsEquiv-class and count the number of occurences of this particle
        // this is done inside the loop to avoid merging SingleParticles - otherwise we cannot check for asymmetries.
        for (SingleParticle *single_p: spList) {
            // increase expand count for this particle and the current particle weight value
            particle_asym_map[single_p][single_p->getTotalWeight()]++;

            // retrieve the first occuring SingleParticle of that obsEquiv-class
            auto p_class_it = particle_class.find(single_p);
            if (p_class_it == particle_class.end()) {
                // first particle of that obsEquiv-class
                auto &particle_set = particle_class[single_p];
                particle_set.insert(single_p);
            } else {
                // insert particle into that obsEquiv-class
                p_class_it->second.insert(single_p);
            }
        }
    }

    // now, for every obsEquiv-class, create the obsEquiv-Particles

    // for every asym_t for one set of obsEquiv states, map to one obsEquiv-Particle
    // for speed and memory savings, we store only the pointer to asym_t in the particle_asym_map map
    typedef std::unordered_map<const asym_t*, Particle*,
        Pointer_Hash<const asym_t*, hash_unordered<const asym_t>>, Pointer_Equal<const asym_t*>,
        pool_allocator<std::pair<const asym_t* const, Particle*>>> particle_map_t;
    particle_map_t particle_map;

    for (auto &particle_class_val: particle_class) {
        particle_map.clear();

        // for every "asymmetry" of states, create a new obsEquiv-Particle and add
        // all X-states to that particle
        for (SingleParticle *s_particle: particle_class_val.second) {
            const asym_t &asym = particle_asym_map[s_particle];
            Particle* &pNew = particle_map[&asym];
            if (pNew == NULL) {
                // create a new particle
                // a single state's weight is the sum of the particles weights that led to this state
                Sum<Weight> w;
                for (auto &weight_it: asym) {
                    // multiply weight by number of occurences
                    w += weight_it.first * Weight(weight_it.second);
                }
                pNew = new Particle(s_particle->state, w.sum());
            } else {
                // a particle with the same "asymmetry" exists, thus the weight has already been computed
                // we only need to add a new SA state
                // (all states added have the same weight,
                // the total weight is automatically multiplied by the number of obsEquiv particles)
                pNew->state.states.insert(SA(s_particle->state.modelState, s_particle->state.opid));
            }

            // no longer need this particle, its state has been added
            delete s_particle;
        }

        // add every obsEquiv-particle to the result list
        for (auto &particle_val: particle_map) {
            Particle *p = particle_val.second;
            addParticle(p, newParticles, set);
        }
    }

    // we no longer need this actual particle, because any continuations are already present in the single particles
    delete particle;
}



template <class Model>
void Particle<Model>::doCallbacksAndObservations() {
    // because the states are observation-equivalent, we can pick one arbitrarily
    const SA &sa = *state.states.begin();
    model::doCallbacksAndObservations(sa.opid, sa.modelState->first);
}



template <class Model>
void Particle<Model>::update() {
    // because the states are observation-equivalent, we can pick one arbitrarily
    StateTablePtr modelState = state.states.begin()->modelState;

#if !(defined(HAS_CALLBACKS) || defined(USE_ACTION_OBSERVATION))
    StateDataRec &gpt = modelState->second;
    // We can base the observation only on the state.
    // This allows us to cache the result.
    //
    // If action callbacks are enabled, instead, we have to make observation every time, as
    // action callbacks depend not only on the state, but also the
    // currently executed action.
    if (gpt.time == Global::time) {
        // pyx has already been computed
        updateWeight(gpt.pyx);
        return;
    }
#endif // HAS_CALLBACKS

    // call pre_observe_*(), then do observations, then call post_observe_*() to update the weight
    for (size_t i = 0; i < Model::n_agents; i++)
        model::pre_observe_action(i);
    const model::StateRec &s = modelState->first;
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



template <class Model>
Probability Particle<Model>::get_pyx() const {
#if (defined(HAS_CALLBACKS) || defined(USE_ACTION_OBSERVATION))
    return pyx;
#else
    // because the states are observation-equivalent, we can pick one arbitrarily
    StateTablePtr modelState = state.states.begin()->modelState;
    StateDataRec &gpt = modelState->second;
    if (gpt.time != Global::time) {
        std::cerr << "Particle has old observation (" << gpt.time << " / " <<  Global::time << ")" << std::endl;
        throw std::runtime_error("Cannot use old pyx.");
    }
    return gpt.pyx;
#endif
}



template <class Model>
void Particle<Model>::set_pyx(Probability _pyx) {
#if (defined(HAS_CALLBACKS) || defined(USE_ACTION_OBSERVATION))
    pyx = _pyx;
#else
    // consistently set pyx for all states
    for (const auto &sa: state.states) {
        StateDataRec &gpt = sa.modelState->second;
        gpt.time = Global::time;
        gpt.pyx = _pyx;
    }
#endif
}



// free function for the prediction

template <class Model>
StepResult Particle<Model>::step(ParticleList &particles, States &states) {
    StepResult stepResult;

    ParticleSet<Particle*> particleSet;
    ParticleList newParticles; // successor particles


    for (Particle *p : particles) {
        Probability oldWeight = p->getTotalWeight();
        size_t count_new = newParticles.size();

        // create successor particles and compute pStop for all particles
        Particle::step(p, newParticles, particleSet, states);

        if (count_new == newParticles.size()) {
            // no successor particles and particle does not continue action
            // this particles is dead
            stepResult.numDead++;
            stepResult.weightDead += oldWeight;
        }
    }

    // continue with the new particles
    particles.swap(newParticles);

    return stepResult;
}



//      static fields
// #############################################################################


template <class Model>
typename Particle<Model>::Options Particle<Model>::options;


//      explicit template instantiations
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


template struct detail::State<model::Model>;
template class Particle<model::Model>;


} // namespace marginal::particle:: obsEquiv
} // namespace marginal:: particle
} // namespace marginal
