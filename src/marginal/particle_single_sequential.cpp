#include <boost/functional/hash.hpp>

#include "../util/summation.h"

#include "../global.h"
#include "../obsmodel.h"

#include "particle_single_sequential.h"


namespace marginal {
namespace particle {
namespace sequential {


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



// If the particle is not already in the set, create a new particle and add it to the list;
// otherwise merge the particles and add the weights to the existing particle - the particle is deleted
template <class Model>
void Particle<Model>::addParticle(Particle* particle, ParticleList& particles, ParticleSet<Particle*>& set) {
    // Check if this particle state was already encountered
    typename ParticleSet<Particle*>::const_iterator setIt = set.find(particle);
    if (setIt == set.end()) {
        // this is a new state → add it to the set and list
        set.insert(particle);
        particles.push_back(particle);
        return;
    }

    // this is a known state → update the existing particle by merging the weights
    Particle &p = **setIt;
    p.weight += particle->weight;

    delete particle;
}


template <class Model>
void Particle<Model>::step(Particle* particle, size_t agid, ParticleList& newParticles, ParticleSet<Particle*>& set, States &states) {
    expandState(states, agid, particle->state.modelState);

    StateDataRec &d = particle->state.modelState->second;
    // compute and normalise action probability
    Weight saliencies[NOPS];
    Sum<Weight> sumSaliency = ZeroWeight;

#ifdef NORMALIZE_DISTANCE
    Weight max = -infinity;
    for (int i = 0; i < d.appl[agid]; ++i) {
        Weight sal = calcWeight(*particle, i, agid);
        max = std::max(sal, max);
    }

    for (int i = 0; i < d.appl[agid]; ++i) {
        Weight sal = calcWeight(*particle, i, agid);
        saliencies[i] = exp(sal-max);
        sumSaliency += saliencies[i];
    }

#else
    for (int i = 0; i < d.appl[agid]; ++i) {
        saliencies[i] = calcWeight(*particle, i, agid);
        sumSaliency += saliencies[i];
    }
#endif

#if ALLOW_AGENT_BLOCKING
    // set to false if we could expand at least one state
    bool agentBlocked = true;
#endif

    // compute pStop
    Probability pStop;
    {
        double pStop_;
        (model::actions[particle->state.opid[agid]]->durationModel)(particle->state.startTime[agid], &pStop_);
        pStop = pStop_;
    }

    // for each reaching state, create a new particle
    for (int i = 0; sumSaliency.sum() > ZeroWeight && i < d.appl[agid]; ++i) {
        // compute the weight based on the action saliency
        Weight weight = particle->weight * saliencies[i] / sumSaliency.sum() * pStop;
        if (weight == ZeroWeight)
            continue;

#if ALLOW_AGENT_BLOCKING
        agentBlocked = false;
#endif
#if ENDSTAT_REACH_STATES
        d.appi[agid][i].reached->second.visited = true;
#endif

        State newState(particle->state); // copy state
        newState.opid[agid] = d.appi[agid][i].opid;
        newState.startTime[agid] = Global::time;
        newState.modelState = d.appi[agid][i].reached;

        // create new particle
        Particle* newParticle = new Particle(std::move(newState), weight);
        addParticle(newParticle, newParticles, set);
    }

#if ALLOW_AGENT_BLOCKING
    Weight oldWeight;
    if (!agentBlocked)
        particle->allBlocked = false;
    else
        // this particle may become blocked, so save the current weight
        oldWeight = particle->weight;
#endif

    // for the current particle, update the weights with 1-pStop
    particle->weight *= Probability(1) - pStop;

    if (particle->weight != ZeroWeight) {
        // this very particle stays executing the current action
#if ALLOW_AGENT_BLOCKING
        particle->allBlocked = false;
#endif
        addParticle(particle, newParticles, set);
    } else {
#if !ALLOW_AGENT_BLOCKING
        delete particle;
#else
        // this agent does not continue its current action
        // usually, we simply throw this particle away
        // However, if the agent is blocked, we keep it (some other agent may not be blocked)
        if (agentBlocked && (!(particle->allBlocked) || agid < Model::n_agents - 1)) {
            // this agent is blocked, but other agents are (or may be) not blocked
            if (agid == 0)
                particle->allBlocked = true;
            // set opid
            particle->state.opid[agid] = model::NoOpId;
            particle->state.startTime[agid] = Global::time;
            // restore the old weight: "blocking" is allowed and should not result in ZeroWeight
            particle->weight = oldWeight;

            addParticle(particle, newParticles, set);
        } else
            // this particle has been expanded or all agents are blocked → finally throw particle away
            delete particle;
#endif // ALLOW_AGENT_BLOCKING
    }
}


template <class Model>
void Particle<Model>::doCallbacksAndObservations() {
    model::do_action_callback(state.opid, state.modelState->first);
    model::do_state_callback(state.modelState->first);
}



template <class Model>
void Particle<Model>::update() {
#if !(defined(HAS_CALLBACKS) || defined(USE_ACTION_OBSERVATION))
    StateDataRec &gpt = state.modelState->second;
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

    // because we already observed the actions in the prediction, we only need to observe the state here
    // call pre_observe_state(), then do observations, then call post_observe_state() to update the weight
    const model::StateRec &s = state.modelState->first;
    model::pre_observe_state(s);

    model::do_state_callback(s);

    // let the model compute pyx (must be independent of action duration!)
    Probability pyx = model::post_observe_state(s);

#if (defined(HAS_CALLBACKS) || defined(USE_ACTION_OBSERVATION))
    set_pyx(get_pyx() * pyx); // update total pyx, has been set for action observations
#else
    set_pyx(pyx); // set total pyx
#endif
    updateWeight(pyx);
}



template <class Model>
Probability Particle<Model>::get_pyx() const {
#if (defined(HAS_CALLBACKS) || defined(USE_ACTION_OBSERVATION))
    return pyx;
#else
    StateDataRec &gpt = state.modelState->second;
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
    StateDataRec &gpt = state.modelState->second;
    gpt.time = Global::time;
    gpt.pyx = _pyx;
#endif
}



// calcWeight is static
template <class Model>
double Particle<Model>::calcWeight(const Particle& particle, int i, int agid) {
    StateDataRec &d = particle.state.modelState->second;
    int curOpid = particle.state.opid[agid];
    ApplInfo ap = d.appi[agid][i];

    model::StatePtr s = &particle.state.modelState->first;
    (void)s;

    // first check if executing the action would be an illegal repition
    if (!model::canFollowAfter(ap.opid, curOpid))
        return 0;

#if defined(SUPPORTS_PYA) && defined(USE_PYA) && defined(HAS_CALLBACKS)
    // if supported, check if current observation are compatible with the action
    // if ZeroProbability is returned, set weight to 0, otherwise leave it as-is
    // first run callback, so the observation model has information about the current action
    model::actions[ap.opid]->callback(s);
    Probability pya = getActionProbability(s);
    if (pya == ZeroProbability)
        return 0;
#endif

    double sal = ap.weight[particle.state.goalIndex()];
    sal = pow(sal, 1/parameters.nroot);

#ifdef NORMALIZE_DISTANCE
    sal = sal+parameters.weightBias;
#else
    sal = exp(sal+parameters.weightBias);
#endif
    return sal;
}


template <class Model>
StepResult Particle<Model>::step(ParticleList &particles, States &states) {
    // this is the core function that differs from single::Particle
    // for every agent, expand state, do action observation, prune
    // this pruning after every agent avoid exponential blow-up in the number of expanded actions

    StepResult stepResult;

    ParticleList newParticles; // successor particles

    for (size_t agid = 0; agid < Model::n_agents; agid++) {
        ParticleSet<Particle*> particleSet;

        // predict agent's actions
        for (Particle *p : particles) {
            Probability oldWeight = p->getTotalWeight();
            size_t count_new = newParticles.size();

            // create successor particles and compute pStop for all particles
            Particle::step(p, agid, newParticles, particleSet, states);

            if (count_new == newParticles.size()) {
                // no successor particles and particle does not continue action
                // this particles is dead
                stepResult.numDead++;
                stepResult.weightDead += oldWeight;
            }

        }

        // observe this agent's action
        // need to observe after we have expanded all particles, because weight of a particle might change (particles can be merged)
#if (defined(HAS_CALLBACKS) || defined(USE_ACTION_OBSERVATION))
        // only need to observe actions if we have action observations
        for (Particle *p: newParticles) {
            model::pre_observe_action(agid);
            model::do_action_callback(p->state.opid, p->state.modelState->first);
            Probability pyx = model::post_observe_action(agid);

            // update pyx
            if (agid == 0)
                p->set_pyx(pyx);
            else
                p->set_pyx(p->get_pyx() * pyx);
            p->updateWeight(pyx);
        }
#endif

        // prune, except for last agent
        if (agid < Model::n_agents - 1) {
            size_t limit = Global::nparticles;
            if (options.prune_agent_increasing)
                limit = Global::nparticles * (agid+2);
            // for every executing agent, allow an extra set of particles
            options.prune->prune(newParticles, limit);
        }

        // continue with the new particles
        particles.clear(); // the step method deletes old, unused particles
        particles.swap(newParticles);

    }

    return stepResult;
}



//      static fields
// #############################################################################


template <class Model>
typename Particle<Model>::Options Particle<Model>::options;



//      explicit template instantiations
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


template class Particle<model::Model>;


} // namespace marginal::particle:: sequential
} // namespace marginal:: particle
} // namespace marginal
