#include <boost/functional/hash.hpp>

#include "../util/summation.h"

#include "../global.h"
#include "../obsmodel.h"

#include "particle_single.h"





namespace marginal {
namespace particle {
namespace single {


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



#if DO_MULTI_AGENT
template <class Model>
void Particle<Model>::step(Particle* particle, ParticleList& newParticles, ParticleSet<Particle*>& particleSet, States &states) {
    using std::swap;

    // Loop over all agents and expand the states

    // For the agents sequentially expand the states
    // That is start with the single particle, expand states for the first agent
    // take all the expanded states and expand them all for the second agent, and so on

    // In all but the last iteration over the agents, read particles from particlesFrom and write them to particlesTo
    // Then, swap both.
    // Before the last iteration, make particlesTo point to newParticles, to add the final
    // particles to the resulting list
    ParticleList particlesFrom_;
    ParticleList particlesTo_;
    ParticleList* particlesFrom = &particlesFrom_;
    ParticleList* particlesTo = &particlesTo_;

    particlesTo->push_back(particle); // from and to will be swapped at begin of loop over the agents

    // temporary particle set for merging "unfinished" particles.
    // only if the last agent has been executed, particles should be merged with the global particleSet
    // (particle from previous loop iterations of agents must not be merged)
    ParticleSet<Particle*> tempSet;
    ParticleSet<Particle*>* set = &tempSet;

    for (size_t agid = 0; agid < Model::n_agents; ++agid) {
        // invariants:
        // particlesTo contains particles expanded from the last iteration
        // particlesFrom is empty

        if (agid < Model::n_agents - 1) {
            // swap from and to
            swap(particlesFrom, particlesTo);
            set->clear();
        } else {
            // last iteration: directly write to result list
            particlesFrom = particlesTo;
            particlesTo = &newParticles;
            set = &particleSet;
        }

        while (!particlesFrom->empty()) {
            // for every state, compute all possible successor states for the current agent
            Particle* p = particlesFrom->back();
            particlesFrom->pop_back();

            expandState(states, agid, p->state.modelState);
            StateDataRec &d = p->state.modelState->second;

            // compute and normalise action probability
            Weight saliencies[NOPS];
            Sum<Weight> sumSaliency = ZeroWeight;

#ifdef NORMALIZE_DISTANCE
            Weight max = -infinity;
            for (int i = 0; i < d.appl[agid]; ++i) {
                Weight sal = calcWeight(*p, i, agid);
                max = std::max(max,sal);
            }
            for (int i = 0; i < d.appl[agid]; ++i) {
                Weight sal = calcWeight(*p, i, agid);
                saliencies[i] = exp(sal-max);
                sumSaliency += saliencies[i];
            }
#else
            for (int i = 0; i < d.appl[agid]; ++i) {
                saliencies[i] = calcWeight(*p, i, agid);
                sumSaliency += saliencies[i];
            }
#endif

            bool deadEnd = prob(sumSaliency) == 0;

#if ALLOW_AGENT_BLOCKING
            // set to false if we could expand at least one state
            bool agentBlocked = true;
#endif

            // compute pStop for this particle/agent
            Probability pStop;
            {
                double pStop_;
                (model::actions[p->state.opid[agid]]->durationModel)(p->state.startTime[agid], &pStop_);
                pStop = pStop_;
            }

            if (!deadEnd) {
                // for each reaching state, create a new particle
                for (int i = 0; i < d.appl[agid]; ++i) {
                    // compute the weight based on the action saliency
                    Weight newWeight = p->weight * saliencies[i] / sumSaliency.sum() * pStop;
                    if (newWeight == ZeroWeight)
                        continue;

#if ALLOW_AGENT_BLOCKING
                    agentBlocked = false;
#endif
#if ENDSTAT_REACH_STATES
                    d.appi[agid][i].reached->second.visited = true;
#endif

                    State newState(p->state); // copy previous state
                    newState.opid[agid] = d.appi[agid][i].opid; // set the new opid
                    newState.startTime[agid] = Global::time;
                    newState.modelState = d.appi[agid][i].reached;

                    // create a new particle
                    Particle* newParticle = new Particle(std::move(newState), newWeight);

                    addParticle(newParticle, *particlesTo, *set);
                } // loop over all applicable actions
            } // if there are applicable actions


#if ALLOW_AGENT_BLOCKING
            Weight oldWeight;
            if (!agentBlocked)
                p->allBlocked = false;
            else
                // this particle may become blocked, so save the current weight
                oldWeight = p->weight;
#endif

            // update the current particle by 1-pStop
            p->weight *= Probability(1) - pStop;

            if (p->weight != ZeroWeight) {
                // this very particle/agent stays executing the current action
#if ALLOW_AGENT_BLOCKING
                p->allBlocked = false;
#endif
                addParticle(p, *particlesTo, *set);
            } else {
#if !ALLOW_AGENT_BLOCKING
                // this agent does not continue its current action
                delete p;
#else
                // this agent does not continue its current action
                // usually, we simply throw this particle away
                // However, if the agent is blocked, we keep it (some other agent may not be blocked)
                if (agentBlocked && (!(p->allBlocked) || agid < Model::n_agents - 1)) {
                    // this agent is blocked, and other agents are (or may be) not blocked
                    if (agid == 0)
                        p->allBlocked = true;
                    // set opid
                    p->state.opid[agid] = model::NoOpId;
                    p->state.startTime[agid] = Global::time;
                    // restore the old weight: "blocking" is allowed and should not result in ZeroWeight
                    p->weight = oldWeight;

                    addParticle(p, *particlesTo, *set);
                } else
                    // this particle has been expanded or all agents are blocked → finally throw particle away
                    delete p;
#endif // ALLOW_AGENT_BLOCKING
            } // if particle does not continue
        } // loop over all particles
    } // loop over all agents
}


#else // DO_MULTI_AGENT


template <class Model>
void Particle<Model>::step(Particle* particle, ParticleList& newParticles, ParticleSet<Particle*>& set, States &states) {
    expandState(states, 0, particle->state.modelState);

    StateDataRec &d = particle->state.modelState->second;
    // compute and normalise action probability
    Weight saliencies[NOPS];
    Sum<Weight> sumSaliency = ZeroWeight;

#ifdef NORMALIZE_DISTANCE
    Weight max = -infinity;
    for (int i = 0; i < d.appl[0]; ++i) {
        Weight sal = calcWeight(*particle, i, 0);
        max = std::max(sal, max);
    }

    for (int i = 0; i < d.appl[0]; ++i) {
        Weight sal = calcWeight(*particle, i, 0);
        saliencies[i] = exp(sal-max);
        sumSaliency += saliencies[i];
    }

#else
    for (int i = 0; i < d.appl[0]; ++i) {
        saliencies[i] = calcWeight(*particle, i, 0);
        sumSaliency += saliencies[i];
    }
#endif

    // compute pStop
    Probability pStop;
    {
        double pStop_;
        (model::actions[particle->state.opid[0]]->durationModel)(particle->state.startTime[0], &pStop_);
        pStop = pStop_;
    }

    // for each reaching state, create a new particle
    for (int i = 0; sumSaliency.sum() > ZeroWeight && i < d.appl[0]; ++i) {
        // compute the weight based on the action saliency
        Weight weight = particle->weight * saliencies[i] / sumSaliency.sum() * pStop;
        if (weight == ZeroWeight)
            continue;

#if ENDSTAT_REACH_STATES
        d.appi[0][i].reached->second.visited = true;
#endif

        State newState(particle->state); // copy state
        newState.opid[0] = d.appi[0][i].opid;
        newState.startTime[0] = Global::time;
        newState.modelState = d.appi[0][i].reached;

        // create new particle
        Particle* newParticle = new Particle(std::move(newState), weight);
        addParticle(newParticle, newParticles, set);
    }

    // for the current particle, update the weights with 1-pStop
    particle->weight *= Probability(1) - pStop;

    if (particle->weight != ZeroWeight)
        // this very particle stays executing the current action for at least one starting time
        addParticle(particle, newParticles, set);
    else
        delete particle;
}
#endif // DO_MULTI_AGENT



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


template class Particle<model::Model>;


} // namespace marginal::particle:: single
} // namespace marginal:: particle
} // namespace marginal
