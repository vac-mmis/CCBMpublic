#pragma once

// this file implements smoothing for marginal filter

#include <stack>
#include <sstream>
#include <memory>


// TODO this should be general enough to also work with the particle filter particle
// if this is the case, remove the marginal:: namespace scope
namespace marginal {
namespace smoothing {

template<class Particle>
class Smoothing {
private:
    typedef typename Particle::template ParticleSet<> ParticleSet;
    typedef typename Particle::ParticleList ParticleList;

    // we need each particle twice: once the weight after pure prediction, and once after the update step (filtered)
    // This is a record of that time-step during filtering and contains *copies* of the particles given as argument to addPredicted / addFiltered
    struct particles_record {
        std::shared_ptr<ParticleSet> predicted;
        std::shared_ptr<ParticleSet> filtered;

        particles_record() :
            predicted(std::make_shared<ParticleSet>()),
            filtered(std::make_shared<ParticleSet>())
        {
        }
    };

    // for every time-step, the predicted and filtered particles
    // this is a stack because smoothing works backwards in time
    std::stack<particles_record> records;

    // while incrementelly adding particles via addPredicted and addFiltered,
    // not all particles that are predicted get finally filtered (e.g. due to pruning)
    // therefore, we first collect all predicted particles, and add only these that are also filtered to the records
    ParticleSet predictedSet;

public:
    ~Smoothing();

    // start a new time-step
    // this has to be called before addPredicted/addFiltered
    void advanceTime();

    // Adds a particle after the predict step
    // This method creates an internal copy of the particle
    void addPredicted(const Particle * _p);

    // Removes all predicted particles from the current time slice
    // Decreases number of records by one
    void removePredicted();

    // Adds a particle after the update step
    // This particle has to be added before with addPredicted
    // This method creates an internal copy of the particle
    void addFiltered(Particle *p_Filtered);

    // runs the smoothing algorithm and returns a smoothed estimate
    // The returned stack contains, starting from the first time-step, the set of all particles with smoothed weights
    // The caller is responsible for deleting the Particle* inside the ParticleSets
    std::shared_ptr< std::stack<ParticleSet> > doSmoothing(States &states);
}; // class Smoothing


template<class Particle>
Smoothing<Particle>::~Smoothing() {
    // Delete all particle copies that may be still in the maps (esp. when doSmoothing has not been called)
    for (Particle *p: predictedSet) {
        delete p;
    }
    predictedSet.clear();

    while (!records.empty()) {
        particles_record &record = records.top();
        for (Particle *p: *record.predicted) {
            delete p;
        }
        for (Particle *p: *record.filtered) {
            delete p;
        }
        records.pop();
    }
}

template<class Particle>
void Smoothing<Particle>::advanceTime() {
    // First, delete all predicted particles that are not added via addFiltered (e.g. due to pruning)
    for (auto p: predictedSet) {
        delete p;
    }
    predictedSet.clear();

    // start a new time and use fresh predicted/filtered sets
    records.push(particles_record());
}

template<class Particle>
void Smoothing<Particle>::addPredicted(const Particle * _p) {
    // copy the particle, because the particles of the filter are updated
    // this copy is free'd either in advanceTime (when not called with addFiltered) or at the end of the smoothing
    Particle* p = new Particle(*_p);

    std::pair<typename ParticleSet::iterator, bool> result = predictedSet.insert(p);
    if (!result.second) {
        throw std::logic_error("addPredicted: Duplicate particle inserted");
    }
}

template<class Particle>
void Smoothing<Particle>::removePredicted() {
    particles_record &record = records.top();
    for (Particle *p: *record.predicted) {
        delete p;
    }
    for (Particle *p: *record.filtered) {
        delete p;
    }
    records.pop();
}

template<class Particle>
void Smoothing<Particle>::addFiltered(Particle *p_Filtered) {
    // get the previously added predicted particle
    // the particles only differ in the starting time weights, the rest of the state is identical
    // therefore, we can use the filtered particle to find the predicted
    typename ParticleSet::iterator itPred = predictedSet.find(p_Filtered);
    if (itPred == predictedSet.end()) {
        throw std::logic_error("addFiltered: This state was never predicted");
    }

    Particle* p_Pred = *itPred;
    // remove this element from the temporary predictedSet
    predictedSet.erase(itPred);

    // Add both particles to the current record
    particles_record &record = records.top();
    record.predicted->insert(p_Pred); // this was already a copy
    std::pair<typename ParticleSet::iterator, bool> result = record.filtered->insert(new Particle(*p_Filtered)); // create a copy

    if (!result.second) {
        throw std::logic_error("addFiltered: Duplicate particle inserted");
    }
}

template<class Particle>
std::shared_ptr< std::stack<typename Smoothing<Particle>::ParticleSet> > Smoothing<Particle>::doSmoothing(States &states) {
    // First, delete all predicted particles that are not added via addFiltered (e.g. due to pruning)
    for (Particle *p: predictedSet) {
        delete p;
    }
    predictedSet.clear();

    // simple sanity check: are there as many records as time-steps?
    // we require that predicted/filtered particles have been added for all time-steps
    if (records.size() != Global::timeSteps.size()) {
        std::ostringstream msg;
        msg << "doSmoothing: different number of time steps and smoothing records";
        msg << ": " << Global::timeSteps.size() << " / " << records.size();
        throw std::logic_error(msg.str());
    }

    // for each time-step, the set of smoothed particles
    // we need to look up particles, therefore this is a set instead of a simple ParticleList
    std::shared_ptr<std::stack<ParticleSet> > smoothed = std::make_shared<std::stack<ParticleSet> >();

    // the predicted/filtered particles at time t
    particles_record currentRecord = records.top();
    records.pop();

    Global::time = Global::timeSteps.back();
    std::cerr << "Start Smoothing at time " << Global::time << '\n';

    // The basic smoothing equation:
    // Let x^(i)_t be one particle at time t and weights w^(i)_t
    // That is, w^(i)_t = p(x^(i)_t | y_1:t) is the weight at time t after filtering
    // Goal is to compute the smoothing density/weight ws^(i)_t = p(x^(i)_t | y_1:T) (where T is the last time-step)
    // Computing ws^(i)_t is done recursively over time t for each particle as follows.
    // By definition, the initialisation (start of iteration) is
    // ws^(i)_T = p(x^(i)_T | y_1:T) = w^(i)_T
    // Then, ws^(i)_t can be iteratively computed as follows:
    // ws^(i)_t = w^(i)_t * SUM(j over all successor particles x^(j)_t+1)
    //                  p(x^(j)_t+1 | x^(i)_t) * ws^(j)_t+1 / p(x^(j)_t+1 | y_1:t)
    // That is: smoothed(i,t) = filtered(i,t) * transition(j,t+1) * smoothed(j,t+1) / predicted(j,t+1) FOR ALL successors j

    // Some implementations of a particle may actually represent multiple ::States
    // This smoothing algorithm operates only at the particle level - e.g. a smoothing weight of 1/2 of the filtered weight
    // propagates to all basic ::States. They are not distinguished.
    // This makes implementation a lot easier, and if the user selected an implementation that represents multiple states,
    // then we can only operate on this "coarse" particle
    // Similar restrictions (may) also apply to estimates in the state, or the Viterbi algorithm
    // If a detailed smoothing is wanted
    // * we may implement another smoothing algoirthm which operates on every single ::State,
    //  but this requires a lot of changes to the API of the particles (one must access and re-weight states, this may not even always be possible, e.g. if the particle represents infinitely many states)
    // * the user can change to a particle that represents exactly one ::State


    {
        // first get smoothing density from last step (p_filtered(T))
        // (prediction at last time step T = smoothing at T)
        smoothed->push(ParticleSet());
        ParticleSet& curSmoothed = smoothed->top();
        curSmoothed.swap(*currentRecord.filtered);

        // print the sum, it should always be 1 (has already been normalised)
        Sum<Weight> sum = ZeroProbability;
        for (Particle *particle: curSmoothed) {
            sum += particle->getTotalWeight();
        }
        std::cerr << Global::time <<":sum=" << sum.sum() << '\n';
    }

    // Therefore the algorithm is
    //  for every filtered particle x^(i)_t at time t
    //      smoothed_weight = 0
    //      for every successor state x^(j)_t+1 of x^(i)_t
    //          smoothed_weight += transition(j,t+1) * smoothed(j,t+1) / predicted(j,t+1)
    //      smoothed_weight *= filtered(i,t) // this is the scalar value w^(i)_t
    // Note that one x^(i)_t can lead to multiple x^(j)_t+1, x^(j)_t+1 - that is the *same state* but different starting times (in multi-agent models where different agents executing the same action leads to the same state)

    // loop over all time-steps backwards
    size_t timeStep = Global::timeSteps.size();
    while (!records.empty()) {
        --timeStep;
        Global::time = Global::timeSteps[timeStep];

        // smoothing of time t+1 ("last" because smoothing is backwards)
        ParticleSet& lastSmoothed = smoothed->top();
        // the predicted particle of the next time step t+1 ("next" because filtering is forward in time)
        std::shared_ptr<ParticleSet> nextPredicted = currentRecord.predicted;
        // get the predicted/filtered particles of the current time t
        currentRecord = records.top();
        records.pop();
        std::shared_ptr<ParticleSet> curFiltered = currentRecord.filtered;

        // create new set of particles for the current smoothing weights
        smoothed->push(ParticleSet());
        // smoothing of time t (now empty and will be filled in the next loop)
        ParticleSet& curSmoothed = smoothed->top();

        // we are currently at time t
        // however, Global::time stays t+1 so pStop is computed correctly for the transition probabilities
        std::cerr << Global::timeSteps[timeStep-1] << ":";

        // loop over every filtered particle filtered(i,t) according to the equation above
        for (Particle *p_Filtered: *curFiltered) {
            // For the current particle, we sum smoothed(i,t) into this weight
            // After looping over all successor particles, the weight is multiplied with the filtered weight of the current particle
            // (The filtered weight is a constant factor within the sum in the smoothing equation for this particle)
            Sum<Weight> smoothed_weight;

            // The particle p_Filtered (at time t) has successor states at time t+1 -- these are in lastSmoothed
            // due to pruning, not all successor states must be actually in lastSmoothed
            // Therefore, for each action applicable in p_Filtered, check if the resulting state is in lastSmoothed

            // for the current particle compute all successor particles and the corresponding transition probability
            // To compute the transition probability, we set its weight to 1, then we call Particle::step to get the successor particles.
            // Their weights are the transition probabilities

            // create a copy of the filtered particle, this is used for the actual step method
            // This particle must not be freed explicitly (the step method below puts it into successorParticles or deletes it)
            Particle *tempParticle = new Particle(*p_Filtered);
            tempParticle->setTotalWeight(1);

            // create successor particles
            ParticleList successorParticles {tempParticle};
            Particle::step(successorParticles, states);

            for (Particle* p_Succ: successorParticles) {
                // check if state is in next slice at t+1
                typename ParticleSet::iterator nextPredIt = nextPredicted->find(p_Succ);
                if (nextPredIt == nextPredicted->end()) {
                    delete p_Succ;
                    // this successor particle has been removed by pruning (or got zero weight during update)
                    // although this is the *predicted* set, this set contains only those that are actually also in the *filtered* set
                    continue;
                }

                typename ParticleSet::iterator lastSmoothedIt = lastSmoothed.find(p_Succ);
                if (lastSmoothedIt == lastSmoothed.end()) {
                    delete p_Succ;
                    // this successor particle got zero smoothing weight
                    // the only case when this happens is when the successor has positive filtering weight,
                    // but this successor's successors have all been removed by pruning in the next time-step
                    continue;
                }

                // transition, prediction and smoothing weights for p_Succ
                Weight pTrans = p_Succ->getTotalWeight();
                Weight pPred = (*nextPredIt)->getTotalWeight();
                Weight pSmo_last = (*lastSmoothedIt)->getTotalWeight();

                // resulting weight = filtered * transition * smoothed() / predicted;
                // pFiltered is a common factor multiplied at the end of the loop
                smoothed_weight += pTrans * pSmo_last / pPred;

                delete p_Succ;
            } // for every succesor particle

            if (smoothed_weight > ZeroWeight) {
                // if it would be empty, the current particle would have no successor particles in the next time-step
                // therefore, it would not contribute to the smoothing density

                // Update p_Filtered to p_Smoothed
                // We do not need p_Filtered any more and can simply update the weight to the new smoothed weight
                p_Filtered->setTotalWeight(smoothed_weight * p_Filtered->getTotalWeight());
                curSmoothed.insert(p_Filtered);
            } else
                // smoothed_weight is zero
                // We do not need p_Filtered any more
                delete p_Filtered;
        } // for every filtered particle at time t

        // we also no longer need the predicted particles at t+1
        for (Particle *p: *nextPredicted) {
            delete p;
        }

        // print sum of weights
        // should mostly be 1? If less than one, this should be the weight that got lost during pruning
        Sum<Weight> sumw = ZeroProbability;
        for (Particle* p: curSmoothed) {
            sumw += p->getTotalWeight();
        }
        std::cerr << "sum=" << sumw.sum() << '\n';

        // normalise weights
        for (Particle* p: curSmoothed) {
            p->normalizeWeight(sumw.sum());
        }
    } // while (!states.empty) - end of actual smoothing loop over all time steps

    // we do not need the predicted particles of the first time-step
    for (Particle *p: *currentRecord.predicted) {
        delete p;
    }

    return smoothed;
}

} // namespace marginal::smoothing
} // namespace marginal
