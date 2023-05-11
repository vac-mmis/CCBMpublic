#pragma once

#include <cassert>
#include <iostream>
#include <sstream>
#include <stack>
#include <deque>
#include <string>
#include <memory>

#include "../util/hash.h"
#include "../util/shared_list.h"
#include "../util/iterators.h"
#include "../util/distribution.h"

#include "../estimate/action_estimator.h"

#include "../global.h"
#include "../weight.h"


namespace marginal {
namespace viterbi {

/**
 * Path information for the viterbi run (visited states + total weight of the path)
 */
template<class Particle>
struct ViterbiPath {
    // The list of particles, starting from an initial state
    // SharedList: use the same list nodes for shared paths when copying the list
    // Equally, use a shared_ptr of Particles, because we do not know when one Particle can be free'd or not
    util::SharedList<std::shared_ptr<Particle> > states;

    // The probability of the path, i.e. all weights along the path multiplied
    // As this is a multiplication of weights, the path probability should always be in the log-domain
    LogProbability pPath;

    bool operator<(const ViterbiPath& other) const {
        return pPath < other.pPath;
    }
};

template<class Particle>
class Viterbi {
public:
    typedef typename Particle::ParticleList ParticleList;
    typedef typename Particle::template ParticleSet<> ParticleSet;

    // Map from filtered particle → p(y|x) (get_pyx may not be cached and depend on the current state, therefore we have to store it separately)
    // Because we expand all particles during the Viterbi run, we need all particles to check which particles have actually been filtered and which have been pruned
    // otherwise, we would run Viterbi on the full state space, and not only the filtered
    // Because during the Viterbi run each particle has only one starting time, we have to map based on the Particle's state and not Particle identity (which includes starting times and weights)
    // (This also saves space, as p(y|x) only depends on the state and not the starting time)
    typedef typename Particle::template State_Map<Probability> record_map;

    // this map associates one marginal particle with the viterbi path that leads to this state
    typedef typename Particle::template State_Map<ViterbiPath<Particle>, std::shared_ptr<Particle> > path_map;

private: // fields

    // for every time-step the list of particles with p(y|x) values
    std::deque<record_map> records;

    std::vector<EstimateTarget<Particle> > estimators;

public: // API

    void addEstimateTarget(const EstimateTarget<Particle> &estTarget) {
        estimators.push_back(estTarget);
    }

    // go to the next time step at time t
    void advanceTime() {
        records.push_back(record_map());
    }

    // takes an internal copy of the particle
    void addDataPoint(const Particle *p) {
        records.back().insert(typename record_map::value_type(new Particle(*p), p->get_pyx()));
    }

    // add all particles for particles between begin and end (iterators over Particle*)
    template <typename It>
    void add_all(It begin, It end) {
        for (It it = begin; it != end; ++it) {
            addDataPoint(*it);
        }
    }

    // compute a Viterbi sequence of particles
    // returns the list of particles, starting at the first time-step, and the path probability
    std::pair<std::deque<std::shared_ptr<Particle> >, LogProbability> doViterbiRun(States &states);
}; // class Viterbi


template<class Particle>
std::pair<std::deque<std::shared_ptr<Particle> >, LogProbability> Viterbi<Particle>::doViterbiRun(States &states) {
    // simple sanity check: if there are more (or less) then 1 records left, then there were more or less time-steps
    // However, we require that viterbi particles have been added for all time-steps (and nothing more)
    if (records.size() != Global::timeSteps.size()) {
        std::ostringstream msg;
        msg << ("doViterbiRun: different number of time steps and Viterbi records");
        msg << ": " << Global::timeSteps.size() << " / " << records.size();
        throw std::logic_error(msg.str());
    }

    // the new viterbi paths generated for time t+1, representing all paths that end at the current time-step
    path_map next_paths;

    {
        record_map &curRecord = records.front();

        // Begin by inserting initial particles as initialisation of Viterbi path
        for (const auto &it: curRecord) {
            Particle *particle = it.first;
            Probability pyx = it.second;

            // The viterbi path manages a shared_ptr<Particle>, but the record_map contains a Particle*
            // however, particle is already a copy, so we can use this for the shared_ptr
            std::shared_ptr<Particle> p(particle);

            // insert a new particle → ViterbiPath mapping in the map (default constructs a new ViterbiPath)
            ViterbiPath<Particle>& path = next_paths[p];

            // path probability of the initial state is p(y|x) * particle weight
            path.pPath = LogProbability(pyx) * LogProbability(p->getTotalWeight());
            path.states.push_front(p);
        }

        // All particles have been moved to the paths, we no longer need the current record
        records.pop_front();
    }

    // the viterbi paths of the last time-step at time t
    path_map last_paths;

    // Iterative calculation of Viterbi paths (forward in time)
    // Conceptually, Viterbi works as follows:
    // For every particle x at time t+1, find that path among last_paths that maximises the path probability up to t+1 (path probability up to t * transition to x * p(y|x))
    // → Store this path in next_paths
    // At the end of the loop, next_paths has for every filtered particle at t+1 the path that most likely led to this particle
    //
    // This implementation avoids the "reverse lookup" ("which paths lead to the particle?") and instead iterates over all existing paths up to t-1
    // For every existing path (first iteration: only the initial states), calculate the successor particles which are filtered at t
    // For this successor particle, check if this path has greater probability than any previous path (if any) to this particle

    std::cerr << "Running viterbi for time ";

    // loop over all time-steps (starting from t=1, t=0 was initial state above)
    for (size_t timeStep = 1; timeStep < Global::timeSteps.size(); ++timeStep) {
        // invariant: last_paths is empty, next_paths is the generated list of new paths up to current time t
        // now swap both: continue with the generated list, and clear next_paths
        last_paths.swap(next_paths);

        // particles and p(y|x) values of time t
        record_map &curRecord = records.front();
        // Global::time is the time of t (required to correctly compute pStop)
        Global::time = Global::timeSteps[timeStep];

        std::cerr << Global::time << " ";

        // loop over all previous path up to time t-1 and find/update all paths up to t
        for (typename path_map::iterator it = last_paths.begin(); it != last_paths.end(); ++it) {
            // Find all successor particles of this path

            std::shared_ptr<Particle> particle = it->first; // The key in the map is the last particle of the path
            // the Viterbi path to this particle
            // this path is not modified, but for every successor a copy will be made and extended
            const ViterbiPath<Particle> &curPath = it->second;

            // we need to get the transition probability
            // create a new Particle from the particle and set its weight to 1
            // this tempParticle will be either in successorParticles or deleted below, so we do not need to free it separately
            Particle *tempParticle = new Particle(*particle);
            tempParticle->setTotalWeight(1);

            ParticleList successorParticles {tempParticle};
            Particle::step(successorParticles, states);

            // for every successor particle, check if this particle has been filtered
            // if yes, check if this particle is already the end of a Viterbi path to time t (and update if appropriate)
            for (Particle* p_Succ: successorParticles) {
                // check if this successor particle has been filtered
                typename record_map::iterator recordIt = curRecord.find(p_Succ);

                if (recordIt == curRecord.end()) {
                    // This successor particle has not been filtered (it has been pruned or got zero weight) and will definitely not be part of any Viterbi path
                    delete p_Succ;
                    continue;
                }


                // This successor particle state has been filtered

                // we will now use the predicted particle as the particle that is in the state, and not the filtered particle
                // theoretically, there should no real difference - however, the filtered particle may have adopted its internal distribution according to the observations
                // The predicted particle does not have this information (although pruning might change this negatively for the filtered particle)
                // To save one Particle allocation (recordIt->first must be copied, because curRecord's particles get deleted), we use the filtered particle
                std::shared_ptr<Particle> p_SuccPtr(p_Succ);
                Probability pyx = recordIt->second;

                Probability pTrans = prob(p_Succ->getTotalWeight());
                // get the new path probability: old path probability * transition probability * p(y|x)
                LogProbability newPathProb = curPath.pPath * LogProbability(pTrans) * LogProbability(pyx);

                typename path_map::iterator pathIt = next_paths.find(p_SuccPtr);

                if (pathIt == next_paths.end()) {
                    // there is currently no Viterbi path to this particle
                    ViterbiPath<Particle> newPath = curPath; // creates a copy
                    newPath.pPath = newPathProb;
                    newPath.states.push_front(p_SuccPtr);
                    next_paths.insert(typename path_map::value_type(p_SuccPtr, newPath));
                } else {
                    // there is already a Viterbi path
                    ViterbiPath<Particle> &path = pathIt->second;
                    if (newPathProb > path.pPath) {
                        // ... but we found a path with higher probability
                        path.pPath = newPathProb;
                        path.states = curPath.states;
                        path.states.push_front(p_SuccPtr);
                    }
                }
            } // loop over all successor particles
        } // loop over all paths up to time t

        // All particle states have been used to retrieve p(y|x), we no longer need the particles and the current record
        for (auto it: curRecord) {
            delete it.first;
        }
        records.pop_front();

        // Because there was a possible path by filtering, there must also be a Viterbi path
        assert(!next_paths.empty());

        // we iterated over all particles of the last time-step and no longer need the particles
        last_paths.clear();
    } // loop over all time-steps

    std::cerr << '\n';
    ViterbiPath<Particle> *maxPath = NULL;
    // find the path with maximum path probability
    for (auto &path: next_paths) {
        if (maxPath == NULL || *maxPath < path.second) {
            maxPath = &path.second;
        }
    }

    // the states list is saved in reverse, beginning with the last state
    // therefore, first reverse it so we return the particle in the "correct" order
    // → reverse the path and prepare the result
    std::pair<std::deque<std::shared_ptr<Particle> >, LogProbability> result;
    result.second = maxPath->pPath;
    for (const std::shared_ptr<Particle> &p: maxPath->states)
        result.first.push_front(p);

    return result;
}

} // namespace marginal::viterbi
} // namespace marginal
