#pragma once

#include <iostream>
#include <algorithm>
#include <memory>

#include "../util/allocator.h"
#include "../util/summation.h"

#include "../config.h"
#include "../particle_common.h"

#ifndef MARGINAL_PRUNE_LIMIT
#define MARGINAL_PRUNE_LIMIT 100
#endif

namespace marginal {

// virtual base class for pruning strategies
template<class Particle>
class Prune {
public:
    static std::shared_ptr<Prune<Particle>> construct(PruneStrategy s);

    virtual void prune(typename Particle::ParticleList& particles, size_t limit) = 0;
};

/*
Use a beam search-like strategy: only keep the N with highest weight
Do this by partitioning, i.e. select the Nth largest element. The order of the largest elements does not matter, really.
Using the quickselect algorithm, this has a complexity of O(N) (if we would use median-of-medians, this would be guaranteed O(N))
*/
template<class Particle>
class Prune_Beam : public Prune<Particle> {
public:
    void prune(typename Particle::ParticleList& particles, size_t limit) {
        if (particles.size() <= limit)
            return;

        nth_element(particles.begin(), particles.begin()+(limit-1), particles.end(), compareParticlesDesc<Particle>);

        // select spare particles
        while (particles.size() > limit) {
            Particle* p = particles.back();
            particles.pop_back();
            delete p;
        }
    }
}; // class Prune_Beam

/*
Use a beam search-like strategy: only keep the <limit> particles with highest weight.
Slow (log-linear) version based on sorting.
This has two main drawbacks:
* Sorting takes n*log n time
* If the "correct" particles have all the least weight below the <limit>th particle, they will be completely pruned.
  Assume you have N particles, and each particle has 2 actions *a* and *b* to execute.
  The reuslting set then has 2*N particles. If the observation assign a weight of 0.9 to *a* and 0.1 to *b*,
  the pruned set will consist only of particles with action *a* executed (because all N particles have weight 0.9 > 0.1).
*/
template<class Particle>
class Prune_Beam_slow : public Prune<Particle> {
public:
    void prune(typename Particle::ParticleList& particles, size_t limit) {
        // Keep only up to limit particles.
        // Do this by using partial_sort:
        // Sort only so that the limit particles with the biggest weight are first.
        // The order of the other particles to throw away is irrelevant.
        // Or, if there are less then limit particles to throw away, sort them in ascending order,
        // so that particles with the lowest weight are first. This reduces sorting time.

        if (particles.size() >= 2 * limit) {
            // less particles to keep than to throw away
            // → sort only particles with biggest weight
            partial_sort(particles.begin(), particles.begin() + limit, particles.end(), compareParticlesDesc<Particle>);
            while (particles.size() > limit) {
                Particle* p = particles.back();
                particles.pop_back();
                delete p;
            }
        } else if (particles.size() > limit) {
            // less particles to throw away than to keep
            // → sort only the particles with smallest weight
            partial_sort(particles.begin(), particles.end() - limit, particles.end(), compareParticlesAsc<Particle>);
            while (particles.size() > limit) {
                Particle* p = particles.front();
                particles.pop_front();
                delete p;
            }
        }
    }
}; // class Prune_Beam_slow

/*
Use an approximated quick-sort by sorting only a limited number of levels.

We just want to have the largest <limit> particles, no matter in what order.
This is essentially what the first step of quick-sort does: partition.
So just partition the particles until
a) we have the limit largest particles at the beginning or
b) we have reached a limited number of iterations; in this case the order is just approximated.
*/
template<class Particle>
class Prune_NearQuick : public Prune<Particle> {
public:
    void prune(typename Particle::ParticleList& particles, size_t limit) {
        using std::swap; // use best suiting swap by ADL, or fall back to std::swap

        if (particles.size() <= limit)
            return;

        // accepted particles are those the weight of which is definitely among the largest <limit> particles
        // rejected are those the weight of which is definitely not among the largest <limit> particles
        size_t accepted = 0; // all elements < accepted are accepted
        size_t rejected = particles.size(); // all elements >= rejected are rejected

        int it = 1; // iteration
        while (accepted < rejected && it <= MARGINAL_PRUNE_LIMIT) {
            it++;
            Particle* p = particles[(accepted + rejected-1) / 2]; // pivot element
            Weight nw = p->getTotalWeight();

            // collect candidate particles with weight w > nw
            size_t selected = accepted; // all < selected and >= accepted are possible candidates, and have weight >= nw
            size_t pivot = selected; // all >= pivot and < selected have weight w = nw

            // 3-way partition, keeping the particles with weight w = nw at the right-most end of "selected"
            // layout: [ accepted | <accepted> >weight ... | <pivot> =weight ... | <selected>  <weight ... | <rejected> rejected]
            // heuristic: we will have only few with weight =w, therefore swap them always to their right position
            for (size_t i = accepted; i < rejected; ++i) {
                Weight w = particles[i]->getTotalWeight();
                if (w >= nw) {
                    // this is a candidate
                    swap(particles[i], particles[selected]);

                    if (pivot != selected)
                        // we have at least one element with weight w = nw, thus swap this particle to the end
                        swap(particles[pivot], particles[selected]);
                    if (w != nw)
                        // do not increase the number of elements with weight w = nw
                        pivot++;

                    selected++;
                }
            }

            if (selected > limit) {
                // we must select less particles as candidates
                if (pivot <= limit) {
                    // all particles between pivot and selected have same weight, and cover limit -> finished
                    accepted = limit;
                    rejected = limit;
                } else {
                    // reject all particles with weight w <= nw
                    rejected = pivot;
                }
            } else if (selected <= limit) {
                // criterion holds, we can accept all selected candidates
                accepted = selected;
                if (selected == limit)
                    // finished
                    rejected = accepted;
            }
        }

        // all others must be deleted
        while (particles.size() > limit) {
            Particle* p = particles.back();
            particles.pop_back();
            delete p;
        }
    }
}; // class Prune_NearQuick

/*
Implement the resampling algorithm of Fearnhead and Clifford, 2003, "On-line Inference for Hidden Markov Models via Particle Filters"

The key idea is:
Determine some threshold 1/c
All particles with weight w >= 1/c are resampled directly, as in beam-search. The weights are kept as-is.
All remaining particles are subject to stratified resampling, as done by the particle filter.
The threshold 1/c guarantees that stratified resampling will never sample a particle twice.
The threshold 1/c will be the new weight of the resampled particles.

If the original set of particles is normalised, the resampled set will also be normalised.

let 1/c=nw, the new weight of the resampled particles
let numSel be the number of particles with weight w >= 1/c, i.e. those resampled directly ("accepted")
let weightRest be the weight of the particles not accepted
The following criterion holds:
weightRest / nw + numSel = limit

The goal is to find the smallest 1/c, such that this equation holds.

Finding 1/c is done by partitioning the particles, keeping three sets:
accepted = particles accepted, with weight w >= 1/c
cands = candidate particles for accepting
rejected = particles definitely not accepted

The algorithm works as follows:
cands = particles
Select 1/c=nw from cands, count numSel = number of particles with weight w >= nw, weightRest=weight of non-selected particles
if criterion > N -> reject all <= k, recurse
if criterion < N -> accept all >= k, recurse
if criterion = N -> accept all >= k, finished
where criterion = weightRest / nw + numSel
*/
template<class Particle>
class Prune_FC03 : public Prune<Particle> {
public:
    void prune(typename Particle::ParticleList& particles, size_t limit) {
        using std::swap; // use best suiting swap by ADL, or fall back to std::swap

        if (particles.size() <= limit)
            return;

        // weights need to be normalised
        {
            Sum<Weight> wtotal_sum = ZeroWeight;
            for (typename Particle::ParticleList::iterator it = particles.begin(); it != particles.end(); ++it) {
                wtotal_sum += (*it)->getTotalWeight();
            }
            Weight wtotal = wtotal_sum.sum();
            for (typename Particle::ParticleList::iterator it = particles.begin(); it != particles.end(); ++it) {
                (*it)->normalizeWeight(wtotal);
            }
        }

        // first step: determin 1/c=nw by partitioning and at the same time counting numSel, weightRest
        // partitioning automatically sorts the accepted particles (weight w >= 1/c) to the left

        size_t accepted = 0; // all elements < accepted are accepted
        size_t rejected = particles.size(); // all elements >= rejected are rejected
        Sum<Weight> weightRest = ZeroWeight; // sum of weight of not accepted particles
        Weight nw = ZeroWeight;

        while (accepted < rejected) {
            Particle* p = particles[(accepted + rejected-1) / 2]; // pivot element
            nw = p->getTotalWeight(); // candidate value for nw 1/c

            // collect candidate particles with weight w > candidate nw
            size_t selected = accepted; // all < selected and >= accepted are possible candidates, and have weight >= nw
            size_t pivot = selected; // all >= pivot and < selected have weight w = nw
            Sum<Weight> weightNonSel = ZeroWeight; // weight of the non-candidates

            // 3-way partition, keeping the particles with weight w = nw at the right-most end of "selected"
            // layout: [ accepted | <accepted> >weight ... | <pivot> =weight ... | <selected>  <weight ... | <rejected> rejected]
            // heuristic: we will have only few with weight =w, therefore swap them always to their right position
            for (size_t i = accepted; i < rejected; ++i) {
                Weight w = particles[i]->getTotalWeight();
                if (w >= nw) {
                    // this is a candidate
                    swap(particles[i], particles[selected]);

                    if (pivot != selected)
                        // we have at least one element with weight w = nw, thus swap this particle to the end
                        swap(particles[pivot], particles[selected]);
                    if (w != nw)
                        // do not increase the number of elements with weight w = nw
                        pivot++;

                    selected++;
                } else
                    weightNonSel += w;
            }

            // check criterion
            double criterion = p_value(prob(weightRest / nw)) + selected;
            if (criterion > limit) {
                // the criterion does not hold, we must select less particles as candidates
                // reject all particles with weight w <= nw
                rejected = pivot;
                weightRest += weightNonSel;
            } else if (criterion <= limit) {
                // criterion holds, we can accept all selected candidates
                accepted = selected;
                if (criterion == limit)
                    // finished
                    rejected = accepted;
            }
        }

#if VERBOSITY > 1
        std::clog << "resample: accepted " << accepted << " particles directly\n";
#endif

        // second step: use stratified resampling to select among all not accepted
        if (accepted < limit) {
            // we have not resampled all particles yet
            Weight winc = weightRest.sum() / Weight(limit - accepted);
            Sum<Weight> wlimit = Weight(randomval()) * winc;
            Sum<Weight> wsum = ZeroWeight;

            size_t resampled = accepted;
            for (size_t i = accepted; i < particles.size() && resampled < limit; ++i) {
                wsum += particles[i]->getTotalWeight();
                if (wsum.sum() > wlimit.sum()) {
                    // resample this particle
                    // note that it is guaranteed that no particles will be resampled twice
                    // the whole particle gets weight nw (this is constant for all sampled particles)
                    particles[i]->setTotalWeight(nw); // Update weight of all starting times
                    swap(particles[resampled], particles[i]);
                    resampled++;
                    wlimit += winc;
                }
            }
        }

        // all others must be deleted
        while (particles.size() > limit) {
            Particle* p = particles.back();
            particles.pop_back();
            delete p;
        }
    }
}; // class Prune_FC03

/*
Uses the resampling strategy of the particle filter.
In combination with the marginal filtering approach, this effectively results in a particle filter scheme with the optimal proposal function.
In difference with the other pruning strategies, a particle can be sampled more than once.
Each particle gets weight n/limit, where n is the number how often the particle has been sampled.

TODO Complexity in worst case / average case?
*/
template<class Particle>
class Prune_OptimalProposal : public Prune<Particle> {
public:
    void prune(typename Particle::ParticleList& particles, size_t limit) {
        Weight const winc = 1.0/limit;

        Sum<Weight> wlimit = Weight(randomval()) * winc;
        Sum<Weight> wsum = ZeroWeight; // the current sum of weight ("position") of all processed particles
        typename Particle::ParticleList resample; // target list of resample particles

        // For resampling, all particle weights need to be normalised
        Sum<Weight> wtotal = ZeroWeight;
        for (typename Particle::ParticleList::iterator it = particles.begin(); it != particles.end(); ++it) {
            wtotal += (*it)->getTotalWeight();
        }

        size_t resampleNum = 0; // how many particles have been resampled

        for (typename Particle::ParticleList::iterator it = particles.begin(); it != particles.end(); ++it) {
            Particle* p = *it;
            Weight weight = p->getTotalWeight() / wtotal;
            wsum += weight;

            size_t count = 0; // how often this particle gets sampled
            while (wlimit < wsum && resampleNum < limit) {
                count++;
                resampleNum++;
                wlimit += winc;
            }

            if (count > 0) {
                p->setTotalWeight(Weight(count) * winc);
                resample.push_back(p);
            } else {
                // this particle has not been sampled
                delete p;
            }
        }

        if (resampleNum != limit) {
            std::clog << "*** Resampled " << particles.size() << " to " << resampleNum << " particles "
                 << " " << winc << "\n";
            throw std::runtime_error("Increment of loop variable didn't work properly");
        }

        particles.clear();
        // swap lists
        particles.swap(resample);
    }
}; // class Prune_OptimalProposal


// limit the number of distinct <S,A>-states (for evaluating distribution of starting times)
// currently only works for the single_particle
template <class Particle>
void limitStates(typename Particle::ParticleList &particles, const size_t count) {
    // have each SA state only one in a set, ordered by its weight
    struct SAWeight {
        typedef typename Particle::Model::opid_t opid_t;
        StateTablePtr state;
        opid_t opid;
        Sum<Weight> weight;

        SAWeight(StateTablePtr s, opid_t o, Weight w) :
            state(s), opid(o), weight(w)
        { }

        bool operator<(const SAWeight &x) const {
            // all instances with equal SA-state are equal (we want to modify the weight)
            // defines an ordering first using state, then using opid
            if (std::less<StateTablePtr>()(state, x.state))
                // only std::less of pointer types guarantees strict ordering (operator< does not)
                return true;
            if (state == x.state)
                return opid < x.opid;
            else
                return false;
        }
    };

    std::set<SAWeight, std::less<SAWeight>, pool_small_allocator<SAWeight>> states;
    for (Particle *p : particles) {
        auto &state = p->template getDistribution<SubState<typename Particle::Model, (StateVar_S | StateVar_A)>>().begin()->first; // works only for single::Particle
        SAWeight saw(state.modelState, state.opid, p->getTotalWeight());
        auto saw_it = states.find(saw);
        if (saw_it == states.end())
            // SA-state not encountered yet
            states.insert(saw);
        else {
            // update weight
            saw.weight += saw_it->weight;
            states.erase(saw_it);
            states.insert(saw);
        }
    }

    if (states.size() <= count)
        // less states than required → do not need to do anything
        return;

    // remove all particles the SA-states are not among the top <count>

    // get the <count+1>th largest SA-state by weight
    auto lastState = states.begin();
    for (size_t i = 0; i < count; i++)
        lastState++;

    typename Particle::ParticleList selected;
    for (Particle *p : particles) {
        auto &state = p->template getDistribution<SubState<typename Particle::Model, (StateVar_S | StateVar_A)>>().begin()->first;
        SAWeight saw(state.modelState, state.opid, p->getTotalWeight());
        if (saw < *lastState)
            // this particle's SA state "comes before" the lastState according to the set, thus has more weight
            selected.push_back(p);
        else
            // particle not selected → delete it
            delete p;
    }

    using std::swap;
    swap(selected, particles);
}

// static
template<class Particle>
std::shared_ptr<Prune<Particle>> Prune<Particle>::construct(PruneStrategy s) {
    Prune<Particle> *p = NULL;
    switch (s) {
    case prune_beam: p = new Prune_Beam<Particle>(); break;
    case prune_beam_slow: p = new Prune_Beam_slow<Particle>(); break;
    case prune_nearquick: p = new Prune_NearQuick<Particle>(); break;
    case prune_fc03: p = new Prune_FC03<Particle>(); break;
    case prune_optimal_proposal: p = new Prune_OptimalProposal<Particle>(); break;
    default: throw std::logic_error("Prune::construct: Unknown pruning strategy");
    }
    return std::shared_ptr<Prune<Particle>>(p);
}
} // namespace marginal
