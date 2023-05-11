#pragma once

#include <iostream>
#include <iomanip>
#include <sstream>
#include <memory>
#include <unordered_map>
#include <unordered_set>

#include <boost/functional/hash.hpp>

#include "../util/summation.h"
#include "../util/bench.h"
#include "../util/array.h"

#include "../estimate/estimators.h"

#include "../global.h"
#include "../heuristics.h"

#include "prune.h"
#include "viterbi.h"
#include "smoothing.h"

namespace marginal {

// the marginal filtering algorithm
template<class Particle>
class Filter {
public: // typedefs

    typedef typename Particle::Model Model;
    typedef ::State<Model> XState;
    typedef typename XState::startTime_t startTime_t;

    typedef typename Particle::ParticleList ParticleList;
    typedef typename Particle::template ParticleSet<> ParticleSet;
    typedef typename viterbi::Viterbi<Particle> Viterbi;
    typedef typename smoothing::Smoothing<Particle> Smoothing;

private: // fields

    util::array<double, Model::num_initials> initialStateWeights;
    util::array<double, Model::num_goals> goalWeights;

    // stream to write state statistics to (if not null)
    std::shared_ptr<std::ostream> o_states;

    ParticleList particles;
    ParticleList newParticles;
    // used for combining particles with the same state
    ParticleSet particleSet;

    // if set to true, particles will be shuffled randomly before pruning
    bool shuffleParticles;

    // prune implementation
    std::shared_ptr<Prune<Particle> > prune;

    // Viterbi implementation - particles are added for every time-step (if not NULL)
    std::shared_ptr<Viterbi> viterbi;
    // smoothing implementation - particles are added for every time-step (if not NULL)
    std::shared_ptr<Smoothing> smoothing;

    std::vector<EstimateTarget<Particle> > estimators;

protected:

    States &states;

public: // getters and setters

    const util::array<double, Model::num_initials>& getInitialStateWeights() const { return initialStateWeights; }
    const util::array<double, Model::num_initials>& getGoalWeights() const { return goalWeights; }
    void setInitialStateWeights(std::map<int, double> weights);
    void setGoalWeights(std::map<int, double> weights);

    std::shared_ptr<std::ostream> getStateStatisticsStream() const { return o_states; }
    void setStateStatisticsStream(std::shared_ptr<std::ostream> o_states) { this->o_states = o_states; }

    bool getShuffleParticles() const { return shuffleParticles; }
    void setShuffleParticles(bool shuffleParticles) { this->shuffleParticles = shuffleParticles; }

    std::shared_ptr<Prune<Particle> > getPrune() const { return prune; }
    void setPrune(std::shared_ptr<Prune<Particle> > prune) { this->prune = std::move(prune); }

    std::shared_ptr<Viterbi> getViterbi() const { return viterbi; }
    void setViterbi(std::shared_ptr<Viterbi> viterbi) { this->viterbi = viterbi; }

    std::shared_ptr<Smoothing> getSmoothing() const { return smoothing; }
    void setSmoothing(std::shared_ptr<Smoothing> smoothing) { this->smoothing = smoothing; }

    void addEstimateTarget(const std::shared_ptr<Estimator<Particle> > &estimator, const std::shared_ptr<std::ostream> &os) {
        addEstimateTarget(EstimateTarget<Particle>(estimator, os));
    }
    void addEstimateTarget(const EstimateTarget<Particle> &estimateTarget) { estimators.push_back(estimateTarget); }

    // sets the particle options this filter should set for every particle
    void setParticleOptions(typename Particle::Options options) { Particle::options = options; }

protected: // hidden methods

    void printTime(bool& printed) {
        if (printed)
            return;
        printed = true;
        std::clog << "\nAt time " << Global::time << '\n';
    }

public: // public methods

    // create a new filter instance
    Filter(States &states) :
        shuffleParticles(false),
        states(states)
    { }

    ~Filter();

    // Prints some statistics about the current states and particles to o_states.
    // See parameters.h for a description of the pre-processor macros STATESTAT_*.
    void createStateStatistics();

    void normalizeAndEstimate();

    void run();
}; // class Filter



template<class Particle>
Filter<Particle>::~Filter() {
    for (Particle *p: particles) {
        delete p;
    }
    particles.clear();
}


template<class Particle>
void Filter<Particle>::setInitialStateWeights(std::map<int, double> weights) {
    // initialise goal and initial state weights to -1
    // the normalisation logic applied to these weights in most cases "does the right job™"
    // (setting nothing: uniform distribution, setting single weights that sum to one: all others -1 get 0)
    initialStateWeights.fill(-1);

    // overwrite with user-defined values
    for (const auto weight: weights) {
        initialStateWeights[weight.first] = weight.second;
    }

    normalize(initialStateWeights);
}


template<class Particle>
void Filter<Particle>::setGoalWeights(std::map<int, double> weights) {
    // ditto
    goalWeights.fill(-1);

    for (const auto weight: weights) {
        goalWeights[weight.first] = weight.second;
    }

   normalize(goalWeights);
}


template<class Particle>
void Filter<Particle>::createStateStatistics() {
    if (!o_states)
        return;

#if STATESTAT__STATMAP
    // put every active state in the map
    StateStatMap statmap;
#endif
#if STATESTAT_PARTICLE_WEIGHTS
    ostd::stringstream pweights; // collect weights
#endif

#if STATESTAT_NUM_XSTATES
    size_t XStateCount = 0;
#endif

    // TODO add several stateStatistic classes, much like estimators (issue #36)
#if STATESTAT_OBSEQUIV_NSETS
    // count how many sets of obsEquiv states there are (only valid for obsEquiv Particles)
    // this set is smaller than nparticles, because some particles are in the same state,
    // but execute different actions
    struct StateTablePtr_obsHash {
        size_t operator()(const StateTablePtr s) const {
            typename Model::opid_t opid; // dummy opid
            opid.fill(0);
            return model::observation_hash_value(&s->key, opid);
        }
    };
    struct StateTablePtr_obsEqual {
        bool operator()(const StateTablePtr x, const StateTablePtr y) const {
            typename Model::opid_t opid; // dummy opid
            opid.fill(0);
            return model::observationEquivalent(&x->key, opid, &y->key, opid);
        }
    };
    std::unordered_set<StateTablePtr, StateTablePtr_obsHash, StateTablePtr_obsEqual, pool_allocator<StateTablePtr>> obsEquivStates;
#endif

#if STATESTAT__STATMAP || STATESTAT_PARTICLE_WEIGHTS || STATESTAT_NUM_XSTATES
    for (const Particle *pPtr: particles) {
        const Particle& p = *pPtr;

#if STATESTAT_NUM_XSTATES
        XStateCount += p.getStateCount();
#endif
#if STATESTAT_PARTICLE_WEIGHTS // print only total weight summed over all starting times
        pweights << p.getTotalWeight() << " ";
#endif

#if STATESTAT_OBSEQUIV_NSETS
        // simply count only the first state - every state occurs only once in all particles
        // FIXME: order for different particles may be different
        obsEquivStates.insert(p.getCurrentState().begin()->first);
#endif

#if STATESTAT__STATMAP
        // each particle may represent a distribution over different states, so iterate over them
        for(const auto &p_state : p.template getDistribution<SubState<Model, StateVar_S>>()) {
            StateStatistics& stat = statmap[p_state.first.modelState]; // creates element if not in the table
            (void) stat;
#if STATESTAT__NPARTICLES
            stat.nParticles++;
#endif
#if STATESTAT_MIN_PX
            stat.px += prob(p_state.second); // sum weights for state probability
#endif
        }
#endif // STATESTAT__STATMAP
    }
#endif // STATESTAT__STATMAP || STATESTAT_PARTICLE_WEIGHTS || STATESTAT_NUM_XSTATES


    (*o_states) << Global::time
#if STATESTAT_NUM_PARTICLES
               << ' ' << particles.size()
#endif
#if STATESTAT_NUM_STATES
               << ' ' << statmap.size()
#endif
#if STATESTAT_OBSEQUIV_NSETS
               << ' ' << obsEquivStates.size()
#endif
#if STATESTAT_NUM_XSTATES
               << ' ' << XStateCount
#endif
               ;

#if STATESTAT_STATE_PARTICLES || STATESTAT_STATE_PYX || STATESTAT_MIN_PX
#if STATESTAT_MIN_PX
    Probability minPx(1);
#endif
#if STATESTAT_STATE_PARTICLES || STATESTAT_STATE_PYX
    std::stringstream ostate;
#endif
    for (StateStatMap::iterator it = statmap.begin(); it != statmap.end(); ++it) {
        StateStatistics stat = it->second;
#if STATESTAT_MIN_PX
        if (stat.px > ZeroProbability && stat.px < minPx)
            minPx = stat.px;
#endif
#if STATESTAT_STATE_PARTICLES // number of particles per state
        ostate << ' ' << stat.nParticles;
#endif
#if STATESTAT_STATE_PYX // pyx for each state TODO this is wrong when using action observations (pyx changes)
        const StateDataRec &sd = it->first.second;
        ostate << ':' << sd->pyx;
#endif
    }
#if STATESTAT_MIN_PX
    (*o_states) << ' ' << minPx;
#endif
#endif // final STATESTAT iteration

    typename Particle::Statistics particle_statistics;
    if (Particle::Statistics::enabled::value) {
        for (const Particle *pPtr: particles)
            particle_statistics.collect(*pPtr);
    }
    (*o_states) << ' ' << particle_statistics;

#if STATESTAT_STATE_PARTICLES || STATESTAT_STATE_PYX
    (*o_states) << "## " << ostate.str();
#endif

#if STATESTAT_PARTICLE_WEIGHTS
    (*o_states) << " #weights# " << pweights.str();
#endif

    (*o_states) << std::endl; // flush after every timestep
}

template<class Particle>
void Filter<Particle>::normalizeAndEstimate() {

    // compute total weight for normalising
    for (const Particle *p: particles) {
        Global::wtotal += p->getTotalWeight();
    }

    Global::loglik += p_logValue(prob(Global::wtotal));

    createStateStatistics();

    // normalise particle weights, calculate wsq
    // Note: wsq is computed based on the total weights of the particle, not the individual X-States they might represent
    Sum<Weight> wsq = ZeroWeight;
    for (Particle *p: particles) {
        p->normalizeWeight(Global::wtotal);
        wsq += p->getTotalWeight();
    }

    // run and print estimators
    runEstimators(Global::time, estimators.begin(), estimators.end(), particles.begin(), particles.end());

#if VERBOSITY > 1
    std::clog << ": LL=" << Global::loglik
         << ", Sum(w)=" << p_value(prob(Global::wtotal))
         << ", Sum(w^2)=" << wsq.sum()
         << '\n';
#endif

    Global::wtotal = ZeroWeight;
}


template<class Particle>
void Filter<Particle>::run() {
    if (prune.get() == NULL)
        throw std::runtime_error("marginal filter: pruning strategy not set");

#if VERBOSITY > 0
    std::clog << "Marginal filter for domain: \"" << model::DOMAIN_NAME << '"'
         << ", problem: \"" << model::PROBLEM_NAME << '"'
         << ", " << model::magMode << " agent mode"
#ifdef USE_LOG_PROBS
         << ", using log probabilities"
#endif
         << '\n';
#endif


#if VERBOSITY > 1
    std::cerr << "Normalized goal weights:\n";
    for (size_t i = 0; i < goalWeights.size(); ++i)
        std::cerr << model::goalNames[i] << " : " << goalWeights[i] << '\n';
    std::cerr << "Normalized initial state weights:" << '\n';
    for (size_t i = 0; i < initialStateWeights.size(); ++i)
        std::cerr << model::initialStateNames[i] << " : " << initialStateWeights[i] << '\n';
#endif

    if (o_states != NULL) {
        // print header for state statistics file
        (*o_states) << "Time"
#if STATESTAT_NUM_PARTICLES
                    << " Particles"
#endif
#if STATESTAT_NUM_STATES
                    << " States"
#endif
#if STATESTAT_OBSEQUIV_NSETS
                   << " ObsEquivStateSets"
#endif
#if STATESTAT_NUM_XSTATES
                    << " X-States"
#endif
#if STATESTAT_MIN_PX
                    << " minPx"
#endif
                    ;
        if (Particle::Statistics::enabled::value) {
            (*o_states) << ' ';
            Particle::Statistics::header(*o_states);
        }
        (*o_states) << '\n'
                    << std::setprecision(std::numeric_limits<double>::max_digits10);
    }

    // print header for estimators
    for (auto &est: estimators)
        est.printHeader();

    Global::time = 0;
    Global::timeSteps.push_back(0);
    model::fetchObservation();

    { // init
        if (smoothing != NULL)
            smoothing->advanceTime();

        // first create all initial particles and merge them (they may by chance be equal, or certain particle classes may treat them as equal)
        particleSet.clear();

        typename XState::opid_t opid;
        for (size_t i = 0; i < Model::n_agents; i++)
            opid[i] = model::InitOpId;

        startTime_t startTime;
        for (size_t i = 0; i < Model::n_agents; i++)
            startTime[i] = Global::time;

        for (int is = 0; is < NUM_INITIAL_STATES; is++) {
            model::StateRec xrec;
            model::sampleInitial(is, &xrec);
            auto insResult = states.states.emplace(xrec, StateDataRec());
            StateTablePtr modelState = &(*(insResult.first));

            for (int gdi = 0; gdi < NUM_GOALS; gdi++) {
                XState state(is, gdi, modelState, opid, startTime);
                Particle* p = new Particle(state, Weight(goalWeights[gdi] * initialStateWeights[is]));
                Particle::addParticle(p, particles, particleSet);
            }

            // call the landmark heuristic also in the first step (path dependent)
            if (parameters.currentHeuristic == 1) {
                landmarkHeuristic(states, xrec, 0);
            }
        }

        particleSet.clear();

        for (Particle *p: particles) {
            if (smoothing != NULL)
                smoothing->addPredicted(p);

            // update initial state for the initial observation
            p->update();

        }

        normalizeAndEstimate();

        if (smoothing != NULL) {
            for (Particle *p: particles)
                smoothing->addFiltered(p);
        }
        if (viterbi != NULL) {
            viterbi->advanceTime();
            viterbi->add_all(particles.begin(), particles.end());
        }
    } // init


#if BENCH > 0
    Bench bench_loop;
#endif
    for (int t = 1; t < TimeSteps && model::fetchObservation(); t++) {
        Global::deltaT = t - Global::time;
        Global::time = t;
        Global::timeSteps.push_back(t);
        bool printedTime = false;

#if VERBOSITY > 1
        unsigned long nstates = states.states.size();
        printTime(printedTime);
#endif

        // compute all possible next states
#if BENCH > 1
        Bench bench;
#endif
        StepResult stepResult = Particle::step(particles, states);
        (void) stepResult; // ignore "unused" warnings
#if VERBOSITY > 1
        size_t numDead = stepResult.numDead;
        Weight weightDead = stepResult.numDead;
#endif


#if BENCH > 1
        std::cerr << "Bench: predict took " << bench.stop() << " ms\n";
#endif

#if VERBOSITY > 1
        std::clog << "*** Predicted " << particles.size()
             << ", reached " << (states.states.size() - nstates) << " new states"
             << '\n';
        if (numDead > 0)
            std::clog << numDead << (numDead > 1 ? " Particles" : " Particle")
                 << " reached dead end, sum(weight) = " << weightDead << '\n';
#endif
        if (particles.size() == 0) {
            printTime(printedTime);
            std::clog << "All particles reached dead end before end of observation.\n";
            break;
        }

#if VERBOSITY > 1
        // count dead particles after observations
        numDead = 0;
#endif
#if BENCH > 1
        bench.reset();
#endif
        if (smoothing != NULL)
            smoothing->advanceTime();

        // update particles
        for (Particle *p: particles) {
            if (smoothing != NULL)
                smoothing->addPredicted(p);

            p->update();
            if (p->getTotalWeight() != ZeroWeight) {
                newParticles.push_back(p);
            } else {
#if VERBOSITY > 1
                numDead++;
#endif
                delete p;
            }
        }
        particles.clear();
        particles.swap(newParticles);

#if BENCH > 1
        std::cerr << "Bench: update took " << bench.reset() << " ms\n";
#endif
#if VERBOSITY > 1
        if (numDead > 0)
            std::clog << numDead << (numDead > 1 ? " Particles" : " Particle") << " dead after observations.\n";
#endif
        if (particles.size() == 0) {
            printTime(printedTime);
            std::clog << "All particles dead: impossible observations.\n";
            Global::timeSteps.pop_back();
            if (smoothing != NULL) {
                smoothing->removePredicted();
            }
            break;
        }

        if (shuffleParticles)
            random_shuffle(particles.begin(), particles.end(), Global::random_shuffle);

#if LIMIT_STATES > 0
        // limit the number of distinct <S,A>-states to favour concentration on starting times
        limitStates<Particle>(particles, LIMIT_STATES);
#endif

        if (Global::nparticles > 0) {
            prune->prune(particles, Global::nparticles);
#if BENCH > 1
            std::cerr << "Bench: pruning took " << bench.reset() << " ms\n";
#endif
        }

        normalizeAndEstimate();

        if (smoothing != NULL) {
            for (Particle *p: particles)
                smoothing->addFiltered(p);
        }

#if BENCH > 1
        std::cerr << "Bench: normalize and estimate took " << bench.stop() << " ms\n";
#endif
#if BENCH > 0
        std::cerr << "Bench: one time-step took " << bench_loop.reset() << " ms\n";
#endif
        if (viterbi != NULL) {
            viterbi->advanceTime();
            viterbi->add_all(particles.begin(),particles.end());
        }
    } // end for-loop

    std::clog << "*** Final log-likelihood: " << Global::loglik << '\n'
         << "    Finishing time:       " << Global::time << '\n';


#if ENDSTAT_REACH_STATES
    size_t reach = 0;
    for (auto &e: states.states) {
        if (e.second.visited)
            ++reach;
    }
    std::clog << "Reached " << reach << " states in total" << '\n';
#endif
}

} // namespace marginal
