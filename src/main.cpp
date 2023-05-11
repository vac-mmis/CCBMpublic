#include <iostream>
#include <iomanip>
#include <fstream>
#include <strings.h>
#include <cmath>
#include <cassert>
#include <ctime>
#include <limits>
#include <cstring>
#include <unistd.h>
#include <vector>
#include <memory>

#include <boost/lexical_cast.hpp>

#include "util/allocator.h"
#include "util/functional.h"
#include "util/summation.h"
#include "util/bench.h"
#include "util/parameter.hpp"

#include "estimate/estimators.h"

#include "config.h"
#include "global.h"
#include "heuristics.h"
#include "particle.h"

using namespace std;

namespace particle {

//////////////////////////////////////////////////////////////////////
// Particles

/********************************************************************
 Note: Upon particle replication, we have to make a shallow copy of
 the Hash-Table used for keeping the history.

 Dying particles create a memory hole (although not worse than simply
 not replicating particles). If this becomes unbearable, we might begin
 to set up a Mark / Scan-like garbage collector.

 Interesting Question:
 - How fast would this thing be in Haskell?
 - dito for LISP?
*/


/* State record for a particle.
   For a multi-agent setting, there would be an *array*
   of (opid,time) pairs: one pair for each agent.
     for sampling within a time step, we'd:
     (1) determine a random sequence for all agents
     (2) sequentially draw & apply an op for each agent
     if no agent is able to act, we have a deadlock
 */


// the list of all desired estimators
// TODO make the particle filter a class, have this as a field
std::vector<EstimateTarget<Particle> > estimators;

// runEstimators wants an iterator over pointer, not just iterator
// therefore, we need an iterator that iterates over Particle[] and dereferences to Particle*
struct ItProxy {
    // the current position
    Particle *p;
    ItProxy(Particle *p) : p(p) {}
    bool operator==(const ItProxy &other) { return p == other.p; }
    bool operator!=(const ItProxy &other) { return !(*this == other); }
    ItProxy& operator++() { // prefix
        ++p;
        return *this;
    }
    ItProxy operator++(int) { // postfix
        ItProxy tmp(*this);
        operator++();
        return tmp;
    }
    Particle* operator*() { return p; }
    Particle* operator->() { return p; }
};


// Prints some statistics about the current states and particles to ostates.
// See parameters.h for a description of the pre-processor macros STATESTAT_*.
void createStateStatistics(ofstream *ostates, std::vector<Particle> &particles) {
    if (NULL == ostates) { // no output requested
        return;
    }

#if STATESTAT_UNIQ_PARTICLES
    typedef Particle::ParticleSet<> ParticleSet;
    ParticleSet particleSet; // Collecting all particles, one per equal states
#endif


#if STATESTAT_NUM_PARTICLES
    size_t nactive = 0;
#endif

#if STATESTAT__STATMAP
    StateStatMap statmap;
#endif
#if STATESTAT_PARTICLE_WEIGHTS
    ostringstream pweights; // some separator
#endif

#if STATESTAT__STATMAP || STATESTAT_UNIQ_PARTICLES || STATESTAT_PARTICLE_WEIGHTS
    // put every active state in the map
    for (size_t i = 0; i < Global::nparticles; i++) {
        Particle& p = particles[i];
        if (p.weight <= ZeroWeight)
            continue;
#if STATESTAT_NUM_PARTICLES
        nactive++; // count active particles
#endif
#if STATESTAT_UNIQ_PARTICLES
        particleSet.insert(&p);
#endif
#if STATESTAT_PARTICLE_WEIGHTS
        pweights << p.weight << " ";
#endif

#if STATESTAT__STATMAP
        // each particle may represent a distribution over different states, so iterate over them
        for(const auto &p_state : p.getDistribution<SubState<Particle::Model, StateVar_S>>()) {
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
#endif // STATESTAT__STATMAP || STATESTAT_UNIQ_PARTICLES || STATESTAT_PARTICLE_WEIGHTS


    (*ostates) << Global::time
#if STATESTAT_NUM_PARTICLES
               << ' ' << nactive
#endif
#if STATESTAT_NUM_STATES
               << ' ' << statmap.size()
#endif
#if STATESTAT_UNIQ_PARTICLES
               << ' ' << particleSet.size()
#endif
               ;

#if STATESTAT_STATE_PARTICLES || STATESTAT_STATE_PYX || STATESTAT_MIN_PX
#if STATESTAT_MIN_PX
    Probability minPx(1);
#endif
#if STATESTAT_STATE_PARTICLES || STATESTAT_STATE_PYX
    ostringstream ostate;
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
    (*ostates) << ' ' << minPx;
#endif
#if STATESTAT_STATE_PARTICLES || STATESTAT_STATE_PYX
    (*ostates) << "# " << ostate.str();
#endif
#endif // final STATESTAT iteration

#if STATESTAT_PARTICLE_WEIGHTS
    (*ostates) << " | " << pweights.str();
#endif

    (*ostates) << std::endl; // flush after every timestep
}

void resample(std::vector<Particle> &pfrom, std::vector<Particle> &pto) {
    Probability winc(1.0/Global::nparticles);
    // Using optimized sum implementation for type double
    Sum<Probability> wlimit(Probability(randomval()) * winc);
    Sum<Probability> wsum;

    size_t i = 0;
    size_t j = 0;

    do {
        Particle* p = &pfrom[i++];
        Weight weight = p->weight;
        wsum += prob(weight);

        while (wlimit < wsum && j < Global::nparticles) {
            pto[j] = *p;
            prob(pto[j].weight,winc);

            j++;
            wlimit += winc;
        }
    } while (i<Global::nparticles);
    if ( (i != Global::nparticles) || (j != Global::nparticles)) {
        cerr << "*** Resampled " << i << " to " << j << " particles "
             << " " << winc << "\n";
        throw runtime_error("Increment of loop variable didn't work properly");
    }
}

Weight normalizeAndEstimate(ofstream *o_states, std::vector<Particle> &particles, std::vector<Particle> &resamples) {
#if USE_LOG_PROBS || USE_LOG_WEIGHTS
    if (Global::wtotal.sum() == ZeroWeight) {
#else
    if (prob(Global::wtotal) < 1e-300) {
#endif
        cerr << "*** at time " << Global::time << ": impossible observations" << endl;
        throw runtime_error("Increment of loop variable didn't work properly");
    }
#if USE_LOG_PROBS || USE_LOG_WEIGHTS
    if (Global::wtotal.sum() == ZeroWeight) {
#else
    if (prob(Global::wtotal) < 1e-300) {
#endif
        cerr << "*** at time " << Global::time << ": impossible observations" << endl;
        throw runtime_error("Increment of loop variable didn't work properly");
    }

    createStateStatistics(o_states, particles);
    Global::loglik += p_logValue(Global::wtotal);

    // normalise particle weights, calculate wsq
    Sum<Weight> wsq;
    double neff;
    for (size_t i = 0; i < Global::nparticles; i++)  {
        Particle* p = &particles[i];
        Weight w = p->weight / Global::wtotal;
        p->weight = w;
        wsq += w*w;
    }

    // run and print estimators
    runEstimators(Global::time, estimators.begin(), estimators.end(), ItProxy(&particles[0]), ItProxy(&particles[Global::nparticles]));


    neff = p_value(Probability(1)/wsq);

#if VERBOSITY > 1
    clog << ": LL=" << Global::loglik
         << ", Sum(w)=" << Global::wtotal
         << ", Sum(w^2)=" << wsq
         << ", Neff=" << neff
         << '\n';
#endif

    // resample if necessary: check here for minimal neff

    if (neff < Global::nparticles * parameters.resample_threshold) {
#if VERBOSITY > 0
        clog << "*** at time " << Global::time << ": resampling (Neff=" << neff << ")";
        clog << '\n';
#endif
#if BENCH > 1
        Bench bench_res;
#endif
        resample(particles, resamples);
#if BENCH > 1
        cerr << "Bench: resample took " << bench_res.stop() << " ms\n";
#endif
        std::swap(particles, resamples);
    }

    Global::wtotal = ZeroWeight;
    return wsq;
}

void smoother(std::ostream &os, std::vector<Particle> &particles) {
    using std::swap;

#if !defined(USE_ACTION_ESTIMATION) && defined(HAS_ESTIMATOR)
    typedef std::vector<model::Estimate<Particle> > Estimates;
    // use the estimator of the model
    model::Estimate<Particle> e;
#else
    typedef std::vector<ActionEstimator<Particle, NOPS> > Estimates;
    // if the model has no estimator, do action estimation as default (or if we have been explicitly asked to do action estimation)
#ifdef USE_ACTION_ESTIMATION
    ActionEstimator<Particle, NOPS> e(USE_ACTION_ESTIMATION);
#else
    ActionEstimator<Particle, NOPS> e(2);
#endif
#endif
    // TODO XStateEstimator as given by -X

    Estimates es;

    Particle::State *d;
    double currentTime = Global::time;
    bool hasMoreTime = true;

    // we need to store the iterator to the current step for every particle
    std::vector<std::deque<Particle::State>::reverse_iterator> historyIterators(Global::nparticles);
    for (size_t i = 0; i < Global::nparticles; i++)
        historyIterators[i] = particles[i].getHistory().rbegin();

    e.printHeader(os);

    while (hasMoreTime) {
        double nextTime = -1;
        hasMoreTime = false;
        e.start(currentTime);

        for (size_t i = 0; i < Global::nparticles; i++) {
            auto &histIt = historyIterators[i];

            // find history slot for current time
            while (1) {
                if (histIt == particles[i].getHistory().rend())
                    throw runtime_error("(1) Missing data for smoothing estimation");
                d = &(*histIt);
                if (d->time <= currentTime)
                    break;
                histIt++;
            };

            // temporary set the right state in the particle
            swap(particles[i].state, *d);

            // collect data
            e.pre_collect(particles[i]);
            particles[i].doCallbacksAndObservations();
            e.collect(particles[i]);

            // swap back
            swap(particles[i].state, *d);

            // find next estimation time point
            if (d->time < currentTime) { // this particle has an earlier time point
                hasMoreTime = true;
                if (d->time > nextTime)
                    nextTime = d->time;
            } else { // d->time == currentTime: move to particle's next history element
                if (currentTime>0) {
                    histIt++;
                    if (histIt == particles[i].getHistory().rend())
                        throw runtime_error("(2) Missing data for smoothing estimation");
                    d = &(*histIt);
                    hasMoreTime = true;
                    if (d->time > nextTime)
                        nextTime = d->time;
                }
            }
        }
        e.finish();
        es.push_back(e);
        //clog << "currentTime: " << currentTime << ", nextTime: " << nextTime << '\n';

        if (parameters.smoothRegular && currentTime-nextTime > parameters.smoothStep)
            nextTime = currentTime - parameters.smoothStep;

        currentTime = nextTime;
    }

    for (Estimator<Particle> &est: es) {
        est.print(os);
    }
}


template <class Model>
int main(int argc, char *argv[]) {
    using boost::conversion::try_lexical_convert;

    int ndead, nrunning, ngoal;
    bool revisiting_factor_set = false;
    std::string fullXStateDistFile;
    std::string stateStatisticsFile;
    std::string o_smoothfile;
    std::string o_factor;
    std::string o_bias;
    std::shared_ptr<std::ofstream> o_states;

    util::array<double, Model::num_goals> goalWeights;
    util::array<double, Model::num_initials> initialStateWeights;

    for (int i = 0; i < NUM_GOALS; ++i)
      goalWeights[i] = -1.0;
    for (int i = 0; i < NUM_INITIAL_STATES; ++i)
      initialStateWeights[i] = -1.0;

    add_filter_params_struct s;

    options::option opts;

    bool use_actr_heuristics = false;
    #ifdef USE_ACTR_HEURISTICS
        use_actr_heuristics = true;
    #endif

    try {

        options::add_filter_params(opts, s, true, use_actr_heuristics);
        options::add_particle_filter(opts, stateStatisticsFile, fullXStateDistFile);
        options::add_random(opts);
        options::add_particle_params(opts, revisiting_factor_set, use_actr_heuristics);
        options::add_SMC(opts);

        opts.add("resample", "set the resampling threshold to <r> (0..1)")
            .parameter("r", parameters.resample_threshold,
                [](std::string, double r) { return r >= 0 && r <= 1; },
                true);

        opts.add("h", "set the current heuristic to <factor> (default=0: blind heuristic, 1: landmarks)")
            .parameter("factor", parameters.currentHeuristic);

        opts.add("goal-prior", "<goal:prob>", "sets the a-priori goal probability (#goals=" + boost::lexical_cast<std::string>(NUM_GOALS) + ")",
        [&](std::string, std::vector<std::string> params, size_t start) -> size_t {
            int goalI; std::string parameter;
            if (model::getGoalIndex(params[start], goalI, parameter))
                if (boost::conversion::detail::try_lexical_convert(parameter, goalWeights[goalI]))
                    return 1;
                else
                    throw std::runtime_error("Can't convert " + parameter + " to goal weight");
            else throw std::runtime_error(params[start] + "is not suitable for \"goal-prior\"");
        });

        opts.add("init-prior", "<istate:prob>", "sets the a-priori probability for initial states (#istates=" + std::to_string(NUM_INITIAL_STATES) + ")",
        [&](std::string, std::vector<std::string> in, size_t start) -> size_t {
            int isI;
            std::string parameter;
            if (!model::getInitialStateIndex(in[start], isI, parameter)) {
                throw std::runtime_error("");
            }
            if (!try_lexical_convert(parameter, initialStateWeights[isI])) {
                throw std::runtime_error("Option -i: illegal initial state weight " + parameter);
            }
            return 1;
        });

        opts.add("s", "save smoothed estimates to <smoothfile>")
            .parameter("smoothfile", o_smoothfile);

        opts.add("smooth-regular", "force regular smoothing with step size <value>")
            .parameter("value", parameters.smoothStep,
            [&](std::string, double) -> bool {
                parameters.smoothRegular = true;
                return true;
            }, true);

        opts.add("q", "calculates the probability of the next action by taking the <factor> root of the real probability")
            .parameter("factor", parameters.nroot);

        options::add_help_param(opts);

        if (argc - opts.parse(argc, argv) != 0) {
            std::cerr << "Too many arguments!\n";
            std::cerr << "Use --help to show all available options\n";
            return 1;
        }

    } catch(std::exception &e) {
        std::cerr << e.what() << '\n';
        std::cerr << "Use --help to show all available options\n";
        return 1;
    }

    if (Global::nparticles == 0) {
        std::cerr << "No particles to begin with! Set a non-zero number of particles\n";
        return 1;
    }

    if (revisiting_factor_set) // moved to here, because otherwise no "curently" value for the option can be shown
        parameters.revisitingFactor = log(parameters.revisitingFactor);

    if (!stateStatisticsFile.empty()) {
        o_states = std::make_shared<std::ofstream>(stateStatisticsFile);
    }

    // add estimators as configured by the various preprocessors macros
    // TODO: make this run-time options, allow to change output streams
    {
        std::shared_ptr<std::ostream> stream_p = std::shared_ptr<std::ostream>(&cout, util::empty_function<std::ostream*>());
#ifndef USE_ACTION_ESTIMATION
#ifdef HAS_ESTIMATOR
        estimators.emplace_back(std::make_shared<model::Estimate<Particle> >(), stream_p);
#endif
#else // USE_ACTION_ESTIMATION
        estimators.emplace_back(std::make_shared<ActionEstimator<Particle, NOPS> >(USE_ACTION_ESTIMATION), stream_p);
#endif // USE_ACTION_ESTIMATION
#ifdef USE_GOAL_ESTIMATION
        estimators.emplace_back(std::make_shared<GoalEstimator<Particle, NUM_GOALS> >(false), stream_p);
#endif

        if (!fullXStateDistFile.empty())
            // print full X-state distribution (this estimator ignores the ostream)
            estimators.emplace_back(std::make_shared<XStateEstimator<Particle>>(fullXStateDistFile), stream_p);
    }


#if VERBOSITY > 0
    clog << "Particle filter for domain: \"" << model::DOMAIN_NAME << '"'
         << ", problem: \"" << model::PROBLEM_NAME << '"'
         << ", " << model::magMode << " agent mode"
#ifdef USE_LOG_PROBS
         << ", using log probabilities"
#endif
         << '\n';
#endif

    States states;

    if (!s.allowedStatesFile.empty()) { // read states from stream
        ifstream i(s.allowedStatesFile.c_str());
        if (i.fail()) {
            cerr << "Cannot open file with allowed states " << s.allowedStatesFile << '\n';
            opts.print_help();
            exit(0);
        }
        size_t nrec = loadAllowedStates(states, i);
        std::cerr << "read " << nrec << " states\n";
    }

    if (!s.specificityFile.empty()) {
        StateFunctions::useProvidedSpecificity(StateFunctions::loadSpecificityTable(s.specificityFile.c_str()));
    }

    std::vector<Particle> particles(Global::nparticles);
    std::vector<Particle> resamples(Global::nparticles);

    try {

        for (int g=0; g < NUM_GOALS; g++) {
            if (s.statefiles[g].empty()) {
                // no provided
                continue;
            }

            ifstream sfile(s.statefiles[g].c_str());
            if (sfile.fail()) {
                cerr << "Cannot load state file from " << s.statefiles[g] << endl;
                exit(1);
            }
            int nr = loadStates(states, g, sfile); (void) nr;
            sfile.close();
#if VERBOSITY > 0
            clog << "Read " << nr << " state records from " << s.statefiles[g] << '\n';
#endif
        }

        normalize(goalWeights);
        normalize(initialStateWeights);

#if VERBOSITY > 1
        unsigned long rc1, rc = states.states.size();
        std::cerr << "Normalized goal weights:\n";
        for (size_t i = 0; i < goalWeights.size(); ++i){
            std::cerr << model::goalNames[i] << " : " << goalWeights[i] << '\n';
        }
        std::cerr << "Normalized initial state weights:" << '\n';
        for (size_t i = 0; i < initialStateWeights.size(); ++i){
            std::cerr << model::initialStateNames[i] << " : " << initialStateWeights[i] << '\n';
        }
#endif
        Global::time=0;
        Global::timeSteps.push_back(0);
        model::fetchObservation();

        { // init particles
            typename ::State<Model>::opid_t opid;
            for (size_t i = 0; i < Model::n_agents; i++)
                opid[i] = model::InitOpId;

            typename ::State<Model>::startTime_t startTime;
            for (size_t i = 0; i < Model::n_agents; i++)
                startTime[i] = Global::time;

            for (size_t i = 0; i < Global::nparticles; i++) {
                // sample an initial state and goal according to the weights
                int gdi = 0;
                double sampleW = randomval();
                Sum<double> sumw = 0.0;
                for (; gdi < NUM_GOALS; gdi++){
                    sumw += goalWeights[gdi];
                    if (sumw >= sampleW)
                        break;
                }
                sumw = 0.0;
                sampleW = randomval();
                int isi = 0;
                for (; isi < NUM_INITIAL_STATES; isi++){
                    sumw += initialStateWeights[isi];
                    if (sumw >= sampleW)
                        break;
                }

                model::StateRec xrec;
                model::sampleInitial(isi, &xrec);
                auto insResult = states.states.emplace(xrec, StateDataRec());
                StateTablePtr modelState = &(*insResult.first);

                ::State<Model> state(isi, gdi, modelState, opid, startTime);

                particles[i] = Particle(state, Weight(1.0/Global::nparticles));
                if (state.startTime[0] < 0)
                    return 1;
                if (particles[i].state.startTime[0] < 0)
                    return 1;

                Global::wtotal += particles[i].weight;
                // call the landmark heuristic also in the first step (path dependent)
                if (parameters.currentHeuristic == 1) {
                    landmarkHeuristic(states, *particles[0].getStatePtr(), 0);
                }
            }
        }

        if (o_states) {
            (*o_states) << "Time"
#if STATESTAT_NUM_PARTICLES
                        << " Particles"
#endif
#if STATESTAT_NUM_STATES
                        << " States"
#endif
#if STATESTAT_UNIQ_PARTICLES
                        << " ParticleStates"
#endif
#if STATESTAT_MIN_PX
                        << " minPx"
#endif
                        << '\n'
                        << std::setprecision(std::numeric_limits<double>::max_digits10);
        }

        for (auto &est: estimators)
            est.printHeader();

        normalizeAndEstimate(o_states.get(), particles, resamples);

#if BENCH > 0
        Bench bench_loop;
#endif
        for (/*double*/ int t=1; t<TimeSteps && model::fetchObservation(); t++) {
            Global::deltaT = t - Global::time;
            Global::time = t;
            Global::timeSteps.push_back(t);

#if VERBOSITY > 1
            rc1 = states.states.size();
            clog << "*** " << Global::time << " " << (rc1-rc) << '\n';
            rc = rc1;
#endif
#if BENCH > 1
            Bench bench;
#endif
            for (size_t i = 0; i < Global::nparticles; i++) {
                particles[i].step(states);
                Global::wtotal += particles[i].weight;
            }
#if BENCH > 1
            cerr << "Bench: predict took " << bench.reset() << " ms\n";
#endif

            normalizeAndEstimate(o_states.get(), particles, resamples);
#if BENCH > 1
            cerr << "Bench: normalize took " << bench.stop() << " ms\n";
#endif
#if BENCH > 0
            cerr << "Bench: one time-step took " << bench_loop.reset() << " ms\n";
#endif
        } // end for-loop

        if (!o_smoothfile.empty()) {
            ofstream sfile(o_smoothfile);
            smoother(sfile, particles);
            sfile.close();
            clog << "*** saved smoothed estimates to " << o_smoothfile << '\n';
        }

        clog << "*** Final log-likelihood: " << Global::loglik << '\n'
             << "    Finishing time:       " << Global::time << '\n';

#if ENDSTAT_REACH_STATES
        size_t reach = 0;
        for (auto &e: states.states) {
            if (e.second.visited)
                ++reach;
        }
        clog << "Reached " << reach << " states in total\n";
#endif

        ndead = nrunning = ngoal = 0;
        Probability maxw = ZeroProbability, maxsw = ZeroProbability;
        size_t maxi = -1;
        size_t maxsi = -1;
        for (size_t i = 0; i < Global::nparticles; i++) {
            Particle* p = &particles[i];

            // TODO: check if we want to show the most likely particle of each model
            if (prob(p->weight) > maxsw && p->state.isGoalState()) {
                maxsw=prob(p->weight);
                maxsi=i;
                if (maxsw >= maxw) {
                    maxw = maxsw;
                    maxi=i;
                }
            }
            if (prob(p->weight) > maxw) {
                maxw=prob(p->weight);
                maxi=i;
            }

            if (p->weight == ZeroWeight)
                ++ndead;
            else
                ++nrunning;
            if (p->state.isGoalState())
                ++ngoal;
        }

        clog << "Dead: " << ndead
             << ", running: " << nrunning
             << ", successful: " << ngoal
             << '\n';

        if (maxsi != static_cast<size_t>(-1)) {
            clog << "*** Best successful particle: " << maxsi << ", weight: " << maxsw << '\n';
            particles[maxsi].review();
        }
        if (maxi != static_cast<size_t>(-1) && maxi != maxsi) {
            clog << "*** Best surviving particle: " << maxi << ", weight: " << maxw << '\n';
            particles[maxi].review();
        }
    } catch (const std::exception& e) {
        cerr << "Exception: " << e.what() << endl;
        return 1;
    } catch (Particle *id) {
        id->review();
    }

    return 0;
}

} // namespace particle

int main(int argc, char** argv) {
    std::ios::sync_with_stdio(false);
    int result = particle::main<model::Model>(argc, argv);

    pool_allocator_base::free_all();

    return result;
}
