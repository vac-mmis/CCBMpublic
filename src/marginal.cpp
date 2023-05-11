#include <iostream>
#include <fstream>
#include <map>
#include <memory>
#if TRAP_FE
#include <cfenv>
#endif

#include <boost/lexical_cast.hpp>

#define HAS_MARGINAL_FILTER 1

#include "util/allocator.h"
#include "util/functional.h"
#include "util/summation.h"

#include "util/parameter.hpp"

#include "config.h"
#include "global.h"
#include "fann_wrapper.h"
#include "obsmodel.h"

#include "estimate/estimators.h"

#include "marginal/filter.h"
#include "marginal/smoothing.h"
#include "marginal/viterbi.h"
#include "marginal/particle_single.h"
#include "marginal/particle_single_sequential.h"
#include "marginal/particle_startTimes.h"
#include "marginal/particle_obsEquiv.h"
#include "marginal/prune.h"

// check selection of pruning strategy (see their definitions in prune.h for details)
#define MARGINAL_PRUNE_BEAM prune_beam
#define MARGINAL_PRUNE_BEAM_SLOW prune_beam_slow
#define MARGINAL_PRUNE_NEAR_QUICK prune_nearquick
#define MARGINAL_PRUNE_PF_OPTIMAL_PROPOSAL prune_optimal_proposal
#define MARGINAL_PRUNE_FC03 prune_fc03

#ifndef MARGINAL_PRUNE
#define MARGINAL_PRUNE MARGINAL_PRUNE_BEAM
#endif


using namespace std;
namespace marginal {


template <class Particle>
int run(const options_t &options, const typename Particle::Options &p_options, States &states) {
    typedef smoothing::Smoothing<Particle> Smoothing;
    typedef viterbi::Viterbi<Particle> Viterbi;

    Filter<Particle> filter(states);

    filter.setParticleOptions(p_options);
    filter.setInitialStateWeights(options.initialStateWeights);
    filter.setGoalWeights(options.goalWeights);
    filter.setShuffleParticles(options.shuffleParticles);
    filter.setPrune(Prune<Particle>::construct(options.prune_strategy));

    if (!options.stateStatisticsFile.empty())
        filter.setStateStatisticsStream(std::make_shared<ofstream>(options.stateStatisticsFile.c_str()));


    std::shared_ptr<Viterbi> viterbi;
    std::vector<EstimateTarget<Particle> > viterbi_estimateTargets;

    // add estimators as configured by the various preprocessors macros
    // TODO: make this run-time options, allow to change output streams
    {
        std::shared_ptr<std::ostream> stream_p = std::shared_ptr<std::ostream>(&cout, util::empty_function<std::ostream*>());
#ifndef USE_ACTION_ESTIMATION
#ifdef HAS_ESTIMATOR
        filter.addEstimateTarget(std::make_shared<model::Estimate<Particle> >(), stream_p);
#endif
#else // USE_ACTION_ESTIMATION
        filter.addEstimateTarget(std::make_shared<ActionEstimator<Particle, NOPS> >(USE_ACTION_ESTIMATION), stream_p);
#endif // USE_ACTION_ESTIMATION
#ifdef USE_GOAL_ESTIMATION
        filter.addEstimateTarget(std::make_shared<GoalEstimator<Particle, NUM_GOALS> >(false), stream_p);
#endif

        // TODO control this separately for smoothing
        if (!options.fullXStateDistFile.empty())
            // print full X-state distribution (this estimator ignores the ostream)
            filter.addEstimateTarget(std::make_shared<XStateEstimator<Particle>>(options.fullXStateDistFile), stream_p);
    }

    if (!options.viterbiFile.empty()) {
        viterbi = std::make_shared<Viterbi>();
        filter.setViterbi(viterbi);

        std::shared_ptr<std::ostream> stream_p;
        if (options.viterbiFile == "-")
            // provide a deleter to shared_ptr that does nothing and keeps cout intact
            stream_p = std::shared_ptr<std::ostream>(&cout, util::empty_function<std::ostream*>());
        else
            // create a new stream
            stream_p = std::make_shared<std::ofstream>(options.viterbiFile.c_str());

        // create estimator
        std::shared_ptr<Estimator<Particle> > est;

#if !defined(USE_ACTION_ESTIMATION) && defined(HAS_ESTIMATOR)
        // use the estimator of the model
        est = std::make_shared<model::Estimate<Particle> >();
#else
        // if the model has no estimator, do action estimation as default (or if we have been explicitly asked to do action estimation)
#ifdef USE_ACTION_ESTIMATION
        est = std::make_shared<ActionEstimator<Particle, NOPS> >(USE_ACTION_ESTIMATION != 0);
#else
        est = std::make_shared<ActionEstimator<Particle, NOPS> >(2);
#endif
#endif
        viterbi_estimateTargets.emplace_back(std::move(est), stream_p);

        // TODO control this separately for Viterbi
        if (!options.fullXStateDistFile.empty())
            // print full X-state distribution (this estimator ignores the ostream)
            viterbi_estimateTargets.emplace_back(std::make_shared<XStateEstimator<Particle>>(options.fullXStateDistFile + "viterbi"), stream_p);
    }


    std::shared_ptr<Smoothing> smoothing;
    std::vector<EstimateTarget<Particle> > smoothing_estimateTargets;

    if (!options.smoothingFile.empty()) {
        smoothing = std::make_shared<Smoothing>();
        filter.setSmoothing(smoothing);

        std::shared_ptr<std::ostream> stream_p;
        if (options.smoothingFile == "-")
            // provide a deleter to shared_ptr that does nothing and keeps cout intact
            stream_p = std::shared_ptr<std::ostream>(&cout, util::empty_function<std::ostream*>());
        else
            // create a new stream
            stream_p = std::make_shared<std::ofstream>(options.smoothingFile.c_str());

        std::shared_ptr<Estimator<Particle> > est;
        // create estimator
#if !defined(USE_ACTION_ESTIMATION) && defined(HAS_ESTIMATOR)
        // use the estimator of the model
        est = std::make_shared<model::Estimate<Particle> >();
#else
        // if the model has no estimator, do action estimation as default (or if we have been explicitly asked to do action estimation)
#ifdef USE_ACTION_ESTIMATION
        est = std::make_shared<ActionEstimator<Particle, NOPS> >(USE_ACTION_ESTIMATION != 0);
#else
        est = std::make_shared<ActionEstimator<Particle, NOPS> >(2);
#endif
#endif
        smoothing_estimateTargets.emplace_back(std::move(est), stream_p);

        if (!options.fullXStateDistFile.empty())
            // print full X-state distribution (this estimator ignores the ostream)
            smoothing_estimateTargets.emplace_back(std::make_shared<XStateEstimator<Particle>>(options.fullXStateDistFile + "smooth"), stream_p);
    }


    // do the actual filter run
    filter.run();


    // post-processing the filtered data

    if (smoothing) {
        // run smoothing algorithm
        std::shared_ptr< std::stack<typename Particle::template ParticleSet<> > > smoothed = smoothing->doSmoothing(states);

        for (auto &est: smoothing_estimateTargets)
            est.printHeader();

        // run estimate on smoothed particles
        for (size_t timeStep = 0; timeStep < Global::timeSteps.size(); ++timeStep) {
            Global::time = Global::timeSteps[timeStep];

            typename Particle::template ParticleSet<> &curSmoothed = smoothed->top();
            // smoothed will be pop'ed at the end, otherwise the reference becomes invalid

            runEstimators(Global::time, smoothing_estimateTargets.begin(), smoothing_estimateTargets.end(), curSmoothed.begin(), curSmoothed.end());

            // the particles are no longer needed
            for (Particle *p: curSmoothed) {
                delete p;
            }

            smoothed->pop();
        }
    }

    if (viterbi) {
        // run Viterbi
        std::pair<std::deque<std::shared_ptr<Particle> >, LogProbability> path = viterbi->doViterbiRun(states);

        for (auto &est: viterbi_estimateTargets)
            est.printHeader();

        // print estimates
        typename std::deque<std::shared_ptr<Particle> >::iterator particleNext = path.first.begin();
        for (auto time: Global::timeSteps) {
            typename std::deque<std::shared_ptr<Particle> >::iterator particleIt = particleNext;
            ++particleNext;
            // estimate the next single particle
            runEstimators(time, viterbi_estimateTargets.begin(), viterbi_estimateTargets.end(), particleIt, particleNext);
        }
    }


    return 0;
} // function run()


int main(int argc, char *argv[]) {

    using boost::conversion::try_lexical_convert;

    enum { Particle_single, Particle_sequential, Particle_startTimes, Particle_set } particle_class = Particle_sequential;
    // optional string of particle-specific options, given after "-P" option
    std::string particle_options;

    add_filter_params_struct s;

    // TODO make this run-time options
    options_t options;
    options.prune_strategy = MARGINAL_PRUNE;

    options::option opts;

    try {

        options::add_filter_params(opts, s, true);
        options::add_particle_filter(opts, options.stateStatisticsFile, options.fullXStateDistFile);
        options::add_random(opts);
        options::add_SMC(opts);

        opts.add("h", "set the current heuristic to <factor> (default=0: blind heuristic, 1: landmarks, 2: ann)")
            .parameter("factor", parameters.currentHeuristic);

        opts.add("goal-prior", "<goal:prob>", "sets the a-priori goal probability (#goals=" + boost::lexical_cast<std::string>(NUM_GOALS) + ")",
        [&](std::string, std::vector<std::string> params, size_t start) -> size_t {
            int goalI; std::string parameter;
            if (model::getGoalIndex(params[start], goalI, parameter))
                if (boost::conversion::detail::try_lexical_convert(parameter, options.goalWeights[goalI]))
                    return 1;
                else
                    throw std::runtime_error("Can't convert " + parameter + " to goal weight");
            else throw std::runtime_error(params[start] + "is not suitable for \"goal-prior\"");
        });

        opts.add("init-prior", "<istate:prob>", "sets the a-priori probability for initial states (#istates=" + boost::lexical_cast<std::string>(NUM_INITIAL_STATES) + ")",
        [&](std::string, std::vector<std::string> params, size_t start) -> size_t {
            int IsI; std::string parameter;
            if (model::getInitialStateIndex(params[start], IsI, parameter))
                if (try_lexical_convert(parameter, options.initialStateWeights[IsI]))
                    return 1;
                else
                    throw std::runtime_error("Can't convert " + parameter + " to initial state weight");
            else throw std::runtime_error(params[start] + "is not suitable for \"init-prior\"");
        });

        opts.add("q", "calculates the probability of the next action by taking the <factor> root of the real probability")
            .parameter("factor", parameters.nroot);

        opts.add("S", "<file>", "runs smoothing after forward filtering and saves estimates to <file>",
        [&](std::string, std::vector<std::string> in, size_t start) -> size_t {
            if(options.smoothingFile.empty()) {
                options.smoothingFile = in[start];
                return 1;
            }
            throw std::runtime_error("Error: currently only one smoothing estimator is supported\n");
        });

        opts.add("V", "<file>", "runs viterbi after forward filtering and saves estimates to <file>",
        [&](std::string, std::vector<std::string> in, size_t start) -> size_t {
            if(options.viterbiFile.empty()){
                options.viterbiFile = in[start];
                return 1;
            }
            throw std::runtime_error("Error: currently only one Viterbi estimator is supported\n");
        });

        opts.add("P", "[SsTO]", "Use an alternative particle representation (s sequential (default), S single, T starting times, O observation-equivalent)",
        [&](std::string, std::vector<std::string> in, size_t start) -> size_t {
            if (in[start] == "S") particle_class = Particle_single;
            else if (in[start] == "s") particle_class = Particle_sequential;
            else if (in[start] == "T") particle_class = Particle_startTimes;
            else if (in[start] == "O") particle_class = Particle_set;
            else throw std::runtime_error("Unknown option " + in[start] + ": Use one of SsTO\n"+
            "\
    s (sequential): prune after every agent - faster [default]\n\
    S (single): only prune after the last agent - considerably increases temporary number of particles\n\
    T (starting times): approximate all starting times with a single particle (might be faster)\n\
    O (observation equivalent): represent all obs-equiv. states in one particle (very slow, no advantage, experimental)");
            if (in.size() > start+1 && in[start+1][0] != '-') {
                particle_options = in[start+1];
                return 2;
            }
            return 1;
        });

        opts.add("shuffle", "<seed>", "shuffle the particles prior to pruning",
        [&](std::string, std::vector<std::string> in, size_t start) -> size_t {
            try {
                Global::random_shuffle.seed(boost::lexical_cast<int>(in[start]));
                options.shuffleParticles = true;
                return 1;
            } catch (std::exception &e){
                throw std::runtime_error("Can't convert " + in[start] + " to int");
            }
        });

        options::add_help_param(opts);

        if (argc - opts.parse(argc, argv) != 0) {
            std::cerr << "Too many arguments!" << std::endl;
            std::cerr << "Use --help to show all available options\n";
            return 1;
        }

    } catch(std::exception &e) {
        std::cerr << e.what() << std::endl;
        std::cerr << "Use --help to show all available options\n";
        return 1;
    }


    if (parameters.currentHeuristic == 2) {
        if (s.ANNFile.empty()) {
            cerr << "Neural network action selection can only be used if network is specified." << endl;
            return 1;
        } else {
            Global::pAnn = new FANN::neural_net();
            if (!Global::pAnn->create_from_file(s.ANNFile)) {
                cerr << "Unable to create neural network from file " << s.ANNFile << endl;
                delete Global::pAnn;
                Global::pAnn = NULL;
                return 1;
            }
        }
    }

    States states;

    if (!s.allowedStatesFile.empty()) { // read states from stream
        ifstream i(s.allowedStatesFile.c_str());
        if (i.fail()) {
            cerr << "Cannot open file with allowed states " << s.allowedStatesFile << endl;
            exit(0);
        }
        size_t nrec = loadAllowedStates(states, i);
        std::cerr << "read " << nrec << " states" << std::endl;
    }

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
        clog << "Read " << nr << " state records from " << s.statefiles[g] << endl;
#endif

    }



    switch (particle_class) {
        case Particle_single: {
            particle::single::Particle<model::Model>::Options p_options;
            return run<particle::single::Particle<model::Model> >(options, p_options, states);
        }
        case Particle_sequential: {
            particle::sequential::Particle<model::Model>::Options p_options;
            p_options.prune = Prune<particle::sequential::Particle<model::Model>>::construct(options.prune_strategy);
            if (!particle_options.empty()) {
                p_options.prune_agent_increasing = particle_options != "0";
            }
            return run<particle::sequential::Particle<model::Model> >(options, p_options, states);
        }
        case Particle_startTimes: {
            // parse particle-specific options
            // TODO generalise this, e.g. let each particle parse the command line after the -P option
            particle::startTimes::Particle<model::Model>::Options p_options;
            if (!particle_options.empty()) {
                std::istringstream opt_stream(particle_options);
                opt_stream >> p_options.min_points >> p_options.split_error >> p_options.split_early_error >> p_options.merge_error >> p_options.merge_exp >> p_options.merge_a >> p_options.min_logWeight;
            }
            return run<particle::startTimes::Particle<model::Model> >(options, p_options, states);
        }
        case Particle_set: {
            particle::obsEquiv::Particle<model::Model>::Options p_options;
            if (!SUPPORTS_OBS_EQUIV)
                cerr << "Notice: the model does not support observation-equivalent states. Using a set representation will not have any advantage." << endl;
            return run<particle::obsEquiv::Particle<model::Model> >(options, p_options, states);
        }
        default:
            cerr << "Unknown particle class " << particle_class << endl;
            return 1;
    }
} // marginal::main



} // namespace marginal

int main(int argc, char** argv) {
#if TRAP_FE
    feenableexcept(FE_INVALID);
#endif
    std::ios::sync_with_stdio(false);

    int result = marginal::main(argc, argv);

    pool_allocator_base::free_all();

    return result;
}
