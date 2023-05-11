#define USE_ACTR_HEURISTICS
#define SIMULATOR

#include <iostream>
#include <fstream>
#include <memory>

#include "util/allocator.h"
#include "util/functional.h"
#include "util/parameter.hpp"

#include "config.h"
#include "global.h"
#include "particle.h"


namespace simulator {

int steps = 50;

using namespace std;

typedef particle::Particle Particle;

template <class Model>
int main(int argc, char** argv) {

    bool stopAtGoal = true;
    bool revisiting_factor_set = false;

    // Estimators
    std::shared_ptr<std::ostream> stream_p;
    // Estimators end

    int goalIndex = 0;
    int initialIndex = 0;
    std::string o_factor;
    std::shared_ptr<std::ostream> outstream = std::shared_ptr<std::ostream>(&cout, util::empty_function<std::ostream*>());

    // the list of all desired estimators
    std::vector<EstimateTarget<Particle> > estimators;

    add_filter_params_struct s;
    parameters.revisitingFactor = 0.0;

    bool use_actr_heuristics = false;
    #ifdef USE_ACTR_HEURISTICS
        use_actr_heuristics = true;
    #endif

    options::option opts;

    try {

        options::add_filter_params(opts, s, false, use_actr_heuristics);
        options::add_particle_params(opts, revisiting_factor_set, use_actr_heuristics);
        options::add_SMC(opts);

        opts.add("R", "<seed>", "set random seed for action selection",
        [&](std::string, std::vector<std::string> in, size_t start) -> size_t {
            Global::random->seed(atoi(in[start].c_str()));
            return 1;
        });

        opts.add("r", "", "set random seed to current time",
        [&](std::string, std::vector<std::string>, size_t) -> size_t {
            Global::random->seed(time(NULL));
            return 0;
        });

        opts.add("s", "number of plan steps (default:" +  std::to_string(steps) + " )")
            .parameter("value", steps);

        opts.add("g", "<value>", "set goal (index or name)",
        [&](std::string, std::vector<std::string> in, size_t start) -> size_t {
            try {
                goalIndex = boost::lexical_cast<int>(in[start]);
            } catch (std::exception &e) {
                for (goalIndex = 0; goalIndex < NUM_GOALS; goalIndex++) {
                    if (in[start] == model::goalNames[goalIndex]) {
                        break;
                    }
                }
            }
            if (goalIndex < 0 || goalIndex >= NUM_GOALS) {
                throw std::runtime_error("Goal out of bound: " + in[start] + "(" + std::to_string(goalIndex) + ")");
            }
            return 1;
        });

        opts.add("i", "<value>", "set initial state (index or name)",
        [&](std::string, std::vector<std::string> in, size_t start) -> size_t {
            try {
                initialIndex = boost::lexical_cast<int>(in[start]);
            } catch (std::exception &e) {
                for (initialIndex = 0; initialIndex < NUM_INITIAL_STATES; initialIndex++) {
                    if (in[start] == model::initialStateNames[initialIndex]) {
                        break;
                    }
                }
            }
            if (initialIndex < 0 || initialIndex >= NUM_INITIAL_STATES) {
                throw std::runtime_error("Initial State out of bound: " + in[start] + "(" + std::to_string(initialIndex) + ")");
            }
            return 1;
        });

        opts.add("G,no-stop-goal", "", "do not stop when a goal has been reached",
        [&](std::string, std::vector<std::string>, size_t) -> size_t {
            stopAtGoal = false;
            return 0;
        });

        opts.add("o", "write simulated action sequence to the file (default: stdout)")
            .parameter("file", outstream);

        // add estimators as configured by the various preprocessors macros
        // TODO: make this run-time options which estimator to choose
        opts.add("E", "write estimates to the file").parameter("file", stream_p,
        [&](std::string, std::shared_ptr<std::ostream> /*stream_p*/) -> bool {
            if (!estimators.empty()) {
                cerr << "Error: currently only one estimator is supported\n";
                return false;
            }
            return true;
        });

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

    States states;

    if (revisiting_factor_set) // moved to here, because otherwise no "curently" value for the option can be shown
        parameters.revisitingFactor = log(parameters.revisitingFactor);

    if (!s.allowedStatesFile.empty()) { // read states from stream
        ifstream i(s.allowedStatesFile.c_str());
        if (i.fail()) {
            cerr << "Cannot open file with allowed states " << s.allowedStatesFile << '\n';
            opts.print_help();
            exit(0);
        }
        size_t nrec = loadAllowedStates(states, i);
        std::cerr << "read " << nrec << " states" << '\n';
    }

    if (!s.o_statefile.empty()) {
        ifstream sfile(s.o_statefile);
        int nr;

        nr = loadStates(states, goalIndex, sfile);
        sfile.close();
        clog << "Read " << nr << " state records from " << s.o_statefile << '\n';
    }

    if (!s.specificityFile.empty()) {
        StateFunctions::useProvidedSpecificity(StateFunctions::loadSpecificityTable(s.specificityFile.c_str()));
    }
    
    if (stream_p) {
#ifndef USE_ACTION_ESTIMATION
#ifdef HAS_ESTIMATOR
        estimators.emplace_back(std::make_shared<model::Estimate<Particle> >(), stream_p);
#endif
#else // USE_ACTION_ESTIMATION
        estimators.emplace_back(std::make_shared<ActionEstimator<Particle, NOPS> >(USE_ACTION_ESTIMATION), stream_p);
#endif // USE_ACTION_ESTIMATION
#ifdef USE_GOAL_ESTIMATION
        estimators.emplace_back(std::make_shared<GoalEstimator<Particle, NUM_GOALS> >(true), stream_p);
#endif
    }

    for (auto &est: estimators)
        est.printHeader();

    Particle p;
    {
        typename Model::opid_t opid;
        for (size_t i = 0; i < Model::n_agents; i++)
            opid[i] = model::InitOpId;
        typename ::State<Model>::startTime_t startTime;
        for (size_t i = 0; i < Model::n_agents; i++)
            startTime[i] = Global::time;

        model::StateRec xrec;
        model::sampleInitial(initialIndex, &xrec);
        StateTablePtr modelState = &(*(states.states.emplace(xrec, StateDataRec()).first));

        ::State<Model> initState(initialIndex, goalIndex, modelState, opid, startTime);
        p = Particle(initState, Weight(1));
    }
    // runEstimators requires iterators over Particle* - therefore create a singleton array of Particle*
    Particle *pList[1] = {&p};

    (*outstream) << Global::time << ',' << (p.state.startTime[0] < Global::time ? "" : "*") << ',' << model::actions[p.state.opid[0]]->name << '\n';

    for (int i=1; i<steps; i++) {
        if (stopAtGoal && p.state.isGoalState()) {
            (*outstream) << i << ",*," << model::actions[model::FinishedOpId]->name << '\n';
            break;
        } else if (p.weight == ZeroWeight) {
            std::cerr << "Deadlock found\n";
            break;
        }

        Global::deltaT = i - Global::time;
        Global::time = i;
        Global::timeSteps.push_back(i);
        p.step(states);

        (*outstream) << Global::time << ',' << (p.state.startTime[0] < Global::time ? "" : "*") << ',' << model::actions[p.state.opid[0]]->name << '\n';

        // run and print estimators
        runEstimators(Global::time, estimators.begin(), estimators.end(), &pList[0], &pList[1]);
    }

    return 0;
}

} // namespace simulator


int main(int argc, char** argv) {
    std::ios::sync_with_stdio(false);
    int result = simulator::main<model::Model>(argc, argv);
    pool_allocator_base::free_all();
    return result;
}
