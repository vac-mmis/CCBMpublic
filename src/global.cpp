#include "global.h"

#include <cmath>

#include <boost/lexical_cast.hpp>

#include "util/summation.h"

#include "model.h"
#include "state.h"

// parameters

Parameters::Parameters() :
    resample_threshold(0.2),
    weightFactor(-1.0), // wt = weightBias + exp(weightFactor * goalDistance)
    weightBias(0.0), revisitingFactor(-infinity),
    smoothRegular(false), smoothStep(1.0),
    nroot(1),

    /* weights of the act-r heuristics */
    weightSaliency(0.0), weightSpecificity(0.0), weightRecency(0.0), weightRefractoriness(0.0),

    /* 0 - no goal distance, 1 - landmark heuristics */
    currentHeuristic(0) {}

Parameters parameters;

// define static fields of class Global

#ifdef NPARTICLES
size_t Global::nparticles = NPARTICLES;
#else
size_t Global::nparticles = 10000;
#endif
double Global::time = 0;
std::vector<double> Global::timeSteps;
double Global::deltaT = 0;
Sum<Weight> Global::wtotal = ZeroWeight;
Sum<double> Global::loglik = 0;
#ifdef MERSENNE
std::shared_ptr<Random> Global::random = std::make_shared<RandomM>();
#else
std::shared_ptr<Random> Global::random = std::make_shared<Random48>();
#endif // MERSENNE
RandomInt Global::random_shuffle;
FANN::neural_net* Global::pAnn = NULL;


// define functions


double theLoglik() {
    return Global::loglik;
}

double theTime() {
    return Global::time;
}
double theDeltaT() {
    return Global::deltaT;
}
double randomval() {
    return Global::random->random();
}


// define some other functions
// may be moved to another file

int sampleOp(Weight ssaliency, Weight *saliencies, int nappl) {
    using std::isinf;
    using std::isnan;

    int index = 0;
    Probability saliency;

    // sample according to model
    saliency = prob(ssaliency);

    if (isinf(saliency))
        throw std::logic_error("sampleOp: infinite weight");
    if (isnan(saliency))
        throw std::logic_error("sampleOp: nan weight");
    if (saliency <= ZeroProbability) {
        std::cerr << "sampleOp: non-positive weight: " << saliency << std::endl;
        throw std::logic_error("sampleOp: non-positive weight");
    }
    Probability target = saliency * Probability(randomval());
    //TODO: maybe choose another one
    while (target >= prob(saliencies[index])) {
        index++;
        if (index >= nappl)
            throw std::runtime_error("sampleOp: target exceeds weights");
    }
    return index;
}


size_t loadStates(States &states, size_t goalIndex, std::istream &i) {
    model::StateRec key;
    StateDataRec data;
    size_t nrecs = 0;
    char tag[50], domain[50], problem[50];
    size_t staterec_s, double_s;
    double wt;

    i >> tag >> domain >> problem >> staterec_s >> double_s;
    char dummy[1];
    i.getline(dummy, 1); // read new-line character, what follows are the states

    if (strcmp(tag,"RCStateTable") || strcmp(domain,model::DOMAIN_NAME) || strcmp(problem,model::PROBLEM_NAME)
            || staterec_s != sizeof(model::StateRec) || double_s != sizeof(double)) {
        std::cerr << "Expected: " << "RCStateTable" << ' ' << model::DOMAIN_NAME << ' ' << model::PROBLEM_NAME
                  << ' ' << sizeof(model::StateRec) << ' ' << sizeof(double) << '\n'
                  << "     Got: " << tag << ' ' << domain << ' ' << problem << ' ' << staterec_s << ' ' << double_s << std::endl;
        throw std::logic_error("Not a statefile for this filter");
    }
    while (model::readStateBin(i,&key,wt)) {
        nrecs ++;
        double weight = (parameters.weightFactor == 0) ? 0 : parameters.weightFactor * wt; // changed to the new weight formula (see tech-report)
        if (!states.weightMap[goalIndex].insert(std::make_pair(key, weight)).second) {
            std::cerr << "Duplicate state in statefile. Record="<<nrecs<<", state=" << &key << std::endl;
            throw std::logic_error("Duplicate state in statefile");
        }
    }

    return nrecs;
}

size_t loadAllowedStates(States &states, std::istream &i) {
    if (states.allowedStates != NULL)
        delete states.allowedStates;

    model::StateRec srec;
    double wt;
    states.allowedStates = new StateTable();
    size_t nrec = 0;
    while (model::readState(i,&srec,wt)) {
        states.allowedStates->emplace(srec, StateDataRec());
        nrec++;
    }

    return nrec;
}

namespace options {
void add_filter_params(option &o, add_filter_params_struct &s, bool more_states, bool use_actr_heuristics) {
    if (more_states) {
        o.add("l", "<statefile>", "load states and goal distances from <statefile>",
        [&](std::string, std::vector<std::string> in, size_t start) -> size_t {
            if (!model::tableName(in[start], s.statefiles)) {
                throw std::runtime_error(in [start] + " is not a statefile for \"-l\"");
            }
            return 1;
        });
    } else {
        o.add("l", "load states and goal distances from <statefile>")
            .parameter("statefile", s.o_statefile);
    }

    if (use_actr_heuristics) {

        o.add("specificity", "load specificity table from <file>")
            .parameter("file", s.specificityFile);

        o.add("A", "set weight factor for the saliency to <factor>")
            .parameter("factor", parameters.weightSaliency, true);

        o.add("B", "set weight factor for the specificity to <factor>")
            .parameter("factor", parameters.weightSpecificity, true);

    }

    o.add("restrict-states", "restrict states to be visited to states in file")
        .parameter("filename", s.allowedStatesFile);

    o.add("f", "set weight factor to <factor>")
        .parameter("factor", parameters.weightFactor, true);

    o.add("load-ann", "<file>", "loads trained neural network for action selection (created with train tool)",
    [&](std::string, std::vector<std::string> in, size_t start) -> size_t {
        s.ANNFile = in[start]; parameters.currentHeuristic = 2;
        return 1;
    });
}

void add_particle_filter(option &o, std::string &stateStatisticsFile, std::string &fullXStateDistFile) {
    o.add("p", "saves states distribution for all particles")
        .parameter("statesfile", stateStatisticsFile);

    o.add("X", "saves full X-State distribution for all particles in <distfile><t>.gz, where <t> is the time-step")
        .parameter("distfile", fullXStateDistFile);
}

void add_random(option &o) {
    o.add("r", "", "set random seed to current time",
    [](std::string, std::vector<std::string>, size_t) -> size_t {
        Global::random->seed(time(NULL));
        return 0;
    });

    o.add("R", "<seed>", "set random seet to <seed>",
    [](std::string, std::vector<std::string> in, size_t start) -> size_t {
        try {
            Global::random->seed(boost::lexical_cast<int>(in[start]));
            return 1;
        } catch (std::exception &e){
            throw std::runtime_error(in[start] + " is not a valid random seed number!");
        }
    });
}

void add_particle_params(option &o, bool &revisiting_factor_set, bool use_actr_heuristics) {

    o.add("v", "set revisiting factor to <factor>")
        .parameter("factor", parameters.revisitingFactor,
        [&](std::string, double in) -> bool {
            revisiting_factor_set = true;
            if (in < 0)
                return false;
            return true;
        }, true);

    if (use_actr_heuristics) {
        o.add("C", "set weight factor for the recency to <factor>")
            .parameter("factor", parameters.weightRecency, true);

        o.add("D", "set weight factor for the refractoriness to <factor>")
            .parameter("factor", parameters.weightRefractoriness, true);
    }
}

void add_SMC(option &o) {
    o.add("b", "set weight bias to <bias>")
        .parameter("bias", parameters.weightBias, true);
    o.add("N,nparticles", "set the number of particles to use to <N>")
        .parameter("N", Global::nparticles, true);
}

void add_help_param(option &o) {
    o.add("?,help", "", "prints this screen",
        [&](std::string, std::vector<std::string>, size_t) -> size_t {
            o.print_help();
            exit(0);
        });
}

}
