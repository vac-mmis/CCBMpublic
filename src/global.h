#pragma once

#include <memory>
#include <cmath>
#include <vector>

#include "util/random.h"
#include "util/summation.h"
#include "util/parameter.hpp"

#include "config.h"
#include "weight.h"
#include "state.h"

namespace FANN {
class neural_net;
}

const double infinity = HUGE_VAL;
const double epsilonT      =  0.1; // for computing the time point just before now
const double initialWeight =  0.0; // goal distance

// tuning parameters influencing filtering and heuristics
struct Parameters {
    double resample_threshold;

    double weightFactor; // wt = weightBias + exp(weightFactor * goalDistance)
    double weightBias;
    double revisitingFactor;
    bool smoothRegular;
    double smoothStep;
    double nroot;

    /* weights of the act-r heuristics */
    double weightSaliency;
    double weightSpecificity;
    double weightRecency;
    double weightRefractoriness;

    /* 0 - no goal distance, 1 - landmark heuristics, 2 - ann */
    // TODO: change to enum
    double currentHeuristic;

    Parameters();
};

extern Parameters parameters;

// why do we use a class for this??
class Global {
public:
    static size_t nparticles; // number of particles to use
    static double time; // global time
    static std::vector<double> timeSteps; // all time-steps so far
    static double deltaT; // time since last step;
    static Sum<Weight> wtotal; // total weight of particles

    static void *y; // current observation
    static Sum<double> loglik;
    static std::shared_ptr<Random> random;

    static RandomInt random_shuffle;

    static FANN::neural_net* pAnn;
};


double theLoglik();
double theTime();
double theDeltaT();
double randomval();

int sampleOp(Weight ssaliency, Weight *saliencies, int nappl);

size_t loadStates(States &states, size_t goalIndex, std::istream &i);
size_t loadAllowedStates(States &states, std::istream &i);

/*
 * normalizes the weights to sum to 1
 * - if there are no negative values, normalize as usual
 * - if sum of positive values is lower than one,
 *      negative values will be normalized so that the total sum is 1 (positive values do not change)
 * - if sum of positive values is higher equal than one,
 *      negative values will be replaced by 0 and positive values normalized
 *
 * values of 0 will not be changed
 *
 * I: some container of weights
 */
template <typename T>
void normalize(T &weights){
    // sum of positive weights
    Sum<double> sumw = 0.0;
    // sum of negative weights (which itself is negative)
    Sum<double> sum_negative = 0.0;

    // count total sum
    for (auto weight: weights) {
        if (weight > 0)
            sumw += weight;
        else if (weight < 0)
            sum_negative += weight;
    }

    // normalize
    if (sumw.sum() < 1.0 && sum_negative.sum() < 0) {
        // normalize negative values to sum to one (does not change positive values)
        double restW = 1.0 - sumw.sum();
        for (auto &weight: weights) {
            if (weight < 0)
                // weight and sum_negative are negative, restW is positive → weight will be positive
                weight *= restW / sum_negative.sum();
        }
    } else {
        // normalize positive values, ignore any negative values
        for (auto &weight: weights) {
             weight = (weight <= 0) ? 0 : weight/sumw.sum();
        }
    }
}

// parameter groups

namespace marginal {

    enum PruneStrategy {
        prune_beam,
        prune_beam_slow,
        prune_nearquick,
        prune_fc03,
        prune_optimal_proposal
    };

    struct options_t {
        // all user-defined goal weights
        std::map<int, double> goalWeights;
        // all user-defined initial state weights
        std::map<int, double> initialStateWeights;

        bool shuffleParticles;

        std::string viterbiFile;
        std::string smoothingFile;

        std::string stateStatisticsFile;
        std::string fullXStateDistFile;

        PruneStrategy prune_strategy;

        options_t () :
            shuffleParticles(false)
        { }
    }; // struct options_t
}

struct add_filter_params_struct {
    std::string o_statefile;
    std::string allowedStatesFile;
    std::string ANNFile;
    std::string specificityFile;
    std::string statefiles[NUM_GOALS];
};

namespace options {
void add_filter_params(option &o, add_filter_params_struct &s, bool more_states = false, bool use_actr_heuristics = true);

void add_particle_filter(option &o, std::string &stateStatisticsFile, std::string &fullXStateDistFile);

void add_random(option &o);

void add_particle_params(option &o, bool &revisiting_factor_set, bool use_actr_heuristics = true);

// TODO: add -b to hmm and merge add_SMC into add_filter_params
void add_SMC(option &o);

void add_help_param(option &o);

}
