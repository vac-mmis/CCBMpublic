#include <vector>
#include <stdexcept>
#include <fstream>

#include "config.h"
#include "stateFunctions.h"
#include "global.h"

#if !HAS_STATE_SPACE_ANALYZER
#include "heuristics.h"
#include "fann_wrapper.h"
#endif

namespace StateFunctions {

bool specificityProvided;
std::vector<double> specificityTable;

void useProvidedSpecificity(const std::vector<double>& specificityTable) {
    if (specificityTable.size() < NOPS) {
        throw std::runtime_error("Specificity table does not contain specificites for all actions!");
    }
    specificityProvided = true;
    StateFunctions::specificityTable = specificityTable;
}
std::vector<double> loadSpecificityTable(const char* fileName) {
    std::ifstream specFile(fileName);
    if (!specFile.is_open()) {
        throw std::runtime_error(std::string("Can't open file: ") + fileName);
    }
    int totalStates;
    std::string line;
    std::getline(specFile,line,';');
    std::getline(specFile,line,';');
    specFile >> totalStates;
    std::getline(specFile,line);
    std::vector<double> specificityTable(NOPS);
    for (int i = 0; i < NOPS; ++i) {
        int applicableCount;
        int opId;
        specFile >> opId;
        std::getline(specFile,line,':');
        std::getline(specFile,line,':');
        specFile >> applicableCount;
        std::getline(specFile,line);
        specificityTable[opId] = 1 - (applicableCount/static_cast<double>(totalStates));
    }
    return specificityTable;
}
}

double applyAction(int opi, model::StatePtr current, model::StateRec *next, double* specificity) {
    double spec = 0.0;
    double res = model::actions[opi]->call(current,next,&spec);
    *specificity = spec;
    return res;
}

double saliencyFunction(double weight) {
    return (parameters.weightSaliency==0) ? 0 : parameters.weightSaliency * log(weight + 1);
}

double specificityFunction(double specificity) {
    return (parameters.weightSpecificity==0) ? 0 : parameters.weightSpecificity * log(specificity + 1);
}



void expandState(States &states, int agid, StateTablePtr gptr) {
    StateDataRec &d = gptr->second;

    if (d.appl[agid] >= 0)
        // state already expanded
        return;

    const model::StateRec &x = gptr->first;
    ApplInfo appis[NOPS];
    model::StateRec x1rec;
    int nappl = 0;

#if !HAS_STATE_SPACE_ANALYZER
    for (int aopi = 0; aopi < model::nOpsForAgent[agid]; aopi++) {
        int opi = model::opsForAgent[agid][aopi];
#else
    // the state-space analyzer must loop over all actions regardless of the agent
    // looping over all agents separately would be possible, but much slower
    assert(agid == 0); // analyzer should always call with agid=0
    for (int opi=0; opi < NOPS; opi++) {
#endif
        ApplInfo &api = appis[nappl];

        double spec = 0.0;
        double saliency = applyAction(opi, &x, &x1rec, &spec);

        // if there is a set of allowed states, all others are "forbidden"
        if (states.allowedStates != NULL && saliency > 0 && states.allowedStates->count(x1rec) == 0) {
            // skip unknown states
            saliency = 0;
        }

#if !HAS_STATE_SPACE_ANALYZER
        api.saliency = saliency;
#endif

        if (saliency <= 0)
            continue;

        auto insResult = states.states.emplace(x1rec, StateDataRec());
        api.reached = &(*(insResult.first));


#if !HAS_STATE_SPACE_ANALYZER
        api.opid = opi;
        if (!StateFunctions::specificityProvided) {
            api.specificity = spec;
        } else {
            api.specificity = StateFunctions::specificityTable[opi];
        }

#ifdef USE_ACTR_HEURISTICS
        api.wSaliency = saliencyFunction(api.saliency);
        api.wSpecificity = specificityFunction(api.specificity);
        if (isnan(api.saliency)) {
            std::cerr << "Error in expandState: probability of action(" << opi << ") is NaN." << std::endl;
            throw std::runtime_error("NaN in expandState");
        }
#else // if we do not use the ACT-R things, we should reset the prior prob
        api.wSaliency = 0.0;
        api.wSpecificity = 0.0;
#endif // USE_ACTR_HEURISTICS

        if (parameters.currentHeuristic == 1) {
            // use the landmark heuristic (the landmark heuristic is path dependent,
            // but it is also possible computing it only once, because it is a approximation (see richter thesis)
            landmarkHeuristic(states, x1rec, &d.accepted);
            for (int goalIndex = 0; goalIndex < NUM_GOALS; goalIndex++)
                api.wGoalDistance[goalIndex] = api.reached->second.heuristicWeight[goalIndex];
        } else if (parameters.currentHeuristic == 2) {
            // use ANN
            if (NULL == Global::pAnn) {
                std::cerr << "Cannot use neural network for goal distance. No network loaded." << std::endl;
                throw std::runtime_error("Missing Neural Network");
            }
            fann_type* output;
            fann_type input[ELEMENTS_UNIVERSE];
            std::stringstream statebits;

            // FIXME: this is the slowest possible way to get the state bits
            model::writeState(statebits, &x1rec, 0);
            int opid, step, distance;
            statebits >> opid >> step >> distance;
            // create data in correct format
            for (int i=0; i<ELEMENTS_UNIVERSE; i++) {
                int a;
                statebits >> a;
                input[i] = a;
            }

            // TODO: load multiple anns
            for (int goalIndex = 0; goalIndex < NUM_GOALS; goalIndex++) {
                Global::pAnn->scale_input(input);
                output = Global::pAnn->run(input);
                Global::pAnn->descale_output(output);

                // we use ceil here, because it went good in initial tests
                int gd = ceil(output[0]);
                // XXX: why this condition?
                api.wGoalDistance[goalIndex] = (parameters.weightFactor == 0) ? 0 : parameters.weightFactor * gd;
            }
        } else {// don't use the heuristic
            for (int goalIndex = 0; goalIndex < NUM_GOALS; goalIndex++) {
                StateWeightMap::iterator weight = states.weightMap[goalIndex].find(x1rec);
                if (weight == states.weightMap[goalIndex].end())
                    api.wGoalDistance[goalIndex] = initialWeight;
                else
                    api.wGoalDistance[goalIndex] = weight->second;
            }
        }

        for (int goalIndex = 0; goalIndex < NUM_GOALS; goalIndex++) {
            api.weight[goalIndex] = api.wSaliency + api.wSpecificity + api.wGoalDistance[goalIndex];
        }

#endif // !HAS_STATE_SPACE_ANALYZER

        nappl++;
    }

    d.appl[agid] = nappl;
    if (nappl>0) { // something interesting ...
        d.appi[agid] = new ApplInfo[nappl];
        for (int i=0; i<nappl; i++) {
            d.appi[agid][i] = appis[i];
        }
    }
}
