#pragma once

#include <vector>

#include "model.h"
#include "state.h"

namespace StateFunctions {

void useProvidedSpecificity(const std::vector<double>& specificityTable);
std::vector<double> loadSpecificityTable(const char* fileName);
}

void expandState(States &states, int agid, StateTablePtr gptr);
double applyAction(int opi, model::StatePtr current, model::StatePtr next, double* specificity);
double saliencyFunction(double weight);
double specificityFunction(double specificity);

inline bool isGoalState(size_t goalIndex, StateTablePtr p) {
    return model::isGoalState(goalIndex, &p->first);
}
