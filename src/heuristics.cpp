#include "global.h"
#include "state.h"
#include "heuristics.h"

/* approximate the goal distance with the help of landmarks and orderings.
 * this method does not compute the landmark graph. it is using the generated code
 * of the ruleCompiler */
void landmarkHeuristic(States &states, const model::StateRec &currentState, model::StatePtr parentAccepted) {

    // check if the value was already computed
    StateTablePtr ptr;
    auto ptrIt = states.states.find(currentState);

    if (ptrIt == states.states.end()) {
        auto insResult = states.states.emplace(currentState, StateDataRec());
        ptr = &(*(insResult.first));
    } else
        ptr = &(*ptrIt);

    StateDataRec &d = ptr->second;

    if (parentAccepted != 0) {
        d.accepted = *parentAccepted;
    }

    // compute landmarks for each goal independently
    for (int goalIndex = 0; goalIndex < NUM_GOALS; goalIndex++) {
        int goalDistance = 0;
        if (!model::isGoalState(goalIndex, &currentState))
            goalDistance = model::landmarkCountHeuristic(0, goalIndex, &currentState, &d.accepted);
        d.heuristicWeight[goalIndex] = (parameters.weightFactor == 0) ? 0 : parameters.weightFactor * goalDistance;
    }
}
