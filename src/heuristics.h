#pragma once

#include "model.h"

/* approximate the goal distance with the help of landmarks and orderings.
 * this method does not compute the landmark graph. it is using the generated code
 * of the ruleCompiler */
void landmarkHeuristic(States &states, const model::StateRec &currentStatePtr, model::StatePtr parentAccepted);
