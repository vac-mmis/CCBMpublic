#include "state.h"

#if !HAS_STATE_SPACE_ANALYZER

StateDataRec::StateDataRec() :
    time(-1)
#ifndef HAS_CALLBACKS
    , pyx(0)
#endif
#if ENDSTAT_REACH_STATES
    , visited(false)
#endif
{
    for (size_t i = 0; i < Model::n_agents; i++) {
        appi[i] = NULL;
        appl[i] = -1;
    }
    for (size_t i = 0; i < Model::num_goals; i++) {
        heuristicWeight[i] = 0;
    }

}

StateDataRec::~StateDataRec() {
    for (size_t i = 0; i < Model::n_agents; i++) {
        if (appi[i]) {
            delete[] appi[i];
            appi[i] = NULL;
        }
    }
}

#else // !HAS_STATE_SPACE_ANALYZER

StateDataRec::StateDataRec() :
    lastMaxLevel(-1),
    currentLevel(-1),
    queuedOnStack(false)
{
    appi[0] = NULL;
    appl[0] = -1;
}

StateDataRec::~StateDataRec() {
    if (appi[0]) {
        delete[] appi[0];
        appi[0] = NULL;
    }
}

#endif // !HAS_STATE_SPACE_ANALYZER

bool StateTablePtr_Equal::operator()(StateTablePtr const a, StateTablePtr const b) const {
    return stateEquals(&a->first, &b->first);
}

size_t StateTablePtr_Hash::operator()(StateTablePtr const a) const {
    boost::hash<model::StatePtr> h;
    return h(&a->first);
}
