#include <boost/functional/hash.hpp>

#include "obsmodel.h"

#ifndef OBSERVATION_MODEL_IMPL
#error "OBSERVATION_MODEL_IMPL not defined"
#endif

namespace model {

#include OBSERVATION_MODEL_IMPL

void pre_observe_state(const StateRec &s) {
    pre_observe_state(&s);
}

Probability post_observe_state(const StateRec &s) {
    return post_observe_state(&s);
}

// call global :observation callback
void do_state_callback(const StateRec &s) {
#ifdef USE_ACTION_OBSERVATION
    (void) s;
    // no state callbacks if USE_ACTION_OBSERVATION is set
    return;
#endif
    stateObservation(&s);
}

void do_action_callback(const Model::opid_t &opid, const StateRec &s) {
    (void) opid;
    (void) s;

#ifdef USE_ACTION_OBSERVATION
    model::actionCallback(opid);
    return;
#endif

#ifdef HAS_CALLBACKS
    // if present, call action :observation callbacks
    for (size_t i = 0; i < Model::n_agents; i++) {
        if (opid[i] >= 0)
            model::actions[opid[i]]->callback(&s, i);
        else
            // If callbacks are used, the observation header must define
            // this callback for INITIALIZE etc. actions.
            model::specialOpCallback(i, opid[i]);
    }
#endif // HAS_CALLBACKS
}

// do full callbacks
void doCallbacksAndObservations(const Model::opid_t &opid, const StateRec &s) {
    do_action_callback(opid, s);
    do_state_callback(s);
}

// if the moel does not SUPPORTS_OBS_EQUIV, we give a default implementation that always returns false
// This allows it to compile the corresponding filter code and check for errors regardless of the current model
#if !SUPPORTS_OBS_EQUIV
bool observationEquivalent(const StateRec &s1, const Model::opid_t &opid1, const StateRec &s2, const Model::opid_t &opid2) {
    return &s1 == &s2 && opid1 == opid2;
};
#endif // !SUPPORTS_OBS_EQUIV

// Similar for SUPPORTS_OBS_HASH
// If SUPPORTS_OBS_EQUIV is true, then we should ensure this is also a proper hash function (x==y → hash(x)==hash(y))
// Because we do not know the code of observationEquivalent, we must return a constant value here
#if !SUPPORTS_OBS_HASH
size_t observation_hash_value(const StateRec &s, const Model::opid_t &opid) {
    (void) s;
    (void) opid;
    return 0;
};
#endif // !SUPPORTS_OBS_EQUIV

}
