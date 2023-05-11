#pragma once

#include <iostream>

#include "../weight.h"
#include "../model.h"

#include "estimate.h"

namespace ActionEstimator_detail {

template <class Model>
struct ActionProp {
    typedef SubState<Model, StateVar_A> argument_type;
    const int agid;

    ActionProp(int agid) :
        agid(agid)
    { }

    size_t operator()(const argument_type &s) const {
        return s.opid[agid] + 3;
    }
}; // struct ActionProp

struct ActionPrinter {
    std::ostream& operator()(size_t action, std::ostream &os) const {
        os << '"' << model::actions[action - 3]->name << '"';
        return os;
    }
}; // struct ActionPrinter

} // namespace ActionEstimator_detail

/*
Estimator class for giving estimates about the ground actions.
This is used when defining USE_ACTION_ESTIMATION

Template parameters:
* Particle: the Particle class
* n_ops: the number of actions in the model
*/
template <class Particle, size_t n_ops>
class ActionEstimator : public AgentPropertyEstimator<Particle, ActionEstimator_detail::ActionProp, n_ops + 3, ActionEstimator_detail::ActionPrinter> {

    typedef AgentPropertyEstimator<Particle, ActionEstimator_detail::ActionProp, n_ops + 3, ActionEstimator_detail::ActionPrinter> PropEstimator;

public:

    ActionEstimator(int type) :
        PropEstimator(
            type == 0 // action,weight;...
            ? (PropEstimator::print_prop | PropEstimator::print_weight | PropEstimator::skip_zero)
            : ( type == 1 // full distribution: weight weight weight ...
            ? (PropEstimator::print_weight)
            // else: only action with heighest weight
            : (PropEstimator::print_max | PropEstimator::print_max_weight)
            ),
            type != 1 ? ',' : ' ', // when printing only weight, separate by space, otherwise separate properties by comma
            ':', ';', '\t')
    {}
}; // class ActionEstimator
