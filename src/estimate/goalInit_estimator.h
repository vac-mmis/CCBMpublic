#pragma once

#include <iosfwd>

#include "property_estimator.h"

// Estimators for the goal and initial state

namespace GoalInitEstimator_detail {

template <class Model>
struct InitProp {
    typedef SubState<Model, StateVar_I> argument_type;
    size_t operator()(const argument_type &s) const {
        return s.initialIndex();
    }

}; // struct InitProp

template <class Model>
struct GoalProp {
    typedef SubState<Model, StateVar_G> argument_type;
    size_t operator()(const argument_type &s) const {
        return s.goalIndex();
    }

}; // struct GoalProp

struct InitPrinter {
    std::ostream& operator()(size_t goal, std::ostream &os) const {
        return os << model::initialStateNames[goal];
    }
}; // struct InitPrinter

struct GoalPrinter {
    std::ostream& operator()(size_t goal, std::ostream &os) const {
        return os << model::goalNames[goal];
    }
}; // struct GoalPrinter

} // namespace GoalInitEstimator_detail

template <class Particle, size_t n_goals>
class GoalEstimator : public ParticlePropertyEstimator<Particle, GoalInitEstimator_detail::GoalProp, n_goals, GoalInitEstimator_detail::GoalPrinter> {

    typedef ParticlePropertyEstimator<Particle, GoalInitEstimator_detail::GoalProp, n_goals, GoalInitEstimator_detail::GoalPrinter> PropEstimator;

public: // constructors

    // create a space-separated distribution of the goals
    // if textual is true, then print in the format "<goal>:<weight>" (where zero weights are skipped)
    // if textual is false, print "<weight>" for every goal (even if zero) - this is useful for parsing, e.g. with R
    GoalEstimator(bool textual) :
        PropEstimator(textual ?
            (PropEstimator::print_prop | PropEstimator::print_weight | PropEstimator::skip_zero) : (PropEstimator::print_weight),
        ' ', ':', ',')
    { }

    // Allow full control of the output
    // See documentation of ParticlePropertyEstimator for the meaning of the arguments
    GoalEstimator(int print_mode, char prop_sep, char weight_sep, char max_sep) :
        PropEstimator(print_mode, prop_sep, weight_sep, max_sep)
    { }
};

template <class Particle, size_t n_goals>
class InitEstimator : public ParticlePropertyEstimator<Particle, GoalInitEstimator_detail::InitProp, n_goals, GoalInitEstimator_detail::InitPrinter> {

    typedef ParticlePropertyEstimator<Particle, GoalInitEstimator_detail::InitProp, n_goals, GoalInitEstimator_detail::InitPrinter> PropEstimator;

public: // constructors

    // create a space-separated distribution of the initial states
    // if textual is true, then print in the format "<init>:<weight>" (where zero weights are skipped)
    // if textual is false, print "<weight>" for every initial state (even if zero) - this is useful for parsing, e.g. with R
    InitEstimator(bool textual) :
        PropEstimator(textual ?
            (PropEstimator::print_prop | PropEstimator::print_weight | PropEstimator::skip_zero) : (PropEstimator::print_weight),
        ' ', ':', ',')
    { }

    // Allow full control of the output
    // See documentation of ParticlePropertyEstimator for the meaning of the arguments
    InitEstimator(int print_mode, char prop_sep, char weight_sep, char max_sep) :
        PropEstimator(print_mode, prop_sep, weight_sep, max_sep)
    { }
};
