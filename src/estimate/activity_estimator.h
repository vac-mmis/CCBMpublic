#pragma once

#include "../util/iterators.h"

#include "../model.h"

#include "property_estimator.h"

/*
Estimator class for computing activity label estimations.
It relies on the observation model to set the static variables model::theActivity and the array model::theActivities[Model::n_agents]
*/

namespace model {
// required for the ActivityEstimator
// if the model does not instantiate the ActivityEstimator, these do not need to be defined

extern int theActivity;
extern int theActivities[Model::n_agents];
}

namespace ActivityEstimator_detail {

// opeator() convert the ::State to the current activity
// TODO: this assumes that model::theActivity is set *and is consistent over all states of this particle*
// This is is the case for marginal::particle::Particle (because it only has one state),
// but not necessarily for marginal::particles::Particles (because it has several states, which may perform different activities - although observation-equivalent)
template <class Model>
struct ActivityProp {
    typedef SubState<Model, 0> argument_type;

    size_t operator()(const argument_type &s) const {
        (void) s;
        // FIXME this is wrong, model::theActivity should be set for every ::State independently
        return model::theActivity;
    }
}; // struct ActivityProp

template <class Model>
struct AgentActivityProp {
    typedef SubState<Model, 0> argument_type;

    const int agid;

    AgentActivityProp(int agid) :
        agid(agid)
    { }

    size_t operator()(const argument_type &s) const {
        (void) s;
        // FIXME this is wrong, model::theActivity should be set for every ::State independently
        return model::theActivities[agid];
    }
}; // struct AgentActivityProp

// TODO: extend observation model to optionally give a string array of activity names
struct ActivityPrinter {
    std::ostream& operator()(size_t activity, std::ostream &os) const {
        return os << activity;
    }
}; // struct ActivityPrinter

} // namespace ActivityEstimator_detail


/*
Estimator class for computing activity label estimations of the full particle.
It relies on the observation model to set the static variable model::theActivity
*/
template <class Particle, size_t n_activities>
class ActivityEstimator : public ParticlePropertyEstimator<Particle, ActivityEstimator_detail::ActivityProp, n_activities, ActivityEstimator_detail::ActivityPrinter> {

    typedef ParticlePropertyEstimator<Particle, ActivityEstimator_detail::ActivityProp, n_activities, ActivityEstimator_detail::ActivityPrinter> PropEstimator;

public: // constructors

    // create a space-separated distribution of the activities
    // prints only the weights, and the activity number with the highest weight
    ActivityEstimator() :
        PropEstimator(
            PropEstimator::print_weight | PropEstimator::print_max,
            ' ', ' ', ' ')
    { }

    // Allow full control of the output
    // See documentation of ParticlePropertyEstimator for the meaning of the arguments
    ActivityEstimator(int print_mode, char prop_sep, char weight_sep, char max_sep) :
        PropEstimator(print_mode, prop_sep, weight_sep, max_sep)
    { }
};

/*
Estimates activities for each agent separately
*/
template <class Particle, size_t n_activities>
class AgentActivityEstimator : public AgentPropertyEstimator<Particle, ActivityEstimator_detail::AgentActivityProp, n_activities, ActivityEstimator_detail::ActivityPrinter> {

    typedef AgentPropertyEstimator<Particle, ActivityEstimator_detail::AgentActivityProp, n_activities, ActivityEstimator_detail::ActivityPrinter> PropEstimator;

public: // constructors

    // create a space-separated distribution of the activities
    // prints only the weights, and the activity number with the highest weight
    AgentActivityEstimator() :
        PropEstimator(
            PropEstimator::print_weight | PropEstimator::print_max,
            ' ', ' ', ' ', '\t')
    { }

    // Allow full control of the output
    // See documentation of ParticlePropertyEstimator for the meaning of the arguments
    AgentActivityEstimator(int print_mode, char prop_sep, char weight_sep, char max_sep, char agent_sep) :
        PropEstimator(print_mode, prop_sep, weight_sep, max_sep, agent_sep)
    { }
};
