#pragma once

#include <iostream>
#include <iomanip>

#include "../util/iterators.h"

#include "../weight.h"
#include "../model.h"
#include "../particle_common.h"

#include "estimate.h"

/*
Generic estimator class used to estimate a *single scalar* property of particles.

Depending on the parameters given to the constructor, it can print in different formats.

print_prop and print_weight: print the properties and their weight
<time>;prop_1,weight_1;prop_2,weight_2,...

print_weight only: only print the weights of the properties in order
<time>;weight_1;weight_2;...

(print_prop only makes no sense)

print_max: only the property with the highest probability is printed
<time>,max_prop

print_max and print_max_weight: only the property with the highest probability is printed, together with its probability
<time>,max_prop:max_weight

print_prop, print_weight and print_max: print the properties and their weight, and max_prop at the end
<time>;prop_1:weight_1;prop_2:weight_2;...,max_prop

print_weight and print_max: only print the weights of the properties in order, but the property with highest weight at the end
<time>;weight_1;weight_2;...,max_prop

In all cases, ";" is prop_sep, "," is weight_sep, and ":" is max_sep
weight_i is the numerical weight, and prop_i / max_prop are the names of the property (as printed by the PropertyPrinter)

If skip_zero is true, only properties with non-zero weight will be listed.
You should not use it without setting print_prop, otherwise you have no chance to correlate the weights with the properties.
skip_zero is ignored if only print_max is set.

Template parameters:
* Particle: the Particle class
* PropertyFunc: a template <class Model> of a default-constructable class with size_t operator()(const State &state), which converts a State to the property to be estimated
                it must have a typedef argument_type which is the type of State it is expecting (either full ::State<Model> or some SubState<Model, vars>)
                it is recommended to always use a SubState<Model, vars> with the smallest set of variables - this allows estimates to be more efficient when the
                particle class can efficiently marginalise over all unrequired state variables
* n_props: the number of different property values
* PropertyPrinter: a default-constructable class with std::ostream& operator()(size_t, std::ostream&), which prints a readable value to the stream
*/
template <class Particle, template <typename> class PropertyFunc, size_t n_props, class PropertyPrinter>
class ParticlePropertyEstimator : public Estimator<Particle> {
public: // static constants

    // print mode

    // shall the property names itself be printed?
    static const int print_prop = 1<<0;

    // shall the weights of the properties be printed?
    static const int print_weight = 1<<1;

    // shall the property with the highest weight be printed separately?
    static const int print_max = 1<<2;
    // shall its weight be printed?
    static const int print_max_weight = 1<<3;

    // shall properties with zero weight be skipped?
    static const int skip_zero = 1<<4;

    // shall the current weight not be printed? (first column)
    static const int print_notime = 1<<5;

    // shall a trailing '\n' not be printed?
    static const int print_noendl = 1<<6;


    // other public constants

    static const size_t n_properties = n_props;

protected: // internal fields

    // the summed weights for each property value
    Weight weights[n_props];

    // the index of the property with the maximum weight
    size_t maxi;
    // the maximum weight of this property
    Weight maxw;

    double time;

    // Any valid combination of the flags
    // print_prop, print_weight, print_max, skip_zero
    int print_mode;

    // separator between different properties
    char prop_sep;
    // separator between property and its weight
    char weight_sep;
    // char to print before the property with highest weight (only if print_max)
    char max_sep;

protected: // internal methods

    void init();

public: // public methods

    // The flags cannot be changed, because it would invalidate the header
    ParticlePropertyEstimator(int print_mode, char prop_sep, char weight_sep, char max_sep) :
        print_mode(print_mode),
        prop_sep(prop_sep),
        weight_sep(weight_sep),
        max_sep(max_sep)
    { }

    int getMode() const {
        return print_mode;
    }

    // print a header with all property names, preceded by "Time"
    // this should only be called if skip_zero is not set
    virtual std::ostream& printHeader(std::ostream &os) const;

    virtual void start(double time) {
        this->time = time;
        init();
    }

    virtual void pre_collect(const Particle & /*p*/) {
    }

    virtual void collect(const Particle &p);

    virtual void finish();

    virtual std::ostream& print(std::ostream &os) const;
}; // class ParticlePropertyEstimator





/*
Similar to the ParticlePropertyEstimator, but estimates the property for each agent individually.
Each agent estimate is separated by agent_sep.

Template parameters:
* Particle: the Particle class
* AgentPropertyFunc: a template <class Model> of a class (constructed from agent id) with size_t operator()(const State &state), which converts a state to the agent's property to be estimated
                     See ParticlePropertyEstimator for more details.
* n_props: the number of different property values
* PropertyPrinter: a default-constructable class with std::ostream& operator()(size_t, std::ostream&), which prints a readable value to the stream
*/
template <class Particle, template <typename> class AgentPropertyFunc, size_t n_props, class PropertyPrinter>
class AgentPropertyEstimator : public Estimator<Particle> {
public: // static constants

    // print mode
    // shall the property names itself be printed?
    static const int print_prop = 1<<0;

    // shall the weights of the properties be printed?
    static const int print_weight = 1<<1;

    // shall the property with the highest weight be printed separately?
    static const int print_max = 1<<2;
    // shall its weight be printed?
    static const int print_max_weight = 1<<3;

    // shall properties with zero weight be skipped?
    static const int skip_zero = 1<<4;

    // shall the current weight not be printed? (first column)
    static const int print_notime = 1<<5;

    // shall a trailing '\n' not be printed?
    static const int print_noendl = 1<<6;


    // other public constants

    static const size_t n_properties = n_props;

    static const size_t n_agents = Particle::Model::n_agents;

protected: // internal fields

    // the summed weights for each property value
    Weight weights[n_agents][n_props];

    // the index of the property with the maximum weight
    size_t maxi[n_agents];
    // the maximum weight of this property
    Weight maxw[n_agents];

    double time;

    // Any valid combination of the flags
    // print_prop, print_weight, print_max, skip_zero
    int print_mode;

    // separator between different properties
    char prop_sep;
    // separator between property and its weight
    char weight_sep;
    // char to print before the property with highest weight (only if print_max)
    char max_sep;
    // char to separate different agents
    char agent_sep;

protected: // internal methods

    void init();

public: // public methods

    // The flags cannot be changed, because it would invalidate the header
    AgentPropertyEstimator(int print_mode, char prop_sep, char weight_sep, char max_sep, char agent_sep) :
        print_mode(print_mode),
        prop_sep(prop_sep),
        weight_sep(weight_sep),
        max_sep(max_sep),
        agent_sep(agent_sep)
    { }

    int getMode() const {
        return print_mode;
    }

    // print a header with all property names, preceded by "Time"
    // this should only be called if skip_zero is not set
    virtual std::ostream& printHeader(std::ostream &os) const;

    virtual void start(double time) {
        this->time = time;
        init();
    }

    virtual void pre_collect(const Particle & /*p*/) {
    }

    virtual void collect(const Particle &p);

    virtual void finish();

    virtual std::ostream& print(std::ostream &os) const;
}; // class AgentPropertyEstimator



namespace estimate {
namespace detail {

// takes a function that converts a ::State to a size_t (property), and provides a function that lifts this to a transform of std::pair< ::State, Weight> to std::pair<size_t, Weight>
// that is, this transform a weighted state to a weighted property, by using a user-provided function that converts a ::State to a property
template <template <typename Model> class F, class Model>
struct weighted_transform {
    F<Model> f;

    weighted_transform() :
        f()
    { }

    weighted_transform(F<Model> f) :
        f(f)
    { }

    std::pair<size_t, Weight> operator()(const std::pair<const typename F<Model>::argument_type &, Weight> &state_estimate) const {
        return std::make_pair(f(state_estimate.first), state_estimate.second);
    }
}; // struct weighted_transform

} // namespace estimate:: detail
} // namespace estimate



// Implementations of ParticlePropertyEstimator



template <class Particle, template <typename> class PropertyFunc, size_t n_props, class PropertyPrinter>
std::ostream& ParticlePropertyEstimator<Particle, PropertyFunc, n_props, PropertyPrinter>::printHeader(std::ostream &os) const {
    static PropertyPrinter printer;
    os << std::setprecision( std::numeric_limits<double>::max_digits10 );

    if (!(print_mode & print_notime))
        os << "Time";

    for (size_t i = 0; i < n_props; i++) {
        os << prop_sep;
        printer(i, os);
    }

    if (!(print_mode & print_noendl))
        os << '\n';
    return os;
}

template <class Particle, template <typename> class PropertyFunc, size_t n_props, class PropertyPrinter>
void ParticlePropertyEstimator<Particle, PropertyFunc, n_props, PropertyPrinter>::init() {
    maxw = ZeroWeight;
    maxi = 0;
    for (size_t i = 0; i < n_props; ++i) {
        weights[i] = ZeroWeight;
    }
}

template <class Particle, template <typename> class PropertyFunc, size_t n_props, class PropertyPrinter>
void ParticlePropertyEstimator<Particle, PropertyFunc, n_props, PropertyPrinter>::collect(const Particle &p) {
    typedef estimate::detail::weighted_transform<PropertyFunc, typename Particle::Model> transform_func_t;
    static transform_func_t propFunc;

    // first, get a distribution over the states of this particle
    // then, convert this distribution to distribution over this particular property (by transforming it using the PropertyFunc)
    // then, collect the weights of the property values

    // the distribution over states
    typedef typename PropertyFunc<typename Particle::Model>::argument_type state_t;
    auto state_range = p.template getDistribution<state_t>();

    // distribution over properties (iterator over std::pair<size_t, Weight>)
    typedef boost::transform_iterator<transform_func_t, typename Particle::template dist_range<state_t>::iterator, std::pair<size_t, Weight> > property_dist_iterator;

    auto property_range = make_transform_iterator_range<property_dist_iterator>(state_range, propFunc);

    typedef std::pair<size_t, Weight> estimate_t;
    for (const estimate_t &weighted_prop: property_range) {
        weights[weighted_prop.first] += weighted_prop.second;
    }
}

template <class Particle, template <typename> class PropertyFunc, size_t n_props, class PropertyPrinter>
void ParticlePropertyEstimator<Particle, PropertyFunc, n_props, PropertyPrinter>::finish() {
    if (!(print_mode & print_max))
        // do not need to determine property with highest weight
        return;

    for (size_t i = 0; i < n_props; i++) {
        if (weights[i] > maxw) {
            maxw = weights[i];
            maxi = i;
        }
    }
}

template <class Particle, template <typename> class PropertyFunc, size_t n_props, class PropertyPrinter>
std::ostream& ParticlePropertyEstimator<Particle, PropertyFunc, n_props, PropertyPrinter>::print(std::ostream &os) const {
    static PropertyPrinter printer;

    if (!(print_mode & print_notime))
        os << time;

    if (print_mode & (print_prop | print_weight)) {
        // individual properties / weights need to be printed

        for (size_t i = 0; i < n_props; i++) {
            if ((print_mode & skip_zero) && weights[i] <= ZeroWeight)
                continue;

            os << prop_sep;
            if (print_mode & print_prop)
                // print the property
                printer(i, os);
            if (print_mode & print_weight) {
                // print the weight
                if (print_mode & print_prop)
                    // property has been printed: add separator
                    os << weight_sep;
                os << weights[i];
            }
        }
    }

    if (print_mode & print_max) {
        // print the maximum property

        if (print_mode & (print_prop | print_weight))
            // if something has been printed before, add separator
            os << max_sep;

        printer(maxi, os);
        if (print_mode & print_max_weight)
            // … and its weight
            os << weight_sep << maxw;
    }

    if (!(print_mode & print_noendl))
        os << '\n';

    // in any case, flush after every timestep
    os.flush();

    return os;
}



template <class Particle, template <typename> class PropertyFunc, size_t n_props, class PropertyPrinter>
inline std::ostream &operator<<(std::ostream &os, const ParticlePropertyEstimator<Particle, PropertyFunc, n_props, PropertyPrinter> &e) {
    return e.print(os);
}



// Implementations of AgentPropertyEstimator



template <class Particle, template <typename> class AgentPropertyFunc, size_t n_props, class PropertyPrinter>
std::ostream& AgentPropertyEstimator<Particle, AgentPropertyFunc, n_props, PropertyPrinter>::printHeader(std::ostream &os) const {
    static PropertyPrinter printer;
    os << std::setprecision( std::numeric_limits<double>::max_digits10 );

    if (!(print_mode & print_notime))
        os << "Time";

    for (size_t agent = 0; agent < n_agents; agent++) {
        os << agent_sep;
        for (size_t i = 0; i < n_props; i++) {
            if (i > 0)
                os << prop_sep;
            printer(i, os);
        }
    }

    if (!(print_mode & print_noendl))
        os << '\n';
    return os;
}

template <class Particle, template <typename> class AgentPropertyFunc, size_t n_props, class PropertyPrinter>
void AgentPropertyEstimator<Particle, AgentPropertyFunc, n_props, PropertyPrinter>::init() {
    for (size_t agent = 0; agent < n_agents; agent++) {
        maxw[agent] = ZeroWeight;
        maxi[agent] = 0;
        for (size_t i = 0; i < n_props; ++i) {
            weights[agent][i] = ZeroWeight;
        }
    }
}

template <class Particle, template <typename> class AgentPropertyFunc, size_t n_props, class PropertyPrinter>
void AgentPropertyEstimator<Particle, AgentPropertyFunc, n_props, PropertyPrinter>::collect(const Particle &p) {

    // first, get a distribution over the states of this particle
    // then, convert this distribution to distribution over this particular property for an agent (by transforming it using the AgentPropertyFunc)
    // then, collect the weights of the property values

    // the distribution over states
    typedef typename AgentPropertyFunc<typename Particle::Model>::argument_type state_t;
    auto state_range = p.template getDistribution<state_t>();

    typedef estimate::detail::weighted_transform<AgentPropertyFunc, typename Particle::Model> transform_func_t;

    // distribution over properties (iterator over std::pair<size_t, Weight>)
    typedef boost::transform_iterator<transform_func_t, typename Particle::template dist_range<state_t>::iterator, std::pair<size_t, Weight> > property_dist_iterator;

    for (size_t agent = 0; agent < n_agents; agent++) {
        transform_func_t propFunc = transform_func_t(AgentPropertyFunc<typename Particle::Model>(agent));

        auto property_range = make_transform_iterator_range<property_dist_iterator>(state_range, propFunc);

        typedef std::pair<size_t, Weight> estimate_t;
        for (const estimate_t &weighted_prop: property_range) {
            weights[agent][weighted_prop.first] += weighted_prop.second;
        }
    }
}

template <class Particle, template <typename> class AgentPropertyFunc, size_t n_props, class PropertyPrinter>
void AgentPropertyEstimator<Particle, AgentPropertyFunc, n_props, PropertyPrinter>::finish() {
    if (!(print_mode & print_max))
        // do not need to determine property with highest weight
        return;

    for (size_t agent = 0; agent < n_agents; agent++)
        for (size_t i = 0; i < n_props; i++) {
            if (weights[agent][i] > maxw[agent]) {
                maxw[agent] = weights[agent][i];
                maxi[agent] = i;
            }
        }
}

template <class Particle, template <typename> class AgentPropertyFunc, size_t n_props, class PropertyPrinter>
std::ostream& AgentPropertyEstimator<Particle, AgentPropertyFunc, n_props, PropertyPrinter>::print(std::ostream &os) const {
    static PropertyPrinter printer;

    if (!(print_mode & print_notime))
        os << time;

    for (size_t agent = 0; agent < n_agents; agent++) {
        os << agent_sep;

        if (print_mode & (print_prop | print_weight)) {
            // individual properties / weights need to be printed

            size_t printed = 0; // print prop_sep only after the first actually printed property (due to skip_zero)

            for (size_t i = 0; i < n_props; i++) {
                if ((print_mode & skip_zero) && weights[agent][i] <= ZeroWeight)
                    continue;

                if (printed > 0)
                    os << prop_sep;
                printed++;

                if (print_mode & print_prop)
                    // print the property
                    printer(i, os);
                if (print_mode & print_weight) {
                    // print the weight
                    if (print_mode & print_prop)
                        // property has been printed: add separator
                        os << weight_sep;
                    os << weights[agent][i];
                }
            }
        }

        if (print_mode & print_max) {
            // print the maximum property

            if (print_mode & (print_prop | print_weight))
                // if something has been printed before, add separator
                os << max_sep;

            printer(maxi[agent], os);
            if (print_mode & print_max_weight)
                // … and its weight
                os << weight_sep << maxw[agent];
        }
    }

    if (!(print_mode & print_noendl))
        os << '\n';

    // in any case, flush after every timestep
    os.flush();

    return os;
}



template <class Particle, template <typename> class AgentPropertyFunc, size_t n_props, class PropertyPrinter>
inline std::ostream &operator<<(std::ostream &os, const AgentPropertyEstimator<Particle, AgentPropertyFunc, n_props, PropertyPrinter> &e) {
    return e.print(os);
}
