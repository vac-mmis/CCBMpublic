#pragma once

#include <ostream>
#include <memory>

#include "../global.h"

/*
Abstract base class for all estimators.
The typical sequence for this class is:

for every time t
    start(t)
    for every particle p
        pre_collect(p)
        <make observations for p>
        collect(p)
    finish()
    print()
*/
template <class Particle>
class Estimator {
public: // public methods

    // print a header if the output format allows this, (the first column is mostly "Time")
    virtual std::ostream& printHeader(std::ostream &os) const = 0;

    // has to be called once per time-step before the estimation starts
    virtual void start(double time) = 0;

    virtual void pre_collect(const Particle &p) = 0;

    virtual void collect(const Particle &p) = 0;

    virtual void finish() = 0;

    virtual std::ostream& print(std::ostream &os) const = 0;
}; // class Estimator

template <class Particle>
inline std::ostream &operator<<(std::ostream &os, const Estimator<Particle> &e) {
    return e.print(os);
}


// One estimator associated with a targeted output
template <class Particle>
struct EstimateTarget {
    // TODO c++11: this should probably be better a unique_ptr, because only one should actually use the estimator
    std::shared_ptr<Estimator<Particle> > estimator;

    std::shared_ptr<std::ostream> os;

    EstimateTarget(const std::shared_ptr<Estimator<Particle> > &estimator, std::shared_ptr<std::ostream> os) :
        estimator(estimator),
        os(os)
    { }

    void print() const {
        estimator->print(*os);
    }

    void printHeader() const {
        estimator->printHeader(*os);
    }
}; // struct EstimateTarget


/*
runs the estimators over all particles and prints their estimate

ItParticle: An iterator over Particle* (or iterator over anything that dereferences to a Particle)
ItEst: An iterator over EstimateTarget
*/
template <class ItParticle, class ItEst>
void runEstimators(double time, const ItEst estBegin, const ItEst estEnd, ItParticle particleBegin, const ItParticle particleEnd) {
    // start estimators
    ItEst est = estBegin;
    while (est != estEnd) {
        est->estimator->start(time);
        ++est;
    }
    // for every particle, run estimators: pre_collect, observe, collect
    while (particleBegin != particleEnd) {
        est = estBegin;
        while (est != estEnd) {
            est->estimator->pre_collect(**particleBegin);
            ++est;
        }

        (*particleBegin)->doCallbacksAndObservations();

        est = estBegin;
        while (est != estEnd) {
            est->estimator->collect(**particleBegin);
            ++est;
        }

        ++particleBegin;
    }

    // finish and print estimators
    est = estBegin;
    while (est != estEnd) {
        est->estimator->finish();
        est->print();
        ++est;
    }
}
