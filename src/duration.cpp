// Predefined duration models
// _startTime is start of action
// theTime() is current time.
// theDeltaT() is time difference since last step
// durationmodel should return true if the time is up otherwise false

#include <cstdlib>
#include <cmath>
#include <boost/math/distributions.hpp>

#include "duration.h"
#include "global.h"

/*
TODO (Issue #6): Boost provides classes for all (except "exact"/dirac) distributions.
It would be cleaner if we simply use these classes and functions, without the need for implementing *cdf, *pdf, *Model wrapper functions. cdf and pdf exist in Boost, the Model function can be written generically.
*/

// preliminary implementation of new distributions using Boost


double gammacdf(double time, double shape, double scale) {
    boost::math::gamma_distribution<> dist(shape, scale);
    return cdf(dist, time);
}

double gammapdf(double time, double shape, double scale) {
    boost::math::gamma_distribution<> dist(shape, scale);
    return pdf(dist, time);
}

bool gammaModel(double startTime, double shape, double scale, double *prob) {
    boost::math::gamma_distribution<> dist(shape, scale);
    return boostDistModel(startTime, dist, prob);
}


double lognormalcdf(double time, double location, double scale) {
    boost::math::lognormal_distribution<> dist(location, scale);
    return cdf(dist, time);
}

double lognormalpdf(double time, double location, double scale) {
    boost::math::lognormal_distribution<> dist(location, scale);
    return pdf(dist, time);
}

bool lognormalModel(double startTime, double location, double scale, double *prob) {
    boost::math::lognormal_distribution<> dist(location, scale);
    return boostDistModel(startTime, dist, prob);
}


double weibullcdf(double time, double shape, double scale) {
    boost::math::weibull_distribution<> dist(shape, scale);
    return cdf(dist, time);
}

double weibullpdf(double time, double shape, double scale) {
    boost::math::weibull_distribution<> dist(shape, scale);
    return pdf(dist, time);
}

bool weibullModel(double startTime, double shape, double scale, double *prob) {
    boost::math::weibull_distribution<> dist(shape, scale);
    return boostDistModel(startTime, dist, prob);
}


double exponentialcdf(double time, double lambda) {
    boost::math::exponential_distribution<> dist(lambda);
    return cdf(dist, time);
}

double exponentialpdf(double time, double lambda) {
    boost::math::exponential_distribution<> dist(lambda);
    return pdf(dist, time);
}

bool exponentialModel(double startTime, double lambda, double *prob) {
    boost::math::exponential_distribution<> dist(lambda);
    return boostDistModel(startTime, dist, prob);
}


double normalcdf(double time, double mean, double sigma) {
    boost::math::normal_distribution<> dist(mean, sigma);
    return cdf(dist, time);
}

double normalpdf(double time, double mean, double sigma) {
    boost::math::normal_distribution<> dist(mean, sigma);
    return pdf(dist, time);
}

bool normalModel(double startTime, double mean, double sigma, double *prob) {
    boost::math::normal_distribution<> dist(mean, sigma);
    return boostDistModel(startTime, dist, prob);
}


double cauchycdf(double time, double location, double scale) {
    boost::math::cauchy_distribution<> dist(location, scale);
    return cdf(dist, time);
}

double cauchypdf(double time, double location, double scale) {
    boost::math::cauchy_distribution<> dist(location, scale);
    return pdf(dist, time);
}

bool cauchyModel(double startTime, double location, double scale, double *prob) {
    boost::math::cauchy_distribution<> dist(location, scale);
    return boostDistModel(startTime, dist, prob);
}


double uniformcdf(double time, double lower, double upper) {
    boost::math::uniform_distribution<> dist(lower, upper);
    return cdf(dist, time);
}

double uniformpdf(double time, double lower, double upper) {
    boost::math::uniform_distribution<> dist(lower, upper);
    return pdf(dist, time);
}

bool uniformModel(double startTime, double lower, double upper, double *prob) {
    boost::math::uniform_distribution<> dist(lower, upper);
    return boostDistModel(startTime, dist, prob);
}


// old implementations

// Exact

double exactcdf(double time, double interval) {
    if (time >= interval) return 1.0;
    return 0.0;
}

double exactpdf(double time, double interval) {
    if (time == interval) {
        return 1.0;
    }
    return 0.0;
}

bool exactModel(double startTime, double interval, double *prob) {
    if (interval > theTime() - startTime) {
        if (prob) {
            *prob=0.0;
        }
        return false;
    }
    if (prob) {
        *prob=1.0;
    }
    return true;
}
