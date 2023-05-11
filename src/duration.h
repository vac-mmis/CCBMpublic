// Predefined duration models
// _startTime is start of action
// theTime() is current time.
// theDeltaT() is time difference since last step
// durationmodel should return true if the time is up otherwise false
#pragma once

#include "global.h"

double gammacdf(double time, double shape, double scale);
double gammapdf(double time, double shape, double scale);
bool gammaModel(double startTime, double shape, double scale, double *prob=NULL);

double lognormalcdf(double time, double location, double scale);
double lognormalpdf(double time, double location, double scale);
bool lognormalModel(double startTime, double location, double scale, double *prob=NULL);

double weibullcdf(double time, double shape, double scale);
double weibullpdf(double time, double shape, double scale);
bool weibullModel(double startTime, double shape, double scale, double *prob=NULL);

double exponentialcdf(double time, double lambda);
double exponentialpdf(double time, double lambda);
bool exponentialModel(double startTime, double lambda, double *prob=NULL);

double normalcdf(double time, double mean, double sigma);
double normalpdf(double time, double mean, double sigma);
bool normalModel(double startTime, double mean, double sigma, double *prob = NULL);

double cauchycdf(double time, double location, double scale);
double cauchypdf(double time, double location, double scale);
bool cauchyModel(double startTime, double s, double t, double *prob=NULL);

double uniformcdf(double time, double a, double b);
double uniformpdf(double time, double a, double b);
bool uniformModel(double startTime, double a, double b, double *prob=NULL);

double exactcdf(double time, double interval);
double exactpdf(double time, double interval);
bool exactModel(double startTime, double interval, double *prob = NULL);

// generic implementation of *Model functions for boost distributions

template<class Dist>
bool boostDistModel(double startTime, Dist dist, double *prob) {
    double a = (theTime() - theDeltaT()) - startTime;
    double b = theTime() - startTime;

    double cdfa = cdf(dist, a);

    if (1.0 == cdfa) {
        // the cdf has reached 1, i.e. this is the maximum duration
        if (prob)
            *prob = 1.0;
        return true;
    }

    double cdfa_c = cdf(complement(dist, a)); // 1 - cdfa or P(X>a)
    double r;

    if (cdfa < 0.9) {
        // for small values of the cdf, everything is fine
        double cdfb = cdf(dist, b);
        r = (cdfb - cdfa) / cdfa_c;
    } else {
        // for large values of the cdf, the accuracy drops enormously
        // therefore, compute with the complement values
        double cdfb_c = cdf(complement(dist, b));
        r = (cdfa_c - cdfb_c) / cdfa_c;
    }

    if (prob) {
        *prob=r;
    }

    double x = randomval();
    return x<=r;
}
