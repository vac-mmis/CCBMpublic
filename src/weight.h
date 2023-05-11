#pragma once

#include "probability.h"

#ifdef USE_LOG_WEIGHTS
typedef LogProbability Weight;
#else
typedef Probability Weight;
#endif

const Weight ZeroWeight(0.0);

/**
Access the probability of the weight.
The pos parameter is a relict of USE_COMPRESSING, and serves now as a possible future extension.
*/
inline Probability prob(Weight w, size_t pos=0) {
    if (pos != 0)
        return ZeroProbability;

#ifdef USE_LOG_WEIGHTS
    // Weight { aka LogProbability } has to be converted to Probability
    return Probability(w.get());
#else
    return w;
#endif
}

inline void prob(Weight& w, Probability p, size_t pos=0) {
    if (pos != 0)
        return;

    w = p;
}
