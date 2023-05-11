#pragma once

#include "util/log.h"

typedef Log<false> LogProbability;

inline double p_value(double prob) { return prob; }
inline double p_logValue(double prob) { return log(prob); }

inline double p_value(LogProbability prob) { return (double) prob; }
inline double p_logValue(LogProbability prob) { return prob.log(); }

#ifdef USE_LOG_PROBS
typedef LogProbability Probability;

inline Probability make_p(double p) { return Probability(p); }
// make probability from log-value
inline Probability make_p(double p, log_domain_t) { return Probability(p, log_domain); }

#else
typedef double Probability;

inline Probability make_p(double p) { return p; }
// make probability from log-value
inline Probability make_p(double p, log_domain_t) { return exp(p); }
#endif // USE_LOG_PROBS

const Probability ZeroProbability(0);
