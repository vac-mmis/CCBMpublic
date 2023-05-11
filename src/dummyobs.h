#pragma once

/* Dummy observation definitions for RC */

#ifdef USE_ACTION_OBSERVATION

#ifndef MISSING_PROB
#define MISSING_PROB 0.0
#endif

#ifndef NOISE_PROB
#define NOISE_PROB 0.0
#endif

#define INVALID_ACTION -10
#define MISSING_ACTION -11

#endif // USE_ACTION_OBSERVATION
