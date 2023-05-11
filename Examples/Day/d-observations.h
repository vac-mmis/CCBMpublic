#pragma once

#define NUM_ACTIVITIES 5
//#define SUPPORTS_PYA 1
//#define SUPPORTS_OBS_EQUIV 1
//#define SUPPORTS_OBS_HASH 1

void setActivity(int agid, int i);
extern int theActivity;

#define HAS_ESTIMATOR

template <class Particle>
class Estimate : public ActivityEstimator<Particle, NUM_ACTIVITIES> {};
