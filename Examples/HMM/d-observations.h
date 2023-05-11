
enum Side { nothing=0, left=1, right=2 };

void setSide(size_t agid, Side s);

#define HAS_ESTIMATOR
extern int theActivity;

template <class Particle>
class Estimate : public ActivityEstimator<Particle, 3> {
};
