#include <math.h>

const double itopi = 1/sqrt(2*M_PI);

double gaussian(double x, double mu, double sd) {
  double dd = x-mu;

  return (itopi/sd)*exp(-dd*dd/(2*sd*sd));
}

//////////////////////////////////////////////////////////////////////


Observation theObservation = {3,3};
Observation theState;

bool fetchObservation() {
    std::cin >> theObservation.x >> theObservation.y;
    return std::cin.good();
}


void setXYcoordinates(double x, double y) {
  theState.x = x; theState.y = y;
}

void pre_observe_action(size_t /*agid*/) { }
void pre_observe_state(StatePtr /*x*/) { }

Probability post_observe_action(size_t /*agid*/) { return 1; }
Probability post_observe_state(StatePtr /*x*/) {
  return gaussian(theObservation.x,theState.x,1) * gaussian(theObservation.y,theState.y,1);
}
