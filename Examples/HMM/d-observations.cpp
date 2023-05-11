#include <iostream>

int theActivity;
Side obs = nothing;
Side side = nothing;
bool firstObs = true;

void setSide(size_t agid, Side s) {
  (void) agid; // this is a single-agent model
  side = s;
  theActivity = s;
}

void specialOpCallback(int agid, int op) {
    (void) agid;
    (void) op;
}

bool fetchObservation() {
  if (firstObs) {
    firstObs = false;
    obs = nothing; 
  } else {
    char o;
    std::cin >> o;
    switch(o) {
      case 'l': obs = left; break;
      case 'r': obs = right; break;
      default:  obs = nothing;
    }
  }

  return std::cin.good();
}

void pre_observe_action(size_t /*agid*/) {
  side = nothing;
}

void pre_observe_state(StatePtr /*x*/) { }

Probability post_observe_action(size_t /*agid*/) {
  if (obs == nothing)
    return 1;

  if (side == obs) 
    return 0.9;
  else 
    return 0.1;
}

Probability post_observe_state(StatePtr /*s*/) { return 1; }
