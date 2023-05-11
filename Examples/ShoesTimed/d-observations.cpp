#include <iostream>

Side obs = nothing;
Side side = nothing;
bool firstObs = true;

void setSide(int agid, Side s) {
  (void) agid;
#if VERBOSITY>2
  std::cerr << "## setSide(" << agid << ", " << s << ")" << std::endl;
#endif
  side = s;
}

void specialOpCallback(int agid, int op) {
  (void) agid;
  (void) op;
#if VERBOSITY>2
  std::cerr << "## specialOpCallback(" << agid << ", " << op << ")" << std::endl;
#endif
}

bool fetchObservation() {
  if (firstObs) {
    firstObs = false;
    obs = nothing; 
#if VERBOSITY>2
    std::cerr << "## fetchObservation() (firstObs=true, obs="<<obs<< ")" << std::endl;
#endif
  } else {
    char o;
    std::cin >> o;
    switch(o) {
      case 'l': obs = left; break;
      case 'r': obs = right; break;
      default:  obs = nothing;
    }
#if VERBOSITY>2
    std::cerr << "## fetchObservation() (o = "<< o << ", obs="<<obs<< ")" << std::endl;
#endif
  }

  return std::cin.good();
}

void pre_observe_action(size_t /*agid*/) {
  side = nothing;
#if VERBOSITY>2
  std::cerr << "## pre_observe() (obs="<<obs<< ", side="<< side <<")" << std::endl;
#endif
}

void pre_observe_state(StatePtr /*x*/) { }

Probability post_observe_action(size_t /*agid*/) {
#if VERBOSITY>2
  std::cerr << "## post_observe() (obs="<<obs<< ", side="<< side <<")" << std::endl;
#endif
  if (obs == nothing)
    return 1;

  if (side == obs) 
    return 0.9;
  else 
    return 0.1;
}

Probability post_observe_state(StatePtr /*x*/) { return 1; }

