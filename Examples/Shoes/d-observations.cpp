#include <iostream>

Side obs = nothing;
Side side = nothing;
bool firstObs = true;

void setSide(int agid, Side s) {
  std::cerr << "## setSide(" << agid << ", " << s << ")" << std::endl;
  side = s;
}

void specialOpCallback(int agid, int op) {
  std::cerr << "## specialOpCallback(" << agid << ", " << op << ")" << std::endl;
}

bool fetchObservation() {
  if (firstObs) {
    firstObs = false;
    obs = nothing; 
    std::cerr << "## fetchObservation() (firstObs=true, obs="<<obs<< ")" << std::endl;
  } else {
    char o;
    std::cin >> o;
    switch(o) {
      case 'l': obs = left; break;
      case 'r': obs = right; break;
      default:  obs = nothing;
    }
    std::cerr << "## fetchObservation() (o = "<< o << ", obs="<<obs<< ")" << std::endl;
  }

  return std::cin.good();
}

void pre_observe_action(size_t /*agid*/) {
  side = nothing;
  std::cerr << "## pre_observe() (obs="<<obs<< ", side="<< side <<")" << std::endl;
}

void pre_observe_state(StatePtr /*x*/) { }

Probability post_observe_action(size_t /*agid*/) {
  std::cerr << "## post_observe() (obs="<<obs<< ", side="<< side <<")" << std::endl;
  if (obs == nothing)
    return 1;

  if (side == obs) 
    return 0.9;
  else 
    return 0.1;
}

Probability post_observe_state(StatePtr /*x*/) { return 1; }

