#include <iostream>

Location obs = nothing;
Location loc = nothing;
bool firstObs = true;

void setLocation(int agid, Location l) {
  std::cerr << "## setLocation(" << agid << ", " << l << ")" << std::endl;
  loc = l;
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
      case 'b': obs = bathroom; break;
      case 'h': obs = hall; break;
      case 'k': obs = kitchen; break;
      default:  obs = nothing;
    }
    std::cerr << "## fetchObservation() (o = "<< o << ", obs="<<obs<< ")" << std::endl;
  }

  return std::cin.good();
}

void pre_observe_action(size_t /*agid*/) { }

void pre_observe_state(StatePtr /*x*/) {
  loc = nothing;
  std::cerr << "## pre_observe() (obs="<<obs<< ", location="<< loc <<")" << std::endl;
}

Probability post_observe_action(size_t /*agid*/) { return 1; }

Probability post_observe_state(StatePtr /*x*/) {
  std::cerr << "## post_observe() (obs="<<obs<< ", location="<< loc <<")" << std::endl;
  if (obs == nothing)
    return 1;

  if (loc == obs) 
    return 0.9;
  else 
    return 0.1;
}

