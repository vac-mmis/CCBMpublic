#include <iostream>

Probability obs[NUM_ACTIVITIES];
int theActivity;
bool firstObs = true;

void setActivity(int agid, int i) {
    std::cerr << "## setActivity(" << agid << ", " << i << ")" << std::endl;
    (void) agid; // single-agent model
    // use zero-based activity
    theActivity = i - 1;
}

void specialOpCallback(int agid, int opid) {
    //if (opid == -1) {
    //	setActivity(agid, 6); // set unknown as initial activity
    //}
    std::cerr << "## specialOpCallback(" << agid << ", " << opid << ")" << std::endl;
}

bool fetchObservation() {
    std::cin.ignore(100000,'\n'); //ignore the first line
    if (firstObs) {
        // the original lisp model does no observation for the initial state, so simply return 1 in the observations
        for (int i = 0; i < NUM_ACTIVITIES; ++i) {
            obs[i] = 1;
        }
        firstObs = false;
        return true;
    }
//	std::cin.ignore(100000,'\n');
	
    for (int i = 0; i < NUM_ACTIVITIES; ++i) {
        double o;
        std::cin >> o;
        std::cerr << "bla" << std::endl;
        if (!std::cin.good())
            return false;
		obs[i] = o;
        std::cerr << o;
    }
    std::cerr << std::endl;
    return true;
}

void pre_observe_action(size_t /*agid*/) {
  //theActivity = 5;
  const int N = sizeof(obs) / sizeof(int);
  int o = std::distance(obs, std::max_element(obs, obs + N));
  std::cerr << "## pre_observe() (obs="<<o<< ", activity="<< theActivity <<")" << std::endl;
}

void pre_observe_state(StatePtr /*x*/) { }

Probability post_observe_action(size_t /*agid*/) {
  const int N = sizeof(obs) / sizeof(int);
  int o = std::distance(obs, std::max_element(obs, obs + N));
  std::cerr << "## post_observe() (obs="<< o << ", activity="<< theActivity <<")" << std::endl;
  return obs[theActivity];
}

Probability post_observe_state(StatePtr /*x*/) { return 1; }
