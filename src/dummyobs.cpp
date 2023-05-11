/* Dummy observation definitions for RC */

#ifndef USE_ACTION_OBSERVATION

bool fetchObservation() {
    return true;
}

void pre_observe_action(size_t /*agid*/) { }
void pre_observe_state(StatePtr) { }

Probability post_observe_action(size_t /*agid*/) {
    return 1;
}
Probability post_observe_state(StatePtr) {
    return 1;
}


#else // USE_ACTION_OBSERVATION

#include <string>
#include <iostream>
#include <sstream>
#include <stdexcept>

// TODO extend to multi-agent case
int simulatedActionId;
int observedActionId;

std::string getActionFromStream(std::istream& in) {
    std::string line;
    double time;
    std::string action;

    std::getline(in >> std::ws,line);
    // Maybe check for comments
    std::istringstream line_stream(line);

    // skip time
    line_stream >> time;

    // skip isnew flag
    std::getline(line_stream,action,',');
    std::getline(line_stream,action,',');
    // now we should find the action
    std::getline(line_stream,action,',');

    const std::string whitespace = " ";
    std::size_t begin = action.find_first_not_of(whitespace);
    if (begin == std::string::npos) {
        return "";
    }
    std::size_t end = action.find_last_not_of(whitespace);
    return action.substr(begin, end - begin + 1);
}


void actionCallback(const Model::opid_t &opid) {
    // TODO extend to multi-agent case: simply copy opid
    simulatedActionId = opid[0];
}

// read in some plan syntax
bool fetchObservation() {
    std::string action = getActionFromStream(std::cin);

    std::cerr << "Read " << action << std::endl;

    if (std::cin.fail()) {
        return false;
    }

    try {
        observedActionId = getActionIndexFromName(action);
    } catch (std::runtime_error& e) {
        observedActionId = MISSING_ACTION;
    }

    return true;
}


void pre_observe_action(size_t /*agid*/) {
    simulatedActionId = INVALID_ACTION;
}

void pre_observe_state(StatePtr) { }

Probability post_observe_action(size_t /*agid*/) {
    static double p_missing = 0.0;
    static double p_incorrect = 0.0;
    static double p_correct = 0.0;
    static bool renew = true;

    if (renew) {
        p_missing = (MISSING_PROB) * (1 - NOISE_PROB);
        p_correct = (MISSING_PROB * NOISE_PROB * 1/(double)getnops()) + ((1 - MISSING_PROB) * ((1-NOISE_PROB) + NOISE_PROB * 1/(double)getnops()));
        p_incorrect = (MISSING_PROB * NOISE_PROB * ((double)getnops()-1)/(double)getnops()) + ((1-MISSING_PROB) * NOISE_PROB * ((double)getnops() - 1)/(double)getnops());
        p_incorrect /= (double)getnops();
        renew = false;
        std::cerr << "Using p_m=" << p_missing << ", p_c=" << p_correct << ", p_i=" << p_incorrect << std::endl;
    }
    // for details, see problem statement

    if (INVALID_ACTION == simulatedActionId) {
        throw std::runtime_error("missing simulated action");
    }

    // check for missing action
    if (MISSING_ACTION == observedActionId) {
        // a missing action is always right
        return p_missing;
    }

    if (observedActionId == simulatedActionId) {
        // right action observed
        return p_correct;
    }
    // wrong action
    return p_incorrect;
}

Probability post_observe_state(StatePtr) { return 1; }

#endif // USE_ACTION_OBSERVATION
