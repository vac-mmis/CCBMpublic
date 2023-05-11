/* Output format for plan information
Time; <* (if new); action name ; pStop(a_t-1); Saliency; wSaliency; Specificity; wSaliency; Goal Distance; wGoalDistance; Recency; wRecency; Refractoriness; wRefractoriness; e^sum > (repeat for each agent)
*/

/*
There are several different combinations of plan timings and observationtiming
We have to distinguish two different cases:
1.) observation data contains time
1.a)  the number of steps in plan file matches number of observation data entries (plan file is 'expanded')
1.a.1)  time matches
        --> everything ok, just take the time from the observation data
1.a.2)  time does not match
        --> report problem, take time from observation data

1.b)  the number of steps in the plan file is less than the number of observation data entries (plan file is not 'expanded')
      --> the plan file has
1.c) the number of steps in plan file is greater than the number of observation data entries (obviously no match)

2.) observation data doesn't contain time
2.a)

*/
#define VALIDATOR

#ifndef USE_ACTR_HEURISTICS
#define USE_ACTR_HEURISTICS
#endif

#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <stdexcept>
#include <algorithm>
#include <cmath>
#include <string>

#include "util/allocator.h"
#include "util/parameter.hpp"

#include "config.h"
#include "global.h"
#include "VParticle.h"
#include "VWeightTable.h"


namespace validator {

/**
 * Class representing the structure of a plan.
 * Doesn't assign any semantic meaning to it.
 */
class Plan {
public:
    struct Action {
        std::string name;
        bool newFlag;
    };
    struct Step {
        double time;
        int inLineNumber;
        std::vector<Action> actions;
    };
    Step operator()(int step);
    Action operator()(int step,int slot);
    std::string actionAt(int step, int slot);
    bool isNew(int step, int slot);
    int numSlots();
    int numSteps();
    double timeAt(int step);
    static Plan fromStream(std::istream&);
    static Plan fromFile(const std::string& path);
private:
    Plan();
    static Step stepFromStream(std::istream&);
    static Action actionFromStream(std::istream&);
    std::vector<Step> steps;
};

enum ValidatorResult {
    VALIDATE_OK_GOAL_REACHED,
    VALIDATE_OK_GOAL_NOT_REACHED,
    VALIDATE_SYNTAX_ERROR,
    VALIDATE_OBSERVATION_ERROR,
    VALIDATE_INVALID_PLAN_ERROR,
    VALIDATE_UNKNOWN_ERROR
};

const std::string ValidatorResultStrings[] = {
    "Ok, goal reached",
    "Ok, goal not reached",
    "Syntax error",
    "Observation error",
    "Invalid plan",
    "Unknown error"
};

class ValidatorError {
public:
    ValidatorError(ValidatorResult r, std::string msg) : result(r), msg(msg) {}
    const ValidatorResult result;
    std::string what() {
        return ValidatorResultStrings[result] + ": " + msg ;
    }
    virtual ~ValidatorError() {};
private:
    std::string msg;
};

std::ostream& operator<<(std::ostream&,const Plan::Step&);
std::ostream& operator<<(std::ostream&,const Plan::Action&);

std::string removeLeadingAndTrailingWhitespace(const std::string&);
model::ActionFunc getAction(int actionIndex);

double initStep(bool);
double nextStep(bool, double);

template <class Model>
int main(int argc, char** argv) {

    std::string o_selectionfile;
    int left = argc;
    bool useObservation = false;
    bool revisiting_factor_set = false;
    double timeBase = 0.0;
    CVweightTable *weightTable = NULL;
    CVActionTable *actionTable = NULL;
    std::string specFile;
    size_t goalIndex=0, initialIndex=0;
    size_t parsed_params;

    add_filter_params_struct s;

    options::option opts;

    try {

        opts.add_help("usage: [OPTIONS] <planfile1> {other planfiles}");

        options::add_filter_params(opts, s);
        options::add_particle_params(opts, revisiting_factor_set);
        options::add_SMC(opts);

        opts.add("a", "save history of action selection to file")
            .parameter("filename", o_selectionfile, [&](std::string, std::string) -> bool { left--; return true; });

        opts.add("h", "set the current heuristic to <factor> (default=0: blind heuristic, 1: landmarks)")
            .parameter("factor", parameters.currentHeuristic);

        opts.add("g", "<value>", "set goal (index or name)",
        [&](std::string, std::vector<std::string> in, size_t start) -> size_t {
            try {
                goalIndex = boost::lexical_cast<int>(in[start]);
            } catch (std::exception &e) {
                for (goalIndex = 0; goalIndex < NUM_GOALS; goalIndex++) {
                    if (in[start] == model::goalNames[goalIndex]) {
                        break;
                    }
                }
            }
            if (goalIndex >= NUM_GOALS) {
                throw std::runtime_error("Goal out of bound: " + in[start] + "(" + std::to_string(goalIndex) + ")");
            }
            return 1;
        });

        opts.add("i", "<value>", "set initial state (index or name)",
        [&](std::string, std::vector<std::string> in, size_t start) -> size_t {
            try {
                initialIndex = boost::lexical_cast<int>(in[start]);
            } catch (std::exception &e) {
                for (initialIndex = 0; initialIndex < NUM_INITIAL_STATES; initialIndex++) {
                    if (in[start] == model::initialStateNames[initialIndex]) {
                        break;
                    }
                }
            }
            if (initialIndex >= NUM_INITIAL_STATES) {
                throw std::runtime_error("Initial State out of bound: " + in[start] + "(" + std::to_string(initialIndex) + ")");
            }
            return 1;
        });


        opts.add("q", "calculates the probability of the next action by taking the <factor> root of the real probability")
            .parameter("factor", parameters.nroot);

        opts.add("o", "enables obervation data evaluation. Data is read from stdin.", &useObservation);

        opts.add("expand-time", "expand time base using deltaT of <factor> (Value must not be smaller than real timebase)")
            .parameter("factor", timeBase);

        opts.add("w", "<filename>", "reads the weights of the single heuristic components from a file, one line per timestep. (other weights specified will be ignored)",
        [&](std::string, std::vector<std::string> in, size_t start) -> size_t {
            left--;
            weightTable = new CVweightTable();
            if (!weightTable->init((char*) in[start].c_str())) {
                throw std::runtime_error("Error in file " + in[start]);
            }
            return 1;
        });

        opts.add("t", "<filename>", "reads the weights of each action from a file, one line per timestep",
        [&](std::string, std::vector<std::string> in, size_t start) -> size_t {
            actionTable = new CVActionTable();
            if (!actionTable->init((char*) in[start].c_str())) {
                throw std::runtime_error("Error in file " + in[start]);
            }
            return 1;
        });

        options::add_help_param(opts);

        parsed_params = opts.parse(argc, argv, options::ERROR_IGNORE);
        if (argc - parsed_params < 1) {
            std::cerr << "No planfile specified!\n";
            std::cerr << "Use --help to show all available options\n";
            return 1;
        }

    } catch(std::exception &e) {
        std::cerr << e.what() << '\n';
        std::cerr << "Use --help to show all available options\n";
        return 1;
    }

    States states;

    if (revisiting_factor_set) // moved to here, because otherwise no "currently" value for the option can be shown
        parameters.revisitingFactor = log(parameters.revisitingFactor);

    if (!s.specificityFile.empty()) {
        StateFunctions::useProvidedSpecificity(StateFunctions::loadSpecificityTable(s.specificityFile.c_str()));
    }

    if (!s.o_statefile.empty()) {
        std::ifstream sfile(s.o_statefile);
        int nr;

        nr = loadStates(states, goalIndex, sfile);
        sfile.close();
        std::clog << "Read " << nr << " state records from " << s.o_statefile << '\n';
    }

    CVParticle<Model> validatorP;
    {
        typename Model::opid_t opid;
        for (size_t i = 0; i < Model::n_agents; i++)
            opid[i] = model::InitOpId;
        typename ::State<Model>::startTime_t startTime;
        for (size_t i = 0; i < Model::n_agents; i++)
            startTime[i] = Global::time;

        model::StateRec xrec;
        model::sampleInitial(initialIndex, &xrec);
        StateTablePtr modelState = &(*(states.states.emplace(xrec, StateDataRec()).first));

        ::State<Model> initState(initialIndex, goalIndex, modelState, opid, startTime);
        validatorP = CVParticle<Model>(initState, Weight(1));
    }
    validatorP.setActionTable(actionTable);
    validatorP.header(std::cout);

    // TODO: determine the time base
    for (int arg=parsed_params; arg < argc; ++arg) {
        try {
            std::string planfile=argv[arg];
            Plan plan = Plan::fromFile(planfile);
            std::cerr << "Validating plan: " << (planfile) << '\n';

            if (weightTable) {
                weightTable->adjustWeights();
            }
            //TODO: read observation here and check for timing
            Global::time = initStep(useObservation);

            StepInfo::pyx = LogProbability(validatorP.get_pyx());

            if (Model::n_agents != plan.numSlots()) {
                std::ostringstream oss;
                oss << "Invalid number of slots in plan, assumed: " << Model::n_agents << ", found: "<<  plan.numSlots() << std::endl;
                throw ValidatorError(VALIDATE_INVALID_PLAN_ERROR,oss.str());
            }

            int planStep=0; // step in the plan file (number of file entry)
            Global::timeSteps.push_back(0); // current time in steps
            bool atEnd=false;

            while (!atEnd) { // run until plan file end
                // set global time to support duration evaluation
                double step = 0.0;
                while (planStep < plan.numSteps() && plan.timeAt(planStep + 1) > plan.timeAt(planStep) + step) {
                    // expand

                    // cerr << "Time: " << Global::time << " + " << step << " = " << Global::time + step<< '\n';
                    //TODO: check if time means timestep here
                    Global::timeSteps.push_back(plan.timeAt(planStep));
                    for (int slot = 0; slot<plan.numSlots(); ++slot) { // check action for each agent
                        // get information from plan
                        bool isNew = 0.0 == step ? plan.isNew(planStep,slot) : false;
                        int actionIndex = model::getActionIndexFromName(plan.actionAt(planStep,slot));
                        validatorP.setaction(slot, plan.actionAt(planStep,slot), actionIndex, isNew);

                        if (model::InitOpId == actionIndex && (0 == planStep || !isNew)) {
                            // initialize: works only at the beginning
                            validatorP.setaction(slot, plan.actionAt(planStep,slot), actionIndex, false);
                        } else if (model::NoOpId == actionIndex) {
                            // BLOCKED
                            // TODO: check if there is no applicable action now
                            // nothing possible except for blocked,
                            // blocked is always new, and should be testet so
                            validatorP.setaction(slot, plan.actionAt(planStep,slot), actionIndex, true);
                        } else if (model::FinishedOpId == actionIndex) {
                            // FINISHED
                        }
                    }
                    if (!validatorP.validateStep(states, useObservation)) {
                        // another action or timestep
                        std::ostringstream oss;
                        int slot=0;
                        // TODO: change slot to the real slot
                        oss << "Failed to meet preconditions of action: " << plan.actionAt(planStep,slot) << '\n'
                            << "In step: " << plan(planStep) << std::endl;
                        throw std::runtime_error(oss.str());
                    }

                    if (weightTable) {
                        if (!weightTable->next()) {
                            std::ostringstream oss;
                            oss << "Missing entry weight table." << std::endl;
                            throw std::runtime_error(oss.str());
                        }
                        weightTable->adjustWeights();
                    }

                    if (actionTable) {
                        if (!actionTable->next()) {
                            std::ostringstream oss;
                            oss << "Missing entry action table." << std::endl;
                            throw std::runtime_error(oss.str());
                        }
                    }
                    validatorP.print(Global::time, std::cout);
                    if (0.0 == timeBase) {
                        step = plan.timeAt(planStep+1);
                    } else { // expand
                        step += timeBase;
                    }

                    Global::time += timeBase;
                    nextStep(useObservation, plan.timeAt(planStep));
                }

                planStep++;
                Global::time = plan.timeAt(planStep);

                atEnd = planStep >= plan.numSteps();
            }

            model::StatePtr state = validatorP.getStatePtr();
            bool goal = model::isGoalState(goalIndex, state);
            std::cerr << "Success! Goalstate " << (goal ? "reached" : "not reached")
                      << '\n';
            if (!goal) {
                std::cerr << state << '\n';
            }

            if (!o_selectionfile.empty())
                validatorP.printHistory(o_selectionfile.c_str());

        } catch (std::runtime_error& e) {
            std::cerr <<  e.what() << std::endl;
            return -1;
        } catch (ValidatorError& e) {
            std::cerr << e.what() << std::endl;
            return e.result;
        }
    }
    std::cerr << "Final log-likelihood:" << p_logValue(StepInfo::fll) << '\n';

    return 0;
}

double initStep(bool useObservation) {
    Global::time = -1;
    if (useObservation) {
        double t = Global::time;
        if (!model::fetchObservation()) {
            std::ostringstream oss;
            oss << "Invalid observation data at initial reading" << std::endl;
            throw std::runtime_error(oss.str());
        }
        if (t != Global::time) { // sensor data contains time
            return Global::time;
        }
    }
    // either no observation data or no time included
    return Global::time = 0.0;
}

double nextStep(bool useObservation, double planTime) {
    if (useObservation) {
        double t = Global::time;
        if (!model::fetchObservation()) {
            std::ostringstream oss;
            oss << "Invalid observation data at time " << Global::time  << std::endl;
            throw std::runtime_error(oss.str());
        }
        if (t != Global::time) { // sensor data contains time
            return Global::time;
        }
    }
    // either not sensor data or no time included
    return planTime;
}

std::ostream& operator<<(std::ostream& os, const Plan::Step& step) {
    os << "<line " << step.inLineNumber << " step: " << step.time;
    for (unsigned int i=0; i<step.actions.size(); ++i) {
        os << "," << step.actions[i];
    }
    return os << ">";
}

std::ostream& operator<<(std::ostream& os, const Plan::Action& action) {
    return os << (action.newFlag ? "*" : "") << "," << action.name;
}

std::string Plan::actionAt(int step, int slot) {
    return (*this)(step,slot).name;
}

bool Plan::isNew(int step, int slot) {
    return (*this)(step,slot).newFlag;
}

int Plan::numSlots() {
    if (steps.empty()) {
        return 0;
    }
    return steps[0].actions.size();
}

int Plan::numSteps() {
    return steps.size();
}

double Plan::timeAt(int step) {
    if (((size_t)step) >= steps.size()) {
        return 0;
    }
    return steps[step].time;
}

Plan::Step Plan::operator()(int step) {
    return steps[step];
}

Plan::Action Plan::operator()(int step, int slot) {
    return steps[step].actions[slot];
}

/**
 * read an action from a stream
 * -- action should start with the flag field,
 *  without any leading separators, and end with the
 *  action-name field, that must either end with a separator
 *  or be the last field on the line
 */
Plan::Action Plan::actionFromStream(std::istream& in) {
    Action result;
    std::string flags;
    std::getline(in,flags,',');
    if (flags.find("*")!=std::string::npos) {
        result.newFlag = true;
    } else {
        result.newFlag = false;
    }
    std::getline(in,result.name,',');
    result.name = removeLeadingAndTrailingWhitespace(result.name);
    if (result.name.empty()) {
        throw std::runtime_error(std::string(__FUNCTION__)+":Empty action name field!");
    }

    return result;
}

/**
 * reads a step from a stream
 * the stream should only contain that step
 */
Plan::Step Plan::stepFromStream(std::istream& in) {
    Plan::Step result;
    in >> result.time;
    if (in.fail()) {
        //reading the time failed
        throw std::runtime_error(std::string(__FUNCTION__)+":Could not read time");
    } else {
        //discard until next separator
        std::string discard;
        std::getline(in,discard,',');
        try {
            //read actions if there is anything other than whitespace left
            while ((in >> std::ws).rdbuf()->in_avail()>0) {
                result.actions.push_back(Plan::actionFromStream(in));
            }
        } catch (std::runtime_error& e) {
            //there was some error during reading actions
            throw std::runtime_error(std::string(__FUNCTION__)+":"+e.what());
        }
    }
    return result;
}

/**
 * Reads a plan from a stream
 * -- the stream should only contain that plan,
 *  with steps separated by newlines
 */
Plan Plan::fromStream(std::istream& in) {
    Plan plan;
    int lineNo = 0;
    std::string commentStarter="#";
    std::string line;
    //FIXME:are negative times allowed?
    //if so, need to do this differently
    double lastTime = -1;
    while (std::getline(in >> std::ws,line)) {
        ++lineNo;
        line = line.substr(0,line.find_first_of(commentStarter));
        if (line.empty()) {
            continue;
        }
        std::istringstream line_stream(line);
        try {
            Plan::Step step = Plan::stepFromStream(line_stream);
            if (step.time < lastTime) {
                throw std::runtime_error("Time goes backwards!");
            }
            lastTime = step.time;
            step.inLineNumber = lineNo;
            plan.steps.push_back(step);
        } catch (std::runtime_error& e) {
            std::ostringstream oss;
            oss << __FUNCTION__ << ":" << e.what() << '\n'
                << "\toffending line: "<< lineNo << std::endl;
            throw ValidatorError(VALIDATE_SYNTAX_ERROR,oss.str());
        }
    }
    return plan;
}

/**
 * Attempts to read a plan from the file
 * at the given path
 */
Plan Plan::fromFile(const std::string& path) {
    try {
        std::ifstream file(path.c_str());
        if (!file.is_open()) {
            throw std::runtime_error(std::string("Could not open file: ") + path);
        }
        return Plan::fromStream(file);
    } catch (std::runtime_error& e) {
        throw std::runtime_error(std::string(__FUNCTION__)+":"+e.what());
    }
}

Plan::Plan() {}

std::string removeLeadingAndTrailingWhitespace(const std::string& string) {
    const std::string whitespace = " ";
    std::size_t begin = string.find_first_not_of(whitespace);
    if (begin == std::string::npos) {
        return "";
    }
    std::size_t end = string.find_last_not_of(whitespace);
    return string.substr(begin, end - begin + 1);
}

//A non-action whose preconditions are always met
double dummyAction(model::StatePtr inState, model::StateRec *outState, double* spec) {
    *spec = 0.0;
    *outState = *inState;
    return 1;
}


/**
 * Returns an action for the given index
 * (dummy action for system actions)
 */
model::ActionFunc getAction(int actionIndex) {
    if (actionIndex < 0) {
        //system action, ignore
        return dummyAction;
    } else {
        return model::actions[actionIndex]->func;
    }
}

} // namespace validator


int main(int argc, char** argv) {
    std::ios::sync_with_stdio(false);
    int result = validator::main<model::Model>(argc, argv);
    pool_allocator_base::free_all();
    return result;
}
