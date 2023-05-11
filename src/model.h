#pragma once

#include <string>

#include "util/array.h"

#include "config.h"
#include "weight.h"

#if STATESTAT_STARTING_TIMES
#include <map>
#endif

/*
 * Declares all model-related functions and variables.
 * This includes especially all names of the generated model file.
 * We require that the user defined / generated model consists of three files.
 * Each file has to be defined as one preprocessor macro (enclosed in double quotes).
 * MODEL_HEADER: pre-processor defines (and potentially other declarations) of the generated model
 * MODEL_IMPL: generated model functions
 * FILTER_PARAMETERS: parameter influencing the filter
 *
 * The MODEL_HEADER must only include non-standard definitions
 * (i.e. those not included here)
 */

#ifndef MODEL_HEADER_DEFINES
#error "MODEL_HEADER_DEFINES not defined"
#endif
#ifndef MODEL_HEADER
#error "MODEL_HEADER not defined"
#endif
#ifndef FILTER_PARAMETERS
#error "FILTER_PARAMETERS not defined"
#endif

#include FILTER_PARAMETERS

namespace model {

// defines from the model
#include MODEL_HEADER_DEFINES

// A struct of only static const members that holds details of this model
// This can be used in templates
// (These are static const because the models compiled here are static const and not changed whatsoever by the program)
// (This allows the compiler to optimize for these models, because it certainly knows some values)
struct Model {
    static const size_t n_agents = NAGENTS;
    static const size_t num_initials = NUM_INITIAL_STATES;
    static const size_t num_goals = NUM_GOALS;

    typedef util::array<int, n_agents> opid_t;
}; // struct Model


// basic types

struct StateRec;
typedef const StateRec *StatePtr;

typedef double (*ActionFunc)(StatePtr, StateRec*, double*);
typedef void (*Callback)(StatePtr s, int agid);
typedef double (*ActionHeuristic)(int rT[ELEMENTS_UNIVERSE], int t, StateRec *s);
typedef bool (*DurationModel)(double,double*);
typedef double (*CDFModel)(double,double);
typedef double (*PDFModel)(double,double);

struct ActionScheme {
    std::string name;
    // <parameter name, type> pairs
    std::vector<std::pair<std::string, std::string> > parameters;

    ActionScheme(const std::string &name, const std::vector<std::pair<std::string, std::string> > &parameters) :
        name(name),
        parameters(parameters)
    { }
};

struct Action {
    int id;
    // the name of the action
    std::string name;

    // the reference to the action scheme of this action
    const ActionScheme *scheme;

    // function pointers

    // the actual execution function
    ActionFunc func;
    // action :observation callback function, may be NULL
    Callback callbackFunc;
    // ACT-R-based heuristic
    ActionHeuristic heuristic;

    DurationModel durationModel;
    CDFModel cdfModel;
    PDFModel pdfModel;
    double durationProb;

    Action(int id, const std::string &name, const ActionScheme *scheme,
            ActionFunc func, Callback callbackFunc, ActionHeuristic heuristic,
            DurationModel durationModel, CDFModel cdfModel, PDFModel pdfModel, double durationProb) :
        id(id),
        name(name),
        scheme(scheme),
        func(func),
        callbackFunc(callbackFunc),
        heuristic(heuristic),
        durationModel(durationModel),
        cdfModel(cdfModel),
        pdfModel(pdfModel),
        durationProb(durationProb)
    { }

    double call(StatePtr x, StateRec *x1, double *specificity) const {
        if (func != NULL)
            return func(x, x1, specificity);
        return 0;
    }

    void callback(StatePtr s, int agid) const {
        if (callbackFunc != NULL)
            callbackFunc(s, agid);
    }
};

// these namespaces will contain named variables of the schemes and actions

namespace ActionSchemes {
typedef std::pair<std::string, std::string> paramType;

// For each action scheme, there will be one instance of ActionScheme (prefixed with "s_"), and one struct definition (prefixed with "S_") that inherits from Action
}

namespace Actions {
// For each action, there will be one variable (prefixed with "a_") of its action scheme struct
}


#include MODEL_HEADER


#if NAGENTS > 1 && MULTI_AGENT_MODE_ENABLED && ! HAS_STATE_SPACE_ANALYZER && ! USE_EXACT_FILTER
#define DO_MULTI_AGENT 1
#endif

// Model-related, these are all in the generated model definition

extern const char * DOMAIN_NAME;
extern const char * PROBLEM_NAME;

std::ostream& operator<<(std::ostream &o, StatePtr x);
void writeState(std::ostream &o, StatePtr x, double wt);
bool readState(std::istream &i, StateRec *x, double &wt);

extern char const * const initialStateNames[];
extern char const * const goalNames[];

extern char const * const objnames_location[];
extern char const * const objnames_object[];

extern char const * const initialStateNames[];
extern char const * const goalNames[];


// Actions

extern int const FinishedOpId;
extern int const NoOpId;
extern int const InitOpId;

extern const ActionScheme* __actionSchemes[];
extern const ActionScheme* *actionSchemes;
extern const Action* __actions[];
extern const Action* *actions;
extern char const * const __actionNames[];
extern char const * const * const actionNames;

// ---------


bool canFollowAfter(int next, int prev);
void stateObservation(StatePtr);

bool isGoalState(size_t goalIndex, StatePtr x);
void sampleInitial(size_t goalIndex, StateRec *x1);


void getDifferences(int rT[ELEMENTS_UNIVERSE], int t,  StatePtr prev, StatePtr curr);
double getRefract(StatePtr a1, StatePtr a2);
int landmarkCountHeuristic(size_t initialState, size_t goalIndex, StatePtr s, StateRec *parentAccepted);


// other, defined in the model.cpp file

int getnops();
int getActionIndexFromName(const std::string&);
const std::string getActionNameFromIndex(int index);

// "single" or "multi" agent mode
extern char const * const magMode;


inline bool stateEquals(StatePtr const s1, StatePtr const s2) {
    return !memcmp(s1, s2, sizeof(StateRec));
}

class StateRec_Equal {
public:
    bool operator()(StateRec const & a, StateRec const & b) const;
};

class StateRec_Hash {
public:
    size_t operator()(StateRec const & a) const;
};

struct StatePtr_Equal {
    bool operator()(StatePtr const s1, StatePtr const s2) const;
};

void writeStateBin(std::ostream &o, StatePtr x, double wt);
bool readStateBin(std::istream &i, StateRec *x, double &wt);

bool getInitialStateIndex(std::string cmdLine, int& intialIndex, std::string& parameter);
bool getGoalIndex(std::string cmdLine, int& goalIndex, std::string& parameter);

bool tableName(std::string cmdLine, std::string stateFiles[NUM_GOALS]);

} // namespace model
