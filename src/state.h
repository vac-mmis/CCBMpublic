#pragma once

#include <map>
#include <sstream>
#include <unordered_map>
#include <functional>

#include "util/allocator.h"
#include "util/array.h"

#include "model.h"

struct ApplInfo;
struct StateDataRec;

typedef std::pair<const model::StateRec, StateDataRec> StateTableEntry;
typedef StateTableEntry *StateTablePtr;

#if !HAS_STATE_SPACE_ANALYZER

struct StateDataRec {
    typedef model::Model Model;

    typedef util::array<ApplInfo*, Model::n_agents> appi_t;
    typedef util::array<int, Model::n_agents> appl_t;
    typedef util::array<double, Model::num_goals> heuristicWeight_t;

    double time; // last time, 'pyx' has been computed for state record
#ifndef HAS_CALLBACKS
    Probability pyx;  // prob. of observation given state record
#endif
#if ENDSTAT_REACH_STATES
    bool visited;
#endif

    appi_t appi; // if non-null: Array of applicable ops && reached states
    appl_t appl; // size of appi arrays

    model::StateRec accepted; // used for the landmarkCount heuristic
    heuristicWeight_t heuristicWeight; // used for all distance approximations


    StateDataRec();
    ~StateDataRec();

    bool isDeadEnd(size_t agent) const {
        return appl[agent] == 0;
    }
};


#else // !HAS_STATE_SPACE_ANALYZER

// StateDataRec for the analyzer
struct StateDataRec {
    // save space by using unions for fields that are used in different stages
    union {
        struct { // creation of state space
            unsigned int lastMaxLevel; // the last DFS maxLevel
            unsigned int currentLevel; // current DFS level when this node is expanded
            bool queuedOnStack; // if this node is queued on stack
        };
        struct { // reversing edges
            int reverse_appl; // the size of the reverse_edges array
            ApplInfo *reverse_edges;
            unsigned int numChild; // currently processing child number
        };
        struct { // computing goal distances
            double goalDistance;
            unsigned long heapIndex;
            StateTablePtr next;
        };
    };

    // these arrays are of length 1 because the analyzer always uses only one agent
    ApplInfo *appi[1]; // if non-null: Array of applicable ops && reached states
    int appl[1];       // size of appi array

    StateDataRec();
    ~StateDataRec();

    bool isDeadEnd() const {
        return appl[0] == 0;
    }
};

#endif // !HAS_STATE_SPACE_ANALYZER

typedef std::unordered_map<model::StateRec, StateDataRec,
    model::StateRec_Hash, model::StateRec_Equal,
    pool_allocator<std::pair<const model::StateRec, StateDataRec>> > StateTable;



// TODO: add more info about strategy
struct ApplInfo {
#if !HAS_STATE_SPACE_ANALYZER
    double saliency; // "Priority" of op
    double wSaliency;
    double specificity;
    double wSpecificity;
    double wGoalDistance[NUM_GOALS];
    double weight[NUM_GOALS];
    int opid;        // Id of action
#endif // !HAS_STATE_SPACE_ANALYZER
    StateTablePtr reached;   // pointer to state reached
    ApplInfo() :
#if !HAS_STATE_SPACE_ANALYZER
        saliency(0), wSaliency(0), specificity(0), wSpecificity(0),  opid(model::InitOpId),
#endif // !HAS_STATE_SPACE_ANALYZER
        reached(NULL) {
#if !HAS_STATE_SPACE_ANALYZER
        for (int i=0; i<NUM_GOALS; i++) {
            wGoalDistance[i] = 0;
            weight[i]=0;
        }
#endif // !HAS_STATE_SPACE_ANALYZER
    }
};



// contains statistics about one state
struct StateStatistics {
#if STATESTAT__NPARTICLES
    size_t nParticles;
#endif
#if STATESTAT_MIN_PX
    Probability px; // for determining this state's probability (sum of all particle weights)
#endif

#if STATESTAT__NPARTICLES || STATESTAT_MIN_PX // if needs constructor
    StateStatistics() :
#if STATESTAT__NPARTICLES
        nParticles(0)
#if STATESTAT_MIN_PX
        ,
#endif
#endif
#if STATESTAT_MIN_PX
        px(0)
#endif
    {}
#endif // constructor
};

class StateTablePtr_Equal {
public:
    bool operator()(StateTablePtr const a, StateTablePtr const b) const;
};

class StateTablePtr_Hash {
public:
    size_t operator()(StateTablePtr const a) const;
};

// Map from StateRec to weight
typedef std::unordered_map<model::StateRec, double, model::StateRec_Hash, model::StateRec_Equal> StateWeightMap;

// Map from state to state statistics
typedef std::unordered_map<StateTablePtr, StateStatistics, StateTablePtr_Hash, StateTablePtr_Equal> StateStatMap;

struct States {
    StateTable states;

    // If allowedStates is not NULL, only the states in the set will be visited
    // - others will not be expanded
    StateTable* allowedStates = NULL;

    StateWeightMap weightMap[NUM_GOALS]; // goal distances

    States(const States &s) = delete; // prevent accidential copying, we always want to pass references

    States() = default;
};
