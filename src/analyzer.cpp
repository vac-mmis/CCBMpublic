#include <cstdlib>
#include <iostream>
#include <fstream>
#include <unistd.h>
#include <stack>
#include <vector>
#include <set>

#define HAS_STATE_SPACE_ANALYZER 1

#include "util/allocator.h"
#include "util/parameter.hpp"

#include "config.h"
#include "global.h"
#include "queue.h"
#include "stateFunctions.h"

using namespace std;

#ifndef IDFS_INC
#define IDFS_INC 5
#endif
#if IDFS_INC < 1
#error "IDFS_INC must be greater than 0"
#endif

template<>
inline double getKey(StateTablePtr v) {
    return v->second.goalDistance;
}
template<>
inline unsigned long getIndex(StateTablePtr v) {
    return v->second.heapIndex;
}
template<>
inline void setIndex(StateTablePtr v, unsigned long i) {
    v->second.heapIndex = i;
}
template<>
inline int numChildren(StateTablePtr v) {
    return v->second.appl[0];
}
template<>
inline StateTablePtr getChild(StateTablePtr v, int i) {
    return v->second.appi[0][i].reached;
}
template<>
inline double getWeight(StateTablePtr, int) {
    return 1.0;
}
template<>
inline double getDistance(StateTablePtr v) {
    return v->second.goalDistance;
}
template<>
inline void setDistance(StateTablePtr v, double k) {
    v->second.goalDistance = k;
}
template<>
inline void setNext(StateTablePtr v, StateTablePtr u) {
    v->second.next = u;
}
template<>
inline void showInfo(const char *what, StateTablePtr v) {
    cout << what << &v->first << '\n';
}

inline StateTablePtr getNext(StateTablePtr v) {
    return v->second.next;
}

namespace analyzer {

namespace AnalyzerGlobals {
std::vector<int> actionApplicableCount(NOPS,0);
}


unsigned int cutoff = 10000000;
unsigned int cutoffLevel = 100; // the global maximum DFS level
unsigned int maxLevel; // the current maximum DFS level
unsigned int goalDepth = -1;
unsigned int numDead = 0;
double stateHist[NOPS+1]; // we have NOPS+1 numbers of applicable ops: 0 .. NOPS
StateTablePtr initialState[NUM_INITIAL_STATES] = {NULL};
StateTablePtr *backpointers; // memory for inverted pointers
std::shared_ptr<std::ostream> goalDistOut; // goal distance output stream (-o option)
std::shared_ptr<std::ostream> planOut; // plan output stream (-p option)

// forward declarations
bool traverse(States &states, int goalIndex);
bool traverseNext(States &states, int goalIndex, StateTablePtr gptr);
void computeGoalDistances(States &states, int goalIndex);
void printPredecessorCount(const std::string&);
void printPath(std::ostream &o, StateTablePtr v);
void saveStates(States &states, std::ostream &o);
void routerStats(States &states, std::ostream &o);



class Stats {
    const char* what;
    double m1, m2, m3, m4,min,max;
    double n;
public:
    Stats(const char *_what) : what(_what) {}
    void init() {
        n=0;
        m1=0;
        m2=0;
        m3=0;
        m4=0;
        min=+infinity;
        max=-infinity;
    }
    void collect1(double v) {
        collect(v,1.0);
    }
    void collect(double v, double w) {
        double v2=v*v;
        n += w;
        m1+= w*v;
        m2+=w*v2;
        m3+=w*v*v2;
        m4+=w*v2*v2;
        if (w>0) {
            if (v<min) min=v;
            else if (v>max) max=v;
        }
    }
    void finish(std::ostream &o) {
        double mean = m1/n;
        double mm   = mean*mean;
        double var  = m2/n - mm;
        double sd   = sqrt(var);
        o << "Statistics of " << what
          << ":\n n=" << n
          <<", min=" << min
          <<", max=" << max
          <<", mean=" << mean
          <<", var=" << var
          <<", sd=" << sd
          <<", skew=" << ((m3/n - (mm*mean)) / (var*sd))
          <<", curt=" << ((m4/n - (mm*mm)) /  (var*var))
          << '\n';
    }
};

int main(int argc, char *argv[]) {
    int goalIndex = 0;
    std::shared_ptr<std::ostream> actionApplicableFile;
    std::shared_ptr<std::istream> allowedStatesFile;

    options::option opts;
    try {
        opts.add("restrict-states", "restrict states to be visited to states in file")
            .parameter("file", allowedStatesFile);

        opts.add("n", "discover at most <number> states")
            .parameter("number", cutoff, true);

        opts.add("d", "limit search depth to <number> levels")
            .parameter("number", cutoffLevel, true);

        opts.add("a", "Write action applicability information to <filename>")
            .parameter("file", actionApplicableFile);

        opts.add("g", "<goal>", "set goal to search (index or name)",
        [&](std::string, std::vector<std::string> in, size_t start) mutable -> size_t {
            try {
                goalIndex = boost::lexical_cast<int>(in[start]);
            } catch (std::exception &e) {
                for (goalIndex = 0; goalIndex < NUM_GOALS; goalIndex++) {
                    if (in[start] == model::goalNames[goalIndex]) {
                        break;
                    }
                }
            }
            if (goalIndex < 0 || goalIndex >= NUM_GOALS) {
                throw std::runtime_error("Goal out of bound: " + in[start] + "(" + std::to_string(goalIndex) + ")");
            }
            return 1;
        });
        
        opts.add("o", " write output to <file>, use \"-o -\" for stdout")
            .parameter("file", goalDistOut);
        
        opts.add("p", " write shortest plan to <file>, use \"-o -\" for stdout")
            .parameter("file", planOut);

        options::add_help_param(opts);

        if (argc - opts.parse(argc, argv) != 0) {
            std::cout << "Too many arguments!\n";
            std::cout << "Use --help to show all available options\n";
            return 1;
        }

    } catch(std::exception &e) {
        std::cout << e.what() << '\n';
        std::cout << "Use --help to show all available options\n";
        return 1;
    }

#if VERBOSITY > 0
    cout << "State Space Analyzer for domain: \"" << model::DOMAIN_NAME << '"'
         << ", problem: \"" << model::PROBLEM_NAME << "\"\n";
#endif

    States states;

    if (allowedStatesFile) { // read states from stream
        size_t nrec = loadAllowedStates(states, *allowedStatesFile);
        cout << "read " << nrec << " states\n";
    }

    bool isComplete;
    try {
        isComplete = traverse(states, goalIndex);

        if (actionApplicableFile) {
            *actionApplicableFile << model::DOMAIN_NAME << ";" << model::PROBLEM_NAME << "; " << states.states.size() << '\n';
            for (size_t i = 0; i < AnalyzerGlobals::actionApplicableCount.size(); ++i) {
                *actionApplicableFile << i << " : " << model::actions[i]->name << " : " << AnalyzerGlobals::actionApplicableCount[i] << '\n';
            }
        }

        if (goalDistOut || planOut) {
            computeGoalDistances(states, goalIndex);
        }
        
    } catch (const exception& e) {
        cout << "Exception: " << e.what() << '\n';
        return 1;
    }

    return isComplete ? 0 : 2;
}

std::ostream& operator<<(std::ostream &o, Stats &s) {
    s.finish(o);
    return o;
}


bool traverse(States &states, int goalIndex) {
    Stats bfstats("branching factor (fan out)");
    int i;
    model::StateRec xrec;
    bool isComplete = false;

    numDead = 0;
    for (i = 0; i < NOPS + 1; i++) stateHist[i] = 0;

    for (int initialIndex = 0; initialIndex < NUM_INITIAL_STATES; initialIndex++) {
        model::sampleInitial(initialIndex, &xrec);
        auto insResult = states.states.emplace(xrec, StateDataRec());
        initialState[initialIndex] = &(*(insResult.first));
        try {
            // iteratively increase maximum DFS level until state space has been explored
            // or the global cutoffLevel has been reached
            maxLevel = IDFS_INC;
            if (maxLevel > cutoffLevel)
                maxLevel = cutoffLevel;
            do {
                cout << "starting DFS with max depth of " << maxLevel << std::endl;
                isComplete = traverseNext(states, goalIndex, initialState[initialIndex]);
                cout << "discovered " << states.states.size() << " states at level " << maxLevel << std::endl;

                if (maxLevel >= cutoffLevel || states.states.size() >= cutoff)
                    break;
                if (maxLevel + IDFS_INC > cutoffLevel)
                    maxLevel = cutoffLevel;
                else
                    maxLevel += IDFS_INC;
            } while (!isComplete && maxLevel <= cutoffLevel);
        } catch (const exception& e) {
            cout << "traversal finished because: " << e.what() << '\n';
        }
    }

    bfstats.init();
    bfstats.collect(0,stateHist[0]);
    cout << "c(" << stateHist[0];
    for (i = 1; i < NOPS + 1; i++) {
        cout << ',' << stateHist[i];
        bfstats.collect(i,stateHist[i]);
    }
    cout << ")\n";

    cout << "*** Traversal statistics ***"
         << (isComplete ? "" : " *** TRAVERSAL INCOMPLETE ***") << '\n'
         << bfstats;

    if (goalDepth != (unsigned int) -1 )
        cout << ", shortest DFS path to goal: " << goalDepth << '\n';
    else
        cout << ", no goal found\n";

    cout << "   nDead=" << numDead << std::endl;
    //bfstats.finish(cout);

    return isComplete;
}

// reverse edges of the state space graph
// try to reverse edges in-place: only if cycles are detected, appi and reverse_edges are both holding values
// otherwise, one of them will be NULL
void transpose(StateTablePtr startNode) {
    // iterative version
    // the recursive variant looks like:
    //   for (child in children)
    //     if (child->appl > 0) transpose(child);
    //     reverse_edges_to(child)
    //   node->appl = 0; ...
    // The basic principle for the iterative version is:
    //   while(node != NULL)
    //     if (child was processed)
    //       reverse_edges_to(child)
    //     if (has more children)
    //       next child
    //       if (child->appl > 0)
    //         // transpose child
    //         node = child;
    //       continue; // executes reverse_edges_to(child)
    //     node->appl = 0; ...

    if (startNode->second.reverse_edges) {
        return;
    }

    StateTablePtr node = startNode;

    stack<StateTablePtr> parents;

    while (true) { // break statement is at the end of the loop body
        StateDataRec &d = node->second;
        if (!d.reverse_edges) {
            // first visit of this non-leaf node
            // use invalid pointer value 1 to indicate node has been processed, but reverse_edges has not yet been initialized
            // this allows to save memory: allocate array only when first edges are reversed
            d.reverse_edges = (ApplInfo*) 1;
            d.numChild = 0;
        } else {
            // we transposed child d.numChild, now reverse edges to it
            StateTablePtr child_node = d.appi[0][d.numChild].reached;
            StateDataRec &child = child_node->second;
            if (child.reverse_edges <= (void*) 1) {
                // reverse_edges has not yet been initialized
                if (child.reverse_edges == NULL) {
                    // this is a leaf node (if not, transpose would have been called and set reverse_edges to 1)
                    // -> delete old appi, we do not need it anymore
                    delete[] child.appi[0];
                    child.appi[0] = NULL;
                }
                // else we have discovered a cycle: appi must be deleted on exit

                child.reverse_edges = new ApplInfo[child.reverse_appl];
                // reverse_appl is now a counter of the next backpointer to set in reverse_edges
                child.reverse_appl = 0;
            }

            child.reverse_edges[child.reverse_appl++].reached = node;

            // process next child node
            d.numChild++;
        }

        if (d.numChild < (unsigned int) d.appl[0] && d.appl[0] > 0) {
            StateTablePtr child_node = d.appi[0][d.numChild].reached;
            StateDataRec &child = child_node->second;

            if (child.appl[0] > 0 && !child.reverse_edges) {
                // child is not a leaf node, transpose child first (if not already processed)
                parents.push(node); // save where to continue
                node = child_node;
            }
            // at this point, child is either
            // * a leaf node or
            // * a non-leaf node in a cycle
            //   - in both cases, "continue" will reverse edges to this child, or
            // * an unprocessed non-leaf node
            //   - "continue" will process child then first (!d.reverse_edges is true)
            continue;
        }

        // this node has been processed, and incoming edges can be reversed
        // essentially, make it a leaf node in the old graph with non-reversed edges
        d.appl[0] = 0;
        if (d.appi[0]) {
            // if this node was part of a cycle, delete appi now
            delete[] d.appi[0];
            d.appi[0] = NULL;
        }

        // if this node is not part of a cycle, we must initialize reverse_edges here
        // especially, if this is the initial node - otherwise, reverse_edges would keep its
        // invalid value 1
        if (d.reverse_edges == (void*) 1) {
            d.reverse_edges = new ApplInfo[d.reverse_appl];
            // reverse_appl is now a counter of the next backpointer to set in reverse_edges
            d.reverse_appl = 0;
        }

        // continue with parent (recursive ascent)
        if (parents.empty())
            break;
        node = parents.top();
        parents.pop();
    }
}

void computeGoalDistances(States &states, int goalIndex) {
    unsigned long ntotal = 0;

    // init fields for reversing edges and count goal states
    unsigned long ngoals = 0;
    for (auto &e: states.states) {
        StateDataRec &d = e.second;
        if (isGoalState(goalIndex, &e.first)) {
            ++ngoals;
        }
        d.numChild = 0;
        d.reverse_appl = 0;
        d.reverse_edges = NULL;
    }
    
    cout << "Computing goal distance with " << ngoals << " goals ..." << std::endl;

    // compute reverse_appl field: how many incoming edges each node has
    for (auto &e: states.states) {
        StateDataRec &d = e.second;
        for (int i = 0; i < d.appl[0]; i++) {
            StateDataRec &other = d.appi[0][i].reached->second;
            other.reverse_appl++;
            ntotal++;
        }
    }

    cout << "state graph has " << ntotal << " edges" << std::endl;

    for (int initialIndex=0; initialIndex < NUM_INITIAL_STATES; initialIndex++) {
        // reverse all edges
        transpose(initialState[initialIndex]);
    }
        
    // init fields for computing goal distances
    // set appl to reverse_appl and appi to reverse_edges
    // both reverse_* fields share memory in a union with
    // the goalDistance fields used by dijkstra below
    for (auto &e: states.states) {
        StateDataRec &d = e.second;

        d.appi[0] = d.reverse_edges;
        d.appl[0] = d.reverse_appl;

        if (isGoalState(goalIndex, &e.first))
            setDistance(&e, 0);
        else
            d.goalDistance = infinity;

        d.heapIndex = 0;
        d.next = NULL;
    }

    // perform calculation of goal distances
    dijkstra<StateTablePtr, StateTable>(states.states);
    routerStats(states, cout);
    if (planOut)
        printPath(*planOut, initialState[0]);
    if (goalDistOut)
        saveStates(states, *goalDistOut);
}

/**
 * Iterative DFS on the state space.
 *
 * Returns true, if the state space has been successfuly explored, or false if the current maxLevel has been reached
 */
bool traverseNext(States &states, int goalIndex, StateTablePtr startNode) {
    // iterative implementation of the tail-recursive variant, which looks like:
    //   if (level > maxLevel) return false;
    //   expand();
    //   for (child in children) result &= traverseNext(level+1, child)

    stack<StateTablePtr> stack;
    stack.push(startNode);

    // result is the accumulator of the result: if any children evaluates to false, we
    // will return false at the end. But so far, we will only stop descending further,
    // and not stop the whole function.
    bool result = true;

    StateDataRec &d = startNode->second;
    d.currentLevel = 0;
    d.lastMaxLevel = maxLevel;

    unsigned int level;
    while (!stack.empty()) {
        StateTablePtr state = stack.top();
        stack.pop();
        StateDataRec &d = state->second;
        d.queuedOnStack = false;

        level = d.currentLevel;

        if (isGoalState(goalIndex, &state->first) && level < goalDepth) {
#if VERBOSITY > 1
            cout << "Dicovered goal at level " << level << '\n';
#endif
            goalDepth = level;
        }

        if (d.appl[0] < 0) {
            expandState(states, 0, state);
            stateHist[d.appl[0]] += 1;
            unsigned int numStates = states.states.size();
            if (numStates % 10000 == 0)
                cout << numStates << std::endl;

            if (d.appl[0] == 0)
                numDead++;

            if (isGoalState(goalIndex, &state->first)) {
#if VERBOSITY > 1
                cout << "Dicovered new goal at level " << level << '\n';
#endif
                if (level < goalDepth) {
                    goalDepth = level;
                }
            }

            if (numStates >= cutoff)
                return false;

            for (int i = 0; i < NOPS; ++i) {
                model::StatePtr sPtr = &state->first;
                model::StateRec dummyOut;
                double dummyWeight;
                if (model::actions[i]->call(sPtr, &dummyOut, &dummyWeight)>0) {
                    AnalyzerGlobals::actionApplicableCount[i] += 1;
                }
            }
        }

        // we expanded now states up to level currentLevel+1
        if (level+1 >= maxLevel) {
            if (d.appl[0] > 0)
                result = false;
            // do not queue nodes on deeper levels
            continue;
        }

        for (int i = d.appl[0]; i-->0;) {
            StateTablePtr child_t = d.appi[0][i].reached;
            StateDataRec &child = child_t->second;

            if (child.lastMaxLevel == maxLevel) {
                // this node has already been queued
                if (child.currentLevel <= level + 1)
                    // we know shorter paths
                    continue;

                // this is a short path to this node
                // -> update level of node
                child.currentLevel = level + 1;
                if (!child.queuedOnStack) {
                    // only push again if this not is not already on stack
                    child.queuedOnStack = true;
                    stack.push(child_t);
                }
            } else {
                // this node must be explored
                if (child.currentLevel < level + 1)
                    // we know shorter paths in the future
                    // here, we use < instead of <=, because we must explore this node
                    // at least once - above, we have already explored it because lastMaxLevel == maxLevel
                    continue;

                child.lastMaxLevel = maxLevel;
                child.currentLevel = level + 1,
                       child.queuedOnStack = true;
                stack.push(child_t);
            }
        }
    }

    return result;
}


void printPath(std::ostream &o, StateTablePtr v) {
    o << &v->first << '(' << ::getDistance(v) << ")\n";
    if (getNext(v)) {
        o << "->";
        printPath(o, getNext(v));
    }
}


void routerStats(States &states, std::ostream &o) {
    Stats dist("goal distance"), child("fan in");
    unsigned long ninfinite = 0;

    dist.init();
    child.init();
    for (auto &e: states.states) {
        if (::getDistance(&e) < infinity) {
            dist.collect1(::getDistance(&e));
        } else {
            ninfinite++;
        }
        child.collect1(e.second.appl[0]);
    }
    o << "*** Goal distance statistics ***\n"
      << dist;
    if (ninfinite > 0)
        o << " ** " << ninfinite << " states not reaching goal\n";
    o << child;
}

void saveStates(States &states, std::ostream &o) {
    o << "RCStateTable " << model::DOMAIN_NAME << ' ' << model::PROBLEM_NAME << ' ' << sizeof(model::StateRec) << ' ' << sizeof(double) << '\n';
    for (auto &e: states.states) {
        model::writeStateBin(o, &e.first, ::getDistance(&e));
    }
}

} // namespace analyzer

int main(int argc, char** argv) {
    std::ios::sync_with_stdio(false);
    int result = analyzer::main(argc, argv);

    pool_allocator_base::free_all();

    return result;
}
