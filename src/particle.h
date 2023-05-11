#pragma once
#include <algorithm>

#include <cassert>
#include <unordered_set>
#include <unordered_map>
#include <deque>

#include "util/iterators.h"
#include "util/distribution.h"

#include "global.h"
#include "obsmodel.h"
#include "stateFunctions.h"
#include "particle_common.h"
#include "VActionTable.h"

namespace particle {

namespace detail {

template <class Model>
struct ParticleState : ::State<Model> {
#if DO_MULTI_AGENT
    bool deadEnd[Model::n_agents]; // only if all agents reached a dead-lock, the particle itself is in a deadlock
#endif

    double time; // when this state has been created (for the history list)

    ParticleState() :
        ::State<Model>(0, 0, NULL, typename ::State<Model>::opid_t(), typename ::State<Model>::startTime_t()),
        time(Global::time)
    {
#if DO_MULTI_AGENT
        for (size_t agid = 0; agid < Model::n_agents; agid++)
            deadEnd[agid] = false;
#endif
    }


    ParticleState(const ::State<Model> &state) :
        ::State<Model>(state),
        time(Global::time)
    {
#if DO_MULTI_AGENT
        for (size_t agid = 0; agid < Model::n_agents; agid++)
            deadEnd[agid] = false;
#endif
    }

    bool isGoalState() const {
        return ::isGoalState(this->goalIndex(), this->modelState);
    }

#if ! DO_MULTI_AGENT
    bool isDeadEnd() const {
        return this->modelState->second->isDeadEnd();
    }
#else
    bool isDeadEnd(size_t agid) const {
        return this->modelState->second->isDeadEnd(agid);
    }
#endif
}; // class ParticleState

} // namespace particle::detail


/* a history data record is written each time, when at least
   one agent changes its action (this includes choosing "being
   blocked". For all agents that have chosen a new action,
   we have 'history->time == history->agents[i].startTime'
 */
typedef std::unordered_set<StateTablePtr,
    std::hash<StateTablePtr>, std::equal_to<StateTablePtr>,
    pool_small_allocator<StateTablePtr> > HistoryTable;


class Particle {
public: // typedefs and static constants

    // no additional statistics
    struct Statistics {
        typedef std::false_type enabled;
        void collect(const Particle&) { }
        static std::ostream& header(std::ostream &os) { return os; }
        friend std::ostream& operator<<(std::ostream &os, const Statistics&) { return os; }
    };

    typedef model::Model Model;

    typedef detail::ParticleState<Model> State;

    // A suitable set implementation that collects instances of this Particle
    // a ParticleSet is used to collect and merge identical particles - this is the actual marginalisation in the marginal filter
    // C++11: we might want to use a template <> using directive here
    template <class P = Particle*>
    struct ParticleSet : std::unordered_set<P, ParticlePtrState_Hash<P>, ParticlePtrState_Equal<P> > { };

    // A suitable map implementation that takes Particle* as a key (where Particles are considered equivalent based solely on their state), and T as a value
    // Note: this is necessary because not every Particle class may define a suitable hash function for their state
    // C++11: we might want to use a template <> using directive here
    template <class T, class P = Particle*>
    struct State_Map : std::unordered_map<P, T, ParticlePtrState_Hash<P>, ParticlePtrState_Equal<P> > { };

    // typedefs for returning distributions (as ranges)

    template <class SubState, class Dummy = void>
    struct dist_range : distribution::single_estimate<SubState> {
        dist_range(const State &s, Weight weight) :
            distribution::single_estimate<SubState>(SubState(s), weight)
        { }
    };
    // specialisation for full ::State: simply return this State (= ::State) as is, do not construct
    // a temporary
    template <class Dummy>
    struct dist_range<::State<Model>, Dummy> : distribution::single_estimate<const ::State<Model>&> {
        dist_range(const State &s, Weight weight) :
            distribution::single_estimate<const ::State<Model>&>(s, weight)
        { }
    };

protected: // internal fields

    CVActionTable *actionTable;
    std::deque<State> historyList;
    HistoryTable history;

#ifdef USE_ACTR_HEURISTICS
    // a time vector for each element of the universe (act-r heuristic)
    int recencyTime [ELEMENTS_UNIVERSE];
#endif // USE_ACTR_HEURISTICS

#if (defined(HAS_CALLBACKS) || defined(USE_ACTION_OBSERVATION))
    Probability pyx;
#endif

    int id;
    static int pid;

public: // public fields

    State state;

    Weight weight;

public: // methods

    Particle() :
        actionTable(NULL),
        id(-1), // denotes uninitialised particle
        weight(ZeroWeight)
    { }

    Particle(const ::State<Model> &state, Weight weight) :
        actionTable(NULL),
        id(pid++),
        state(Particle::State(state)),
        weight(weight)
    {
#ifdef USE_ACTR_HEURISTICS
        resetDifferenceVectors();
#endif // USE_ACTR_HEURISTICS
        stepToState();
    }

    Weight getTotalWeight() const {
        return weight;
    }

    // returns the single <S, A, T, I, G> state (aka X-State, the ::State instance) represented by this particle
    // (or any SubState constructable from the state)
    template <class SubState>
    dist_range<SubState> getDistribution() const { return dist_range<SubState>(state, getTotalWeight()); };

    model::StatePtr getStatePtr() const {
        return &state.modelState->first;
    }

#if (defined(HAS_CALLBACKS) || defined(USE_ACTION_OBSERVATION))
    Probability get_pyx() const {
        return pyx;
    }
#else
    Probability get_pyx() const {
        const StateDataRec &gpt = state.modelState->second;
        if (gpt.time != Global::time) {
            std::cerr << "Particle has old observation (" << gpt.time << " / " <<  Global::time << ")" << std::endl;
            throw std::runtime_error("Cannot use old pyx.");
        }
        return gpt.pyx;
    }
#endif

    virtual int sampleOperator(Weight ssaliency, Weight *saliencies, int nappl) {
        return sampleOp(ssaliency, saliencies, nappl);
    }

    virtual bool stopAction(int opid, double startTime) {
        return (model::actions[opid]->durationModel)(startTime, NULL);
    }

    void setActionTable(CVActionTable *_actionTable) {
        actionTable = _actionTable;
    }

    std::deque<State>& getHistory() {
        return historyList;
    }

#ifdef USE_ACTR_HEURISTICS
    double getRecency (int opi) {
        return (model::actions[opi]->heuristic)(recencyTime, Global::timeSteps.size()-1, NULL);
    }

    double getRefractoriness (int opi, int agent);

    void setDifferences(StateTablePtr currentState, StateTablePtr previousState) {
        model::getDifferences(recencyTime, Global::timeSteps.size()-1, &previousState->first, &currentState->first);
    }

    void resetDifferenceVectors() {
        int t = static_cast<int>(Global::time);

        for (int i=0; i<ELEMENTS_UNIVERSE; i++) {
            recencyTime[i] = t;
        }
    }
#endif // USE_ACTR_HEURISTICS

    // call all observation callbacks
    void doCallbacksAndObservations();

    // Combined predict and update step.
    // the likelihood p(y|x) can be accessed via get_pxy()
    void step(States &states);

    void stepToState();

    void review();

protected:
    struct PInfo {
        double rec;
        double wrec;
        double ref;
        double wref;
        double revisit;
        double actionWeight;

        PInfo(): rec(0), wrec(0), ref(), wref(0), revisit(0), actionWeight(0) {
        }
    };

#if (defined(HAS_CALLBACKS) || defined(USE_ACTION_OBSERVATION))
    void set_pyx(Probability _pyx) {
        pyx = _pyx;
    }
#else
    void set_pyx(Probability _pyx) {
        StateDataRec &gpt = state.modelState->second;
        gpt.time = Global::time;
        gpt.pyx = _pyx;
    }
#endif

    double calcWeight(int goalIndex, int i, int agid, StateTablePtr curState, PInfo* pinfo=NULL);

    // returns whether this agent is still alive (alive = !dead, dead = dead-end modelState and action has stopped)
    virtual bool stepAgent(int agid, const State &pState, StateTablePtr curstate, StateTablePtr *newstate, int *aop, States &states);

    // Update the weight.
    void updateWeight(Probability p) {
        weight *= p;
    }

    // Update the weight.
    void normalizeWeight(Probability p) {
        weight /= p;
    }

    // Update step of particle filtering.
    // Computes p(y|x) by calling doCallbacksAndObservations() and update the weight.
    // That means, first pre_observe() of the model is called, then observations are made,
    // and finally post_observe() is called to compute p(y|x).
    // p(y|x) can be accessed via get_pyx()
    void update() {
#ifdef SIMULATOR
        return;
#endif
#if !(defined(HAS_CALLBACKS) || defined(USE_ACTION_OBSERVATION))
        const StateDataRec &gpt = state.modelState->second;
        // We can base the observation only on the modelState.
        // This allows us to cache the result.
        //
        // If action callbacks are enabled, instead, we have to make observation every time, as
        // action callbacks depend not only on the modelState, but also the
        // currently executed action.
        if (gpt.time == Global::time) {
            // pyx has already been computed
            updateWeight(gpt.pyx);
            return;
        }
#endif // HAS_CALLBACKS
        // call pre_observe_*(), then do observations, then call post_observe_*() to update the weight
        for (size_t i = 0; i < Model::n_agents; i++)
            model::pre_observe_action(i);
        const model::StateRec &s = state.modelState->first;
        model::pre_observe_state(&s);

        doCallbacksAndObservations();

        // let the model compute pyx (must be independent of action duration!)
        Probability pyx = 1;
        for (size_t i = 0; i < Model::n_agents; i++)
            pyx *= model::post_observe_action(i);
        pyx *= model::post_observe_state(&s);

        set_pyx(pyx);
        updateWeight(pyx);
    }
};

int Particle::pid = 0;

void Particle::doCallbacksAndObservations() {
    model::doCallbacksAndObservations(state.opid, state.modelState->first);
}

bool Particle::stepAgent(int agid, const State &pState, StateTablePtr curstate, StateTablePtr *newstate, int *aop, States &states) {
    using std::isnan;

    assert(curstate!=NULL);
    *aop = model::NoOpId;

    if (!stopAction(pState.opid[agid], pState.startTime[agid])) {
        return true; // action still continues
    }

    expandState(states, agid, curstate);

    const StateDataRec &d = curstate->second;
    Weight saliencies[NOPS];
    Sum<Weight> ssaliency;
    int sample;

    for (int i = 0; i < d.appl[agid]; i++) {
        // calculate action weight based on intermediate (current) state
        Weight sal(calcWeight(state.goalIndex(), i, agid, curstate));
        if (isnan(sal)) {
            std::cerr << "error stepAgent: weight of action (" << d.appi[agid][i].opid << ")" << std::endl;
            throw std::runtime_error("invalid probability in stepAgent");
        }
        ssaliency += sal;
        saliencies[i] = ssaliency;
    }
    if (prob(ssaliency) <= ZeroProbability) {
        // No action possible: Particle dies
        return false;
    }

    sample = sampleOperator(ssaliency, saliencies, d.appl[agid]);
    *aop = d.appi[agid][sample].opid;
    *newstate = d.appi[agid][sample].reached;
    return true;
}

#if DO_MULTI_AGENT
// Combined predict and update step.
void Particle::step(States &states) {
    StateTablePtr initialState = state.modelState, newstate = NULL, curstate = NULL;
    int aop = 0;
    bool agentsHaveChanged = false; // change of agent execution modelState
    bool stillRunning = false; // if agents are not in deadlock

    curstate = initialState;

    for (size_t agid = 0; agid < Model::n_agents; agid++) {
        bool agentAlive = stepAgent(agid, state, curstate, &newstate, &aop, states);

#ifdef SHOW_TRANSITION_INFO
        std::cerr << "STEP: Particle " << id
                  << " Agent " << model::agentNames[agid]
                  << " Time " << Global::time
                  << " AOP " << model::actions[aop]->names;
        if (state.isGoalState())
            std::cerr << " Goal ";
        std::cerr << std::endl;
#endif

#if ENDSTAT_REACH_STATES
        curstate->second.visited=true;
#endif

        if (!agentAlive) {
            // agent can't do anything
            if (state.opid[agid] != model::NoOpId) { // have been doing something before?
                agentsHaveChanged = true;
                state.opid[agid] = model::NoOpId;
                state.startTime[agid] = Global::time;
            }
        } else {
            // agent is still working
            stillRunning = true;
            if (aop != model::NoOpId) { // start new op
                agentsHaveChanged = true;
                state.opid[agid] = aop;
                state.startTime[agid] = Global::time;
                curstate = newstate;
            }
        }
    } // for(agid = ...)

    assert(curstate != NULL);
#ifdef USE_ACTR_HEURISTICS
    setDifferences(curstate, initialState);
#endif //USE_ACTR_HEURISTICS

    if (curstate != initialState) {
        state.modelState = curstate;
        stepToState();
    } else {
        if (agentsHaveChanged) {
            state.time = Global::time;
            historyList.push_back(state);
        }
        if (!stillRunning) {
            updateWeight(ZeroProbability);
            set_pyx(ZeroProbability);
        } else
            update();
    }
}

#else // DO_MULTI_AGENT

// Combined predict and update step.
void Particle::step(States &states) {
    int sample;
#ifdef USE_ACTR_HEURISTICS
    StateTablePtr before = state.modelState;
#endif // USE_ACTR_HEURISTICS

    state.time = Global::time;
    if (!stopAction(state.opid[0], state.startTime[0])) {
        // go on with current op
        update();
        return;
    }

    // time is up -> try to find something new
    expandState(states, 0, state.modelState);
#ifndef VALIDATOR
    state.startTime[0] = Global::time;
#endif //VALIDATOR

    const StateDataRec &d = state.modelState->second;
    Weight saliencies[NOPS];
    Sum<Weight> ssaliency;

#ifdef NORMALIZE_DISTANCE
    Weight max = -infinity;
    for (int i = 0; i < d.appl[0]; i++) {
        Weight sal = calcWeight(state.goalIndex(), i, 0, state.modelState);
        max = std::max(max, sal);
    }

    for (int i = 0; i < d.appl[0]; i++) {
        Weight sal = calcWeight(state.goalIndex(), i, 0, state.modelState);
        ssaliency += exp(sal-max);
        saliencies[i] = ssaliency;
    }

#else
    for (int i = 0; i < d.appl[0]; i++) {
        Weight sal = Weight(calcWeight(state.goalIndex(), i, 0, state.modelState));
        ssaliency += sal;
        saliencies[i] = ssaliency;
    }
#endif

    if (prob(ssaliency) <= ZeroProbability) {
        // No action possible: Particle dies
        state.opid[0] = model::NoOpId;
        updateWeight(ZeroProbability);
        return;
    }

    sample = sampleOperator(ssaliency, saliencies, d.appl[0]);
    state.opid[0] = d.appi[0][sample].opid;
    state.modelState = d.appi[0][sample].reached;

#if ENDSTAT_REACH_STATES
    state.modelState->second.visited=true;
#endif

#ifdef USE_ACTR_HEURISTICS
    setDifferences(before, state.modelState);
#endif // USE_ACTR_HEURISTICS
    stepToState();
}
#endif // DO_MULTI_AGENT


void Particle::stepToState() {
    auto insResult = history.insert(state.modelState);
    if (!insResult.second && parameters.revisitingFactor == -infinity) {
        std::cerr << "Step: Particle " << id << " have been here before" << std::endl;
        throw std::logic_error("{stepToState} ** IMPOSSIBLE **");
    }
    state.time = Global::time;
    historyList.push_back(state);

    update();
}

void Particle::review() {
    std::clog << "Particle ["<< id << "] history:\n";
    for (const State &d: historyList) {

        std::clog << d.time;
        for (size_t agid = 0; agid < Model::n_agents; agid++) {
            std::clog << ' ' << model::agentNames[agid]
                      << (d.startTime[agid] < d.time ? ": ..." : ": * ")
                      << model::actions[d.opid[agid]]->name;
        }
        std:: clog << std::endl;
    }
    std::clog << Global::time << ": ";

    const State &s = historyList.back();
    if (s.isGoalState())
        std::clog << "(SUCCESS)";
    else
        std::clog << "(RUNNING)";
    std::clog << std::endl;
}

#ifdef USE_ACTR_HEURISTICS
double Particle::getRefractoriness (int opi, int agent) {
    double ref = 0;

    model::StateRec currentAction;
    (model::actions[opi]->heuristic)(recencyTime, 0, &currentAction);

    int i=0;
    for (auto it = historyList.rbegin(); it != historyList.rend() && i<REFRACTORINESS_ACTIONS; it++) {
        const State &histTemp = *it;
        int oph = histTemp.opid[agent];
        if (oph < 0)
            break;
        model::StateRec a;
        (model::actions[oph]->heuristic)(recencyTime, 0,&a);
        // time check
        double duration = Global::time - histTemp.time;

        if (duration < 0)
            duration = 0;
        else if (duration > REFRACTORINESS_TIME) {
            break;
        } else {
            // duration = exp(((-1)*duration)/(REFRACTORINESS_TIME/10));
            duration = exp((-1)*duration);
        }
        ref += (1-getRefract(&a,&currentAction))*duration;
        i++;
    }
    return i==0 ? 1 : ref/i;
}
#endif // USE_ACTR_HEURISTICS

double Particle::calcWeight(int goalIndex, int i, int agid, StateTablePtr curState, PInfo* pinfo) {
    const StateDataRec &d = curState->second;
    model::StatePtr s = &curState->first;
    (void)s;
    ApplInfo ap = d.appi[agid][i];

    int curOpid = state.opid[agid];

    // first check if executing the action would be an illegal repition
    if (!model::canFollowAfter(ap.opid, curOpid))
        return 0;

#if defined(SUPPORTS_PYA) && defined(USE_PYA) && defined(HAS_CALLBACKS)
    // if supported, check if current observation are compatible with the action
    // if ZeroProbability is returned, set weight to 0, otherwise leave it as-is
    // first run callback, so the observation model has information about the current action
    model::actions[ap.opid]->callback(s);
    Probability pya = getActionProbability(s);
    if (pya == ZeroProbability)
        return 0;
#endif

    PInfo prob;
    double sal;

#ifdef USE_ACTR_HEURISTICS
    prob.ref = getRefractoriness(ap.opid, agid);
    prob.rec = getRecency(ap.opid);
    prob.wrec = parameters.weightRecency == 0 ? 0 : parameters.weightRecency * log(prob.rec+1);
    prob.wref = parameters.weightRefractoriness == 0 ? 0 : parameters.weightRefractoriness * log(prob.ref+1);
#else
    prob.wrec = 0;
    prob.wref = 0;
#endif // USE_ACTR_HEURISTICS
    sal = ap.weight[goalIndex] + prob.wrec + prob.wref;

// add learned action weight if available
    if (actionTable) {
        prob.actionWeight = actionTable->getP(ap.opid);
        sal += (0 == prob.actionWeight) ? -infinity : log(prob.actionWeight);
    }

    sal = pow(sal, 1/parameters.nroot);
    prob.revisit = 0.0;
    if (history.count(ap.reached) > 0) {
        prob.revisit = parameters.revisitingFactor;
        sal += parameters.revisitingFactor;
    }
    if (pinfo)
        *pinfo = prob;
    // TODO: e^-inf = 0;
#ifdef NORMALIZE_DISTANCE
    sal = sal+parameters.weightBias;
#else
    sal = exp(sal+parameters.weightBias);
#endif
    return sal;
}

inline bool compareParticlesDesc(Particle const * const a, Particle const * const b) {
    // particles with biggest weight go first
    return prob(a->weight) > prob(b->weight);
}

inline bool compareParticlesEqual(Particle const * const a, Particle const * const b) {
    return prob(a->weight) == prob(b->weight);
}

inline bool compareParticlesAsc(Particle const * const a, Particle const * const b) {
    // particles with smallest weight go first
    return prob(a->weight) < prob(b->weight);
}

} // namespace particle
