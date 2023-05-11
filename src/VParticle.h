#pragma once

#include <limits>
#include <algorithm>

#include "particle.h"
#include "Stepinfo.h"

namespace validator {

template <class Model>
class CVParticle : public particle::Particle {
private:
    int currentAgent;
public:
    typedef particle::Particle::State State;

public:

    StateTablePtr currentState;

    StepInfo sf[Model::n_agents];
    int actionId[Model::n_agents];

    CVParticle() :
        particle::Particle(),
        currentAgent(0)
    { }

    CVParticle(::State<Model> state, Weight weight) :
        particle::Particle(state, weight),
        currentAgent(0)
    { }

    void setaction(int agid, std::string name, int actionIndex, bool isNew) {
        sf[agid].reset();
        sf[agid].name = name;
        sf[agid].isNew = isNew;
        actionId[agid] = actionIndex;
    }

    void header(std::ostream &o) {
        o << "Time";
        for (size_t i = 0; i < Model::n_agents; i++) {
            o << ";"<< i << "_" << model::agentNames[i] <<";";
            sf[i].header(o,i);
        }
        o << ";paTeam1t;logpaTeam1t;pyx;logpaTeam1ty\n";
    }

    void print(int step, std::ostream &o) {
        LogProbability paTeam1t(1.0);

        o << step;

        for (size_t i = 0; i < Model::n_agents; i++) {
            paTeam1t *= sf[i].paAgent1t;

            if (!sf[i].isNew) {
                paTeam1t *= sf[i].pStop;
            }
            o << ";"<< i << "_" << model::agentNames[i] <<";" << sf[i];
        }

        StepInfo::paTeam1ty = paTeam1t * StepInfo::pyx1t;
        o << ";" << paTeam1t << ";" << p_logValue(paTeam1t) << ";" << StepInfo::pyx << ";" << p_logValue(StepInfo::paTeam1ty) << '\n';
        StepInfo::fll = StepInfo::paTeam1ty;

    }

    bool validateStep(States &states, bool useObs) {
        // returns only the probability of the observation
        step(states);
        if (!useObs)
            StepInfo::pyx = LogProbability(1);
        else
            StepInfo::pyx = LogProbability(get_pyx());
        StepInfo::pyx1t *= StepInfo::pyx;

        LogProbability p(1.0);

        // determine the prob of the whole state
        for (size_t i = 0; i < Model::n_agents; i++) {
            if (0 <= actionId[i]) { // only determine probability for 'real' actions
                if (sf[i].isNew) { // create a new base for further actions
                    sf[i].paAgent1t *= sf[i].patn * sf[i].pStop;
                }
                p *= sf[i].paAgent1t;
            }
        }


        // this step is valid if the transition prob is greater than 0
        return (double)p != 0;
    }

    virtual bool stepAgent(int agid, const State &pState, StateTablePtr curstate, StateTablePtr *newstate, int *aop, States &states) {
        currentAgent = agid;
        currentState = curstate;
        return Particle::stepAgent(agid, pState, curstate, newstate, aop, states);
    }


    virtual int sampleOperator(Weight ssaliency, Weight * saliencies, int nappl) {
        (void) saliencies;

        // state space was explored before
        // check if nappl is the same as d.nappl
        int plannedAction=-1;
        // the maximal probability of an action (used to infer if the selected action is the one with the highest prob.)
        LogProbability maxpatn = LogProbability(0);
        double maxpatnCount=0;

        PInfo info;

        StepInfo sum;

#ifndef DO_MULTI_AGENT
        currentState = state.modelState;
#endif

#ifdef NORMALIZE_DISTANCE
        Weight max = -infinity;
        for (int i = 0; i < nappl; i++) {
            max = std::max(max, calcWeight(i, currentAgent,currentState, &info));
        }
#endif

        StateDataRec &d = state.modelState->second;

        for (int i = 0; i < nappl; i++) {

            ApplInfo &appi = d.appi[currentAgent][i];
#ifdef NORMALIZE_DISTANCE
            Weight pat = exp(calcWeight(state.goalIndex(), i, currentAgent,currentState, &info) - max);
#else
            Weight pat = calcWeight(state.goalIndex(), i, currentAgent,currentState, &info);
#endif

            if (appi.opid == actionId[currentAgent]) {
                // determine prob of action from plan

                sf[currentAgent].bfactor = nappl;
                sf[currentAgent].saliency = LogProbability(appi.saliency);
                sf[currentAgent].specificity = LogProbability(appi.specificity);
                sf[currentAgent].wSaliency = appi.wSaliency;
                sf[currentAgent].wSpecificity = appi.wSpecificity;
                sf[currentAgent].wGoalDistance = appi.wGoalDistance[state.goalIndex()];
                if (0 == parameters.weightFactor) {
                    sf[currentAgent].goalDistance = -infinity;
                } else {
                    sf[currentAgent].goalDistance = sf[currentAgent].wGoalDistance / parameters.weightFactor;
                }
                sf[currentAgent].pat = LogProbability(prob(pat));
                sf[currentAgent].patn = sf[currentAgent].pat / LogProbability(prob(ssaliency));
                sf[currentAgent].patsum = LogProbability(prob(ssaliency));
                sf[currentAgent].recency = LogProbability(info.rec);
                sf[currentAgent].wRecency = info.wrec;
                sf[currentAgent].refractoriness = LogProbability(info.ref);
                sf[currentAgent].wRefractoriness = info.wref;
                sf[currentAgent].revisit = info.revisit;

                plannedAction = i;

                sum += sf[currentAgent];
            } else {
                sum.saliency += LogProbability(appi.saliency);
                sum.specificity += LogProbability(appi.specificity);
                sum.pat += LogProbability(prob(pat));
                sum.recency += LogProbability(info.rec);
                sum.refractoriness += LogProbability(info.ref);
                sum.revisit += exp(info.revisit);
                sum.wGoalDistance += exp(appi.wGoalDistance[state.goalIndex()]);
            }
            pat /= ssaliency;
            if (pat != ZeroWeight) {
                sf[currentAgent].entropy += p_value(prob(pat)) * p_logValue(prob(pat));
            }

            if (fabs(p_logValue(prob(pat)) - p_logValue(maxpatn)) < std::numeric_limits<double>::epsilon()) { // equal
                maxpatnCount+=1.0;
            } else if (p_logValue(prob(pat)) > p_logValue(maxpatn)) { // new prob is greater
                maxpatn = LogProbability(prob(pat));
                maxpatnCount = 1.0;
            }
        }

// Now, we have the probs of all heuristics for all possible actions
// sum up and normalize values for each heuristic, resp.

        sf[currentAgent].normalizeP(sum);
        if (fabs(p_logValue(maxpatn - sf[currentAgent].patn)) < std::numeric_limits<double>::epsilon()) {
            sf[currentAgent].hitcount = 1.0/maxpatnCount;
        } else {// not equal
            sf[currentAgent].hitcount = 0.0;
        }

        // action was not found in the list of applicable actions
        // validator found problem
        if (-1 == plannedAction) {
            std::ostringstream oss;
            // TODO: change slot to the real slot
            oss << "Failed to meet preconditions of action: " << model::actions[actionId[currentAgent]]->name << " in slot " << currentAgent << " in state\n" << getStatePtr() << std::endl;
            throw std::runtime_error(oss.str());
        }

        return plannedAction;
    }


    /*
    two different options:
    1.) the action is new: we have to get pStop(curTime) of action executed before the new one
    2.) the is not new:
    */
    virtual bool stopAction(int opid, double startTime) {
        LogProbability &pStop = sf[currentAgent].pStop;
        bool isNew = sf[currentAgent].isNew;
        pStop = LogProbability((model::actions[opid]->pdfModel)(startTime, Global::time));
        if (!isNew)
            pStop = LogProbability(1) - pStop;
        return isNew;
    }

    void printHistory(const char* fn) {
        std::ofstream os(fn, std::ofstream::out);

        os << "OpId " << "Step " << "Goaldistance";
        for (int i=0; i<ELEMENTS_UNIVERSE; i++) {
            os << " S" << i;
        }
        os << '\n';


        int i = 0;
        for (const Particle::State &p: historyList) {
            const model::StateRec &state = p.modelState->first;
#if DO_MULTI_AGENT
            os << " error (not available): we should also print the intermediate state here\n";
#else
            os << p.opid[0] << ' ' << p.time << ' ';
#endif // MULTI_AGENT
            model::writeState(os, &state, i);
        }
    }
};


} // namespace validator
