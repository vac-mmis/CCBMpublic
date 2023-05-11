#include <cassert>
#include <iostream>
#include <fstream>
#include <sstream>
#include <unistd.h>
#include <memory>
#include <functional>
#include <unordered_map>

#define USE_EXACT_FILTER 1

#include "util/allocator.h"
#include "util/functional.h"
#include "util/iterators.h"
#include "util/distribution.h"
#include "util/summation.h"
#include "util/parameter.hpp"

#include "estimate/estimators.h"

#include "config.h"
#include "global.h"
#include "obsmodel.h"
#include "state.h"
#include "stateFunctions.h"
#include "particle_common.h"

#if HAS_CALLBACKS
#ifdef HMM_STATE_ONLY
#error "Can't use action callbacks with exact filter without actions"
#endif
#endif

#if NUM_GOALS>1
#error "HMM only with one goal"
#endif

using namespace std;

/* Restrictions:
   - single agent
   - only exponential durations
   - no estimate of the starting time

if HMM_STATE_ONLY is defined:
   - no proper action durations (the duration of the first action discovering that state during transition matrix generation)
   - no action callbacks
 */

/* NOTE: If implementing Viterbi-MAP-Estimate (or smoothing estimate):

   remember that p(x'|x) has to be calculated by marginalizing over
   *all* actions a leading from state x to state x'. That is:

   p(x'|x) = \sum_{a\in A | x'=a(x)} p(x',a|x)
           = \sum_{a\in A | x'=a(x)} p(x'|a,x) p(a|x')
	   = \sum_{a\in A | x'=a(x)} 1 p(a|x')
	   = \sum_{a\in A | x'=a(x)} p(a|x')

   we might wish to compute this marginal directly in expandHMMState

 */

namespace hmm {

template <class Model>
struct HMMKey {
    typedef typename Model::opid_t opid_t;

    StateTablePtr modelState; // pointer to underlying modelState
#ifndef HMM_STATE_ONLY
    opid_t opid;
#endif
    Probability        pself; // self transition probability

    HMMKey() :
        modelState(NULL),
        pself(0)
    {}

    HMMKey(StateTablePtr _state, opid_t _opid, Probability _pself) :
        modelState(_state),
#ifndef HMM_STATE_ONLY
        opid(_opid),
#endif
        pself(_pself)
    {}

    bool operator ==(const HMMKey<Model> &other) const {
        if (modelState != other.modelState)
            return false;
#ifndef HMM_STATE_ONLY
        if (opid != other.opid)
            return false;
#endif
        return true;
    }
};

template<class Model>
class HMMKey_Hash {
public:
    size_t operator()(HMMKey<Model> const &key) const {
        boost::hash<StateTablePtr> hasher_state;
        boost::hash<typename Model::opid_t> hasher_opid;

        return hasher_state(key.modelState) * 31 + hasher_opid(key.opid);
    }
};


struct HMMTrans;
struct HMMData {
    Probability p0; // updated state probability p(x_t | y_1:t)
    Probability p1; // predicted state probability p(x_t | y_1:t-1)
    int ntrans; // number of transitions to other states
    HMMTrans *transitions;
    HMMData() : p0(0), p1(0), ntrans(-1), transitions(NULL) {}
};


// class that wraps a HMM state into something that looks like a Particle
// this is required for estimates
template <class Model_>
class HMMParticleProxy {

    const HMMData *data;

public:

    typedef Model_ Model;

    typedef const HMMKey<Model> *State;

    // typedefs for returning distributions (as ranges)

    template <class SubState>
    using dist_range = distribution::single_estimate<SubState>;

public:

    State state;

public:

    HMMParticleProxy(State state, const HMMData *data) :
        data(data),
        state(state)
    { }

    template <class SubState>
    dist_range<SubState> getDistribution() const {
        // Note: starting time is always set to 0, because this is not explicitly saved in the state
        // (This HMM relies on exponentially distributed starting times and works efficiently only when it must not estimate the starting time)
        typename ::State<Model>::startTime_t startTime;
        for (size_t agid = 0; agid < Model::n_agents; agid++)
            startTime[agid] = 0;

#ifndef HMM_STATE_ONLY
        ::State<Model> s(0, 0, state->modelState, state->opid, startTime);
#else
        // state does not contain opid
        typename ::State<Model>::opid_t opid;
        for (size_t i = 0; i < Model::n_agents; i++)
            opid[i] = model::InitOpId;

        ::State<Model> s(0, 0, state->modelState, opid, startTime0);
#endif
        return dist_range<SubState>(SubState(s), getTotalWeight());
    }

    Weight getTotalWeight() const {
        return Weight(data->p0);
    }

    size_t getInitialIndex() const {
        // change this if HMM supports multiple initial states
        return 0;
    }

    size_t getGoalIndex() const {
        // change this if HMM supports multiple goals
        return 0;
    }

    void doCallbacksAndObservations() {
        model::doCallbacksAndObservations(state->opid, state->modelState->first);
    }
}; // class HMMParticleProxy


typedef std::unordered_map<HMMKey<model::Model>, HMMData,
    HMMKey_Hash<model::Model>, std::equal_to<HMMKey<model::Model>>,
    pool_small_allocator<std::pair<const HMMKey<model::Model>, HMMData>> > HMMStateTable;
typedef HMMStateTable::value_type *HMMStateTablePtr;

struct HMMTrans {
    int opid;
    Probability p;
    HMMStateTablePtr dest;
    HMMTrans() :
        opid(model::InitOpId),
        p(ZeroProbability),
        dest(NULL) {}
};

template <class Model>
HMMStateTablePtr expandHMMState(HMMStateTable &hmmStates, HMMKey<Model> key, States &states) {
    auto insResult = hmmStates.emplace(key, HMMData());
    HMMStateTablePtr hmms = &(*(insResult.first));

    if (hmms->second.ntrans >= 0)
        return hmms;

    Probability pSelf = key.pself;
    if (pSelf < ZeroProbability) {
        std::ostringstream msg;
#ifndef HMM_STATE_ONLY
        msg << "Invalid HMM duration probability in action " << key.opid[0];
#else
        msg << "Invalid HMM duration probability in one state";
#endif
        throw std::runtime_error(msg.str());
    }


    // clog << "Expanding HMM State\n";
    StateDataRec &pd = key.modelState->second;

    int ntrans = 0;       // make place for transitions to outside
    for (int i = 0; i < pd.appl[0]; i++) {
        if (pd.appi[0][i].weight[0] > 0)
            ntrans++;
    }

    if (ntrans==0) pSelf = 1; // make sure we stay here in case we can't go any other place
    Probability pOther = Probability(1) - pSelf;      // sum of probabilities of transitions to outside
    if (pSelf > ZeroProbability) ntrans++;   // make place for self transition if exists

    HMMTrans *transitions = new HMMTrans[ntrans];
    if (transitions==NULL)
        throw runtime_error("No memory for transitions");
    hmms->second.ntrans = ntrans;
    hmms->second.transitions = transitions;

    // copy outside transitions
    size_t trans_index = 0;
    for (int i = 0; i < pd.appl[0]; i++) {
        if (pd.appi[0][i].weight[0] <= 0)
            // zero probability of this transitions (probably due to infinite goal distance)
            // do not add these transitions and do not recursively add these HMMStates
            continue;

        // TODO: change this to compute marginal transition probabilities
        transitions[trans_index].opid = pd.appi[0][i].opid;
        transitions[trans_index].p = pOther * Probability(pd.appi[0][i].weight[0]); // saliencies have been scaled to sum to one before the first call to expandHMMState
        typename Model::opid_t opid = { pd.appi[0][i].opid };

        // recursively add HMM state
        transitions[trans_index].dest = expandHMMState<Model>(hmmStates, HMMKey<Model>(pd.appi[0][i].reached, opid,model::actions[pd.appi[0][i].opid]->durationProb), states);
        trans_index++;
    }

    // handle self-transition
    if (pSelf > ZeroProbability) {
        transitions[ntrans-1].opid = -1;
        // due to states with infinite goal distances, some state may have no outgoing transitions
        // in thise case, the actual self-transition probability is larger then pSelf from only the duration model
        // (in fact, it's exactly 1, because ptotal should be 0)
        transitions[ntrans-1].p = pSelf;
        transitions[ntrans-1].dest = hmms;
    }

    // sanity check
    Sum<Probability> ptotal;
    for (int i=0; i<ntrans; i++)
        ptotal += transitions[i].p;

#ifdef USE_LOG_PROBS
    if (fabs(static_cast<double>(ptotal.sum()) - static_cast<double>(Probability(1))) > 1e-14)
#else
    if (fabs(ptotal.sum() - Probability(1)) > 1e-14)
#endif // USE_LOG_PROBS
    {
        cerr << "Transitions don't sum to one: " << (ptotal.sum() - Probability(1)) << endl;
        throw runtime_error("Transitions don't sum to one");
    }

    return hmms;
}

/** print HMM transition matrix

output format:
    States <number of states>
    Agents <number of agents>
    [for every state:
        <opid> <state bits>,
        seperated by \n]
    Init <initial state index, 0-based>
    [for every state s:
        <transition probability from s to all other states, seperated by space>,
        seperated by \n]
*/
template <class Model>
void printTransitionMatrix(std::ostream *o_matrix, HMMStateTable &hmmStates, HMMStateTablePtr hinit) {
    // print total states and nagents
    *o_matrix << "States " << hmmStates.size() << '\n'
              << "Agents " << Model::n_agents << '\n';

    // if we print the complete transition matrix, we need to assign each state a unique index in that matrix
    // The HMMStateTablePtrs are unique, therefore we can simply hash by the pointer address
    std::unordered_map<HMMStateTablePtr, size_t> state_index_map;

    // first, we iterate once over all HMMStates to assign each a unique id
    // then, we iterate a second time to print the actual probability matrix
    size_t i = 0;
    for (auto &e: hmmStates) {
        state_index_map.emplace(&e, i);
        i++;

        const HMMKey<Model> &key = e.first;
#ifndef HMM_STATE_ONLY
        // print the action name first
        typename Model::opid_t const &opid = key.opid;
        for (size_t ag = 0; ag < Model::n_agents; ag++) {
            *o_matrix << model::actionNames[opid[ag]] << ' ';
        }
#endif

        // for every state, print a description of the state (to extract state bits)
        const model::StateRec &s = key.modelState->first;
        *o_matrix << &s << '\n';
    }

    // print initial state index
    // TODO multiple initial states
    *o_matrix << "Init " << state_index_map[hinit] << '\n';

    for (auto &e: hmmStates) {
        HMMData &sd = e.second;

        // generate transition probabilities originating from state s

        std::vector<Probability> probs(hmmStates.size(), 0); // fill with zeros

        for (int i = 0; i < sd.ntrans; i++) {
            HMMStateTablePtr s_reached = sd.transitions[i].dest;
            size_t idx = state_index_map[s_reached];
            probs[idx] = sd.transitions[i].p;
        }

        // print
        *o_matrix << probs[0];
        for (size_t i = 1; i < probs.size(); i++)
            *o_matrix << ' ' << probs[i];
        *o_matrix << '\n';
    }

    o_matrix->flush();
}

int main(int argc, char *argv[]) {
    typedef model::Model Model;

    bool o_uniform_initial = false;

    add_filter_params_struct s;
    parameters.weightSaliency = 1.0;
    std::shared_ptr<std::ostream> o_matrix;

    options::option opts;

    try {

        options::add_filter_params(opts, s);

        opts.add("uniform", "", "use uniform initial probability",
        [&](std::string, std::vector<std::string>, size_t) -> size_t {
            o_uniform_initial = true;
            return 0;
        });

        opts.add("print-matrix", "save the transition matrix to <file>")
            .parameter("file", o_matrix);

        options::add_help_param(opts);

        if (argc - opts.parse(argc, argv) != 0) {
            std::cerr << "Too many arguments!\n";
            std::cerr << "Use --help to show all available options\n";
            return 1;
        }

    } catch(std::exception &e) {
        std::cerr << e.what() << '\n';
        std::cerr << "Use --help to show all available options\n";
        return 1;
    }

    States states;

    if (s.o_statefile.empty()) {
        std::cerr << "No statefile set (-l), this is required for the HMM.\n";
        return 1;
    }
    try {
        ifstream sfile(s.o_statefile);
        if (sfile.fail()) {
            cerr << "Cannot load state file from " << s.o_statefile << '\n';
            exit(1);
        }
        int nr = loadStates(states, 0, sfile); (void) nr;
        sfile.close();
#if VERBOSITY > 0
        clog << "Read " << nr << " state records from " << s.o_statefile << '\n';
#endif
        int ngoal = 0;
        int ndead = 0;
        int nnormal = 0;

        HMMStateTable hmmStates;

        for (StateWeightMap::iterator it = states.weightMap[0].begin(); it != states.weightMap[0].end(); ++it) {
            model::StateRec s = it->first;
            StateTablePtr pt = &(*(states.states.emplace(s, StateDataRec()).first));
            StateDataRec &pd = pt->second;

            expandState(states, 0, pt);
            if (pd.appl[0] == 0)
                ndead++;
            else
                ++nnormal;

            if (isGoalState(0, pt)) {
                ++ngoal;
            }

            if (pd.appl[0]>0) {
                Sum<Probability> ptotal = ZeroProbability;
                for (int i = 0; i < pd.appl[0]; i++) {
                    pd.appi[0][i].weight[0] = exp(pd.appi[0][i].weight[0]);
                    ptotal += pd.appi[0][i].weight[0];
                }
                if (ptotal.sum() != ZeroProbability)
                    // scale probabilities to sum to 1, required by HMM
                    // (if sum is 0, this state probably has goaldistance infinity and will never be reached anyway)
                    for (int i = 0; i < pd.appl[0]; i++)
                        pd.appi[0][i].weight[0] /= p_value(ptotal.sum());
            }
        }
        /*
        clog << "Normal: " << nnormal << '\n'
         << "Dead:   " << ndead << '\n'
         << "Goal:   " << ngoal << '\n'
         << "Total:  " << ngoal + ndead + nnormal << '\n'; */
        // find initial state & set its probability to 1
        model::StateRec xrec;
        // TODO multiple initial states
        model::sampleInitial(0, &xrec);
        StateTablePtr sinit = &(*(states.states.emplace(xrec, StateDataRec()).first));

        ::State<Model>::opid_t opid;
        for (size_t i = 0; i < Model::n_agents; i++)
            opid[i] = model::InitOpId;

        HMMStateTablePtr hinit = expandHMMState<Model>(hmmStates, HMMKey<Model>(sinit, opid, model::actions[-1]->durationProb), states);

        // print the transition matrix
        if (o_matrix)
            printTransitionMatrix<Model>(o_matrix.get(), hmmStates, hinit);

        hinit->second.p1 = 1;

        clog << argv[0] << ": Created " << hmmStates.size() << " HMM States\n";

        if (o_uniform_initial) {
            Probability pinit = 1.0 / hmmStates.size();

            for (auto &e: hmmStates)
                e.second.p1 = pinit;
        }

        std::vector<EstimateTarget<HMMParticleProxy<Model> > > estimators;
        std::shared_ptr<std::ostream> stream_p = std::shared_ptr<std::ostream>(&cout, util::empty_function<std::ostream*>());
#ifndef USE_ACTION_ESTIMATION
#ifdef HAS_ESTIMATOR
        estimators.emplace_back(std::make_shared<model::Estimate<HMMParticleProxy<Model> > >(), stream_p);
#endif
#else // USE_ACTION_ESTIMATION
        estimators.emplace_back(std::make_shared<ActionEstimator<HMMParticleProxy<Model> , NOPS> >(USE_ACTION_ESTIMATION), stream_p);
#endif // USE_ACTION_ESTIMATION
#ifdef USE_GOAL_ESTIMATION
        estimators.emplace_back(std::make_shared<GoalEstimator<HMMParticleProxy<Model> , NUM_GOALS> >(true), stream_p);
#endif

        Global::time=0;
        Global::timeSteps.push_back(0);
        Global::deltaT=1;
        Global::loglik=0;

        for (auto &est: estimators)
            est.printHeader();

        while (model::fetchObservation()) {
            // clog << "Time " << Global::time << '\n';

            Sum<Probability> ptotal = ZeroProbability;
            // correct
            for (auto &entry: hmmStates) {
                HMMData &dt = entry.second;
                StateTablePtr pt = entry.first.modelState;

                Probability pyx, tmp_pyx;

#ifndef HAS_CALLBACKS
                StateDataRec &gpt = pt->second;
                // check if allready cached
                if (gpt.time == Global::time) {
                    tmp_pyx = gpt.pyx;
                } else { // not yet cached
                    for (size_t agid = 0; agid < Model::n_agents; agid++)
                        model::pre_observe_action(agid);
                    model::pre_observe_state(pt->first);
                    gpt.time = Global::time;
                    model::do_state_callback(pt->first);
                    tmp_pyx = 1;
                    for (size_t agid = 0; agid < Model::n_agents; agid++)
                        tmp_pyx *= model::post_observe_action(agid);
                    tmp_pyx *= model::post_observe_state(pt->first);
                    gpt.pyx = tmp_pyx;
                }
#else // HAS_CALLBACKS
                for (size_t agid = 0; agid < Model::n_agents; agid++)
                    model::pre_observe_action(agid);
                model::pre_observe_state(pt->first);
                model::do_state_callback(pt->first);
                model::do_action_callback(entry.first.opid, pt->first);
                tmp_pyx = 1;
                for (size_t agid = 0; agid < Model::n_agents; agid++)
                    tmp_pyx *= model::post_observe_action(agid);
                tmp_pyx *= model::post_observe_state(pt->first);
#endif // HAS_CALLBACKS

                pyx = prob(tmp_pyx);
                dt.p0 = dt.p1 * pyx;
                ptotal += dt.p0;
                dt.p1 = ZeroProbability;
            }
            Global::loglik += p_logValue(ptotal);
            // clog << "Time " << Global::time << " Loglik " << Global::loglik << '\n';
            // cout << Global::time << ' ' << Global::loglik << '\n';

            // normalise weights
            for (auto &e: hmmStates) {
                e.second.p0 /= ptotal.sum();
            }

            // run estimators
            {
                // XXX this is a somewhat dirty hack to convert the HMM states into a list of HMMParticleProxy*, as required by runEstimators
                // Maybe the HMM states itself should be more Particle-like
                std::vector<HMMParticleProxy<Model>> particles;
                std::vector<HMMParticleProxy<Model> *> particles_p;
                particles.reserve(hmmStates.size());
                particles_p.reserve(hmmStates.size());

                for (auto &e: hmmStates) {
                    HMMData &dt = e.second;
                    particles.push_back(HMMParticleProxy<Model>(&e.first, &dt));
                    particles_p.push_back(&particles.back());
                }

                runEstimators(Global::time, estimators.begin(), estimators.end(), particles_p.begin(), particles_p.end());
            }

            // predict
            Global::time += Global::deltaT;
            Global::timeSteps.push_back(Global::time);

            for (auto &e: hmmStates) {
                HMMData &dt = e.second;

                for (int i = 0; i < dt.ntrans; i++) {
                    HMMStateTablePtr hmm1 = dt.transitions[i].dest;
                    hmm1->second.p1 += dt.transitions[i].p * dt.p0;
                }
            }
        } // while

        clog << argv[0] << ": Final log-likelihood: " << Global::loglik << '\n'
             << argv[0] << ": Finishing time:       " << Global::time - Global::deltaT << '\n';

    } // try

    catch (const exception& e) {
        cerr << "Exception: " << e.what() << endl;
        return 1;
    }

    return 0;
}

} // namespace hmm

int main(int argc, char** argv) {
#if TRAP_FE
    feenableexcept(FE_INVALID);
#endif
    std::ios::sync_with_stdio(false);

    int result = hmm::main(argc, argv);
    pool_allocator_base::free_all();
    return result;
}
