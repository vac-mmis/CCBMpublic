#include <cassert>
#include <cmath>

#include "../util/summation.h"

#include "../obsmodel.h"
#include "../global.h"
#include "../stateFunctions.h"

#include "particle_startTimes.h"

namespace marginal {
namespace particle {
namespace startTimes {

namespace detail {



//      free functions
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// compute the max error the merged segment does on the log-values of end-points of the "true" segments
// We also explored the use of KL-divergence, but is has too many numerical instabilities.
// The KL-divergence tends to merge more often in regions of very low probability, while the log-error
// tends to merge more often in regions of high probability.
// To mimic the behaviour of KL-divergence, you can pass exp > 1 to decrease the error in regions of low probability.
//
// template parameters:
// N: passed to LogSegment<N>
// It: iterator of LogSegment<N>
template <bool N, class It>
static double merged_logerror(It true_start, It true_end, const linmodel::LogSegment<N> &merged, double exp) {
    using std::abs;
    double max_error = 0;

    for (It it = true_start; it != true_end; ++it) {
        double log_start = it->predict(it->start).log();
        double log_end = it->predict(it->end).log();
        // compute the error by abs(true-merged)/abs(true)^merge_exp
        // by raising true to the power merge_exp, we allow for even more errors in regions of low probability
        // this shall partly mimic the behaviour of KL-divergence, which tends to neglect errors in those regions
        double e_start = abs(log_start - merged.predict(it->start).log()) / std::pow(abs(log_start), exp);
        double e_end = abs(log_end - merged.predict(it->end).log()) / std::pow(abs(log_end), exp);
        double m = fmax(e_start, e_end);
        if (m > max_error)
            max_error = m;
    }

    return max_error;
}


//      nested class TimeModel::const_iterator
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

// default-constructor
template <class Model>
TimeModel<Model>::const_iterator::const_iterator() :
    segments(nullptr),
    it_s(),
    value(startTime_t(), ZeroWeight)
{
    value.first.fill(0);
}

// construct as past-end iterator
template <class Model>
TimeModel<Model>::const_iterator::const_iterator(const segments_t *segments) :
    segments(segments),
    it_s(segments->end()),
    value(startTime_t(), ZeroWeight)
{
    value.first.fill(0);
}

// construct using dereferencable segment iterator
template <class Model>
TimeModel<Model>::const_iterator::const_iterator(const segments_t *segments, segments_t::const_iterator it_s) :
    segments(segments),
    it_s(it_s),
    value(startTime_t(), static_cast<Probability>(it_s->predict(it_s->start)))
{
    assert(it_s != segments->end() && "Do not call this constructor with an explicit end-iterator");
    // otherwise it_s->start has no valid value
    // TODO multi-agent
    value.first.fill(it_s->start);
}

template <class Model>
typename TimeModel<Model>::const_iterator& TimeModel<Model>::const_iterator::operator++() {
    // TODO multi-agent
    value.first[0] += 1;
    if (value.first[0] > it_s->end) {
        ++it_s;
        if (it_s == segments->end()) {
            // past-end
            value.first.fill(0);
            return *this;
        }
        value.first[0] = it_s->start;
    }
    value.second = static_cast<Probability>(it_s->predict(value.first[0]));
    return *this;
}

template <class Model>
typename TimeModel<Model>::const_iterator& TimeModel<Model>::const_iterator::operator--() {
    value.first[0] -= 1;
    if (value.first[0] < it_s->start) {
        if (it_s == segments->begin()) {
            // before-begin
            --it_s;
            value.first.fill(0);
            return *this;
        }
        --it_s;
        value.first[0] = it_s->end;
    }
    value.second = static_cast<Probability>(it_s->predict(value.first[0]));
    return *this;
}


template <class Model>
typename TimeModel<Model>::const_iterator TimeModel<Model>::begin() const {
    return const_iterator(&segments, segments.begin());
}
template <class Model>
typename TimeModel<Model>::const_iterator TimeModel<Model>::end() const {
    return const_iterator(&segments);
}
template <class Model>
typename TimeModel<Model>::const_iterator TimeModel<Model>::cbegin() const {
    return const_iterator(&segments, segments.begin());
}
template <class Model>
typename TimeModel<Model>::const_iterator TimeModel<Model>::cend() const {
    return const_iterator(&segments);
}





//      class TimeModel
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


template <class Model>
TimeModel<Model>::TimeModel(double t, Probability p)
{
    segments.emplace_back(t, static_cast<LogProbability>(p));
}

template <class Model>
TimeModel<Model>::TimeModel(const TimeModel &other) :
    segments(other.segments),
    pStop_segments(std::move(other.pStop_segments)),
    merge_segment(segments.end()),
    totalWeightCache_valid(other.totalWeightCache_valid),
    totalWeightCache(other.totalWeightCache)
{ }

template <class Model>
Probability TimeModel<Model>::getTotalWeight() const {
    if (!totalWeightCache_valid) {
        totalWeightCache = static_cast<Probability>(totalWeight(segments));
        totalWeightCache_valid = true;
    }
    return totalWeightCache;
}

template <class Model>
TimeModel<Model>& TimeModel<Model>::operator=(const TimeModel &other) {
    segments = other.segments;
    pStop_segments = other.pStop_segments;
    // reset to the new end()-iterator
    merge_segment = segments.end();
    totalWeightCache_valid = other.totalWeightCache_valid;
    totalWeightCache = other.totalWeightCache;
    return *this;
}

// iterators may not be moved from, thus we need a user-defined move constructor
// when moving, we simply reset the merge_segment, because if it's the end-iterator it
// may be invalidated after moving
template <class Model>
TimeModel<Model>::TimeModel(TimeModel &&other) :
    segments(std::move(other.segments)),
    pStop_segments(std::move(other.pStop_segments)),
    merge_segment(segments.end()),
    totalWeightCache_valid(other.totalWeightCache_valid),
    totalWeightCache(other.totalWeightCache)
{ }

template <class Model>
TimeModel<Model>& TimeModel<Model>::operator=(TimeModel &&other) {
    // other.merge_segment may get invalidated after moving if it is the past-end iterator
    // (iterators to elements remain valid)
    bool reset = other.merge_segment == other.segments.end();
    segments = std::move(other.segments);
    pStop_segments = std::move(other.pStop_segments);
    if (reset)
        // reset to the new end()-iterator
        merge_segment = segments.end();
    else
        merge_segment = other.merge_segment; // is not invalidated, can copy
    totalWeightCache_valid = other.totalWeightCache_valid;
    totalWeightCache = other.totalWeightCache;

    return *this;
}

// is static
template <class Model>
LogProbability TimeModel<Model>::totalWeight(const segments_t &segments) {
    Sum<LogProbability> sum;
    for (const LogSegment &s : segments)
        sum += s.sum();
    return sum.sum();
}

// get the total number of discrete time points covered by this TimeModel
template <class Model>
size_t TimeModel<Model>::size() const {
    size_t count = 0;
    for (const LogSegment &s: segments)
        count += static_cast<size_t>(s.end - s.start) + 1;
    return count;
}

// scale every line segment by a constant factor
template <class Model>
TimeModel<Model>& TimeModel<Model>::operator*=(LogProbability value) {
    totalWeightCache *= static_cast<Probability>(value);

    if (value == LogProbability(0)) {
        segments.clear();
        // update merge_segment
        merge_segment = segments.end();
        return *this;
    }
    for (LogSegment &s: segments)
        s *= value;
    return *this;
}

// divide every line segment by a constant factor
template <class Model>
TimeModel<Model>& TimeModel<Model>::operator/=(LogProbability value) {
    if (value == LogProbability(1))
        return *this;

    totalWeightCache /= static_cast<Probability>(value);
    for (LogSegment &s: segments)
        s /= value;
    return *this;
}

// compute the pStop probabilities for the different starting times
// saves the result internally
// call get_pStop to get the summed pStop probability
template <class Model>
void TimeModel<Model>::compute_pStop(const opid_t &opid) {
    pStop_segments.clear();

    // compute pStop for the endpoints of every segments
    for (const LogSegment &segment : segments) {
        // TODO multi-agent
        LogSegment pStop;
        auto durmodel = model::actions[opid[0]]->durationModel;
        double ps, pe;
        durmodel(segment.start, &ps);
        durmodel(segment.end, &pe);

        // if pStop at the start of the values is zero (but not at the end), we set it to the half of the other value
        // as an approximation (zero values are not allowed in the log-linear model)
        // this serves to keep the pStop-segments at the same interval as the weight segments
        // note that pStop is necessarily increasing over time

        if (ps == 0 && pe > 0)
            ps = pe/2;

        if (pe > 0) {
            pStop.add_point(segment.start, LogProbability(ps));
            if (segment.end != segment.start)
                pStop.add_point(segment.end, LogProbability(pe));
        }

        // a completely zero pStop is added as an empty line
        pStop_segments.push_back(pStop);
    }
}

// return the summed probabilities of all w*pStop values (i.e. pStop * weight of the particle at that time)
// - this is the probability that a new action can be executed
template <class Model>
Probability TimeModel<Model>::get_pStopW() const {
    assert(segments.size() == pStop_segments.size());

    // create a temporary list of segments w * pStop (exp(a1+a2 + (b1+b2)x))
    // and call totalWeight() on that list
    segments_t wpStop;

    auto it_s = segments.begin();
    auto it_pStop = pStop_segments.begin();
    for (; it_s != segments.end(); ++it_s, ++it_pStop) {
        if (it_pStop->line.n == 0)
            // if it is an empty line, pStop is zero for the complete segment
            continue;
        LogSegment s = *it_s;
        s *= *it_pStop;
        wpStop.push_back(s);
    }

    return static_cast<Probability>(totalWeight(wpStop));
}

// multiply all starting time probabilities with 1-pStop
// make sure to call compute_pStop beforehand
template <class Model>
void TimeModel<Model>::pStop1m() {
    totalWeightCache_valid = false;
    assert(segments.size() == pStop_segments.size());

    auto pStop = pStop_segments.begin();
    auto s_it = segments.begin();
    while (s_it != segments.end()) {
        if (pStop->line.n == 0) {
            // if it is an empty line, pStop is zero for the complete segment, thus
            // 1-pStop = 1 and we keep the segment unchanged
            ++pStop, ++s_it;
            continue;
        }

        LogSegment &s = *s_it;

        // compute the new weights w*(1-pStop) for the end-points, and compute the new line segment
        LogProbability pStop_start = pStop->predict(pStop->start);
        LogProbability pStop_end = pStop->predict(pStop->end);

        // if, for some numerical reason, pStop is > 1, limit it to 1
        if (pStop_start > LogProbability(1))
            pStop_start = LogProbability(1);
        if (pStop_end > LogProbability(1))
            pStop_end = LogProbability(1);

        LogProbability w_start = (LogProbability(1) - pStop_start) * s.predict(s.start);
        LogProbability w_end = (LogProbability(1) - pStop_end) * s.predict(s.end);

        if (w_start == LogProbability(0) || w_end == LogProbability(0)) {
            // one of the endpoints is zero, which we cannot model in the log-domain
            // the only approximation it would give is constant zero
            // so we simply remove this segment
            ++pStop;
            if (merge_segment == s_it)
                // avoid invalidating the merge_segment iterator, so simply reset it
                merge_segment = segments.end();
            s_it = segments.erase(s_it);
            continue;
        }

        LogSegment s_new(s.start, w_start);
        if (s.start != s.end) {
            s_new.add_point(s.end, w_end);
        }

        // if we simply compute w*(1-pstop) for both end-points, and re-estimate a linear model from only these two points,
        // we get a less "stable" model, because it now consists of only two observations (instead of s.line.n)
        // → We need to recompute sum_y and sum_xy to reflect this scaling
        // (the starting times are fixed, i.e. sum_, sum_x2 and n do not change)

        // update the segment s by copying only the slope and intercept parameters from s_new
        s.line.a = s_new.line.a;
        s.line.b = s_new.line.b;
        s.line.recompute_y();

        ++pStop, ++s_it;
    }
}


template <class Model>
void TimeModel<Model>::add_point(double t, LogProbability p) {
    totalWeightCache_valid = false;
    if (segments.empty()) {
        segments.emplace_back(t, p);
        return;
    }

    LogSegment &last_segment = segments.back();
    assert(last_segment.end < t && "The model already contained the time");

    // check if we should create a new line segment
    // idea #split.2: the predicted values must be "close enough" to the true value (in the log-domain)
    // by computing the error in the log-domain, we allow larger errors in smaller probabilities
    // simple approach: must be accurate up to split.error, e.g. 0.01 (i.e. 1% accurate)
    LogProbability p_predict = last_segment.predict(t);
    double error = std::abs((p_predict.log() - p.log()) / p.log());

    if (error <= Particle<Model>::options.split_error
        || (last_segment.line.n < Particle<Model>::options.min_points && error <= Particle<Model>::options.split_early_error)) {
        // error of predicted value is acceptable
        last_segment.add_point(t, p);
        return;
    }

    // error of predicted value is too large, create new line segment

    // but first, everytime before we create a new line segment,
    // we check if we can merge
    // this is to "control" the number of line segments so they do not increase arbitrarily
    // we do it here, and not at every time step, because as long as the number of segments
    // does not increase, we do not need to control it

    // we test some number of line segment and check if it can be merged with its next segment
    // because merging is not guaranteed, we repeat this test a few number of times
    // currently we repeat it log(a * segments.size()) times. See Options::merge_a for a description of a.
    // we systematically probe the segments in sequence, because this allows a O(1) test for every segment
    // if using a linked list (O(1) removal and insert, and O(1) forward iteration)
    // - if we would randomly select elements, we would either require O(n) lookup (linked list),
    //   or O(n) delete (vector) or O(log(n)) lookup, delete, insert (balanced tree)

    for (int i = 0; i < log(Particle<Model>::options.merge_a * segments.size()) && segments.size() >= 4; i++) {
        if (segments.size() < 4)
            break;

        // find which segment to merge next
        // first, determine if after_segment, which is merge_segment+2, is at the end
        auto after_segment = merge_segment;
        if (after_segment != segments.end())
            after_segment++;
        if (after_segment != segments.end())
            after_segment++;

        // get the next segment to test
        if (after_segment == segments.end()) {
            // i.e. merge_segment == end() or next_segment == end() or after_segment == end()
            // do not merge the last two segments, start over at beginning
            merge_segment = segments.begin();
        } else {
            // get next segment
            merge_segment++;
        }

        // if requested, drop segments with very small weight (< Options::min_logWeight)
        if (Particle<Model>::options.min_logWeight < 0) {
            LogProbability pStart = merge_segment->predict(merge_segment->start);
            LogProbability pEnd = merge_segment->predict(merge_segment->end);
            if (std::max(pStart.log(), pEnd.log()) < Particle<Model>::options.min_logWeight) {
                // segment has less than the required minimum weight - drop it
                merge_segment = segments.erase(merge_segment);
                continue;
            }
        }

        // update next_segment and after_segment
        auto next_segment = merge_segment;
        next_segment++;
        after_segment = next_segment;
        after_segment++;

        // create a copy of the first segment, and merge it with the second
        LogSegment merged = *merge_segment;
        merged.merge(*next_segment);

        // compute error after merging
        double logerror = merged_logerror(merge_segment, after_segment, merged, Particle<Model>::options.merge_exp);

        if (logerror < Particle<Model>::options.merge_error) {
            // remove one segment, overwrite the other with the merged segment
            merge_segment = segments.erase(merge_segment);
            *merge_segment = merged;
        }
    }

    // actually create segment with the new data
    segments.emplace_back(t, p);
}

// add another time model by adding the probabilities for every starting time
// this will change the TimeModel rhs
template <class Model>
TimeModel<Model>& TimeModel<Model>::operator+=(TimeModel &rhs) {
    totalWeightCache_valid = false;
    // when two time models are added, one of them will be a single time at the current time-step
    // when particles are merged, at least one of them made a new transition (otherwise
    // they would have been merged earlier)


    // the calling convention is that this particle always has the single time-point
    assert(segments.size() == 1 && segments.front().line.n == 1 && "*this must always have one time-point");

    LogSegment &single_segment = segments.front();
    double t = single_segment.start;
    LogProbability p = single_segment.predict(t);


    if (rhs.segments.size() == 1 && rhs.segments.front().line.n == 1 && rhs.segments.front().start == t) {
        // two single-point models at the same time, simply add the probabilities and create a new line
        LogProbability p2 = rhs.segments.front().predict(t);
        segments.front().line = linmodel::LogLine<false>(t, p + p2);
        return *this;
    }

    if (rhs.segments.back().end < t) {
        // the single time is not covered by the model - add it
        // first, rhs possibly contains more points, we swap and add the single point
        std::swap(*this, rhs); // this should be fine, we have move semantics

        add_point(t, p);
        return *this;
    }


    // there is a time which is also represented by this model
    // this should not happen, because we first collect all new particles (with exactly one time t = Global::time)
    // and then add this weight to continuing particles (without t = Global::time before)
    // see git commit 49b456a324d2dd76ec62 from 2016-03-01 15:44:59 for code that can handle this case
    assert(false && "There is only one particle with more than one starting time");
    return *this;
}


} // namespace marginal::particle::startTimes:: detail



//      class Particle
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



template <class Model>
void* Particle<Model>::operator new(size_t sz) {
    (void) sz; // is exactly sizeof(Particle) per standard
    pool_small_allocator<Particle> alloc;
    return alloc.allocate(1);
}


template <class Model>
void Particle<Model>::operator delete(void* ptr) {
    pool_small_allocator<Particle> alloc;
    alloc.deallocate(reinterpret_cast<Particle*>(ptr), 1);
}



// If the particle is not already in the set, create a new particle and add it to the list;
// otherwise merge the particles and add the weights to the existing particle - the particle is deleted
template <class Model>
void Particle<Model>::addParticle(Particle* particle, ParticleList& particles, ParticleSet<Particle*>& set) {
    // Check if this particle state was already encountered
    typename ParticleSet<Particle*>::const_iterator setIt = set.find(particle);
    if (setIt == set.end()) {
        // this is a new state → add it to the set and list
        set.insert(particle);
        particles.push_back(particle);
        return;
    }

    // this is a known state → update the existing particle by merging the starting times and weights
    Particle &p = **setIt;
    p.state.startTimes += particle->state.startTimes;

    delete particle;
}

#if DO_MULTI_AGENT
template <class Model>
void Particle<Model>::step(Particle* particle, ParticleList& newParticles, ParticleSet<Particle*>& particleSet, States &states) {
    (void) particle, (void) newParticles, (void) particleSet, (void) states;
    throw std::logic_error("Multi-agent filtering is currently not implemented for efficient starting times. Do not use -P T");
#if 0
    // Loop over all agents and expand the states
    // - For every particle, pre-compute the pStop values and create a startTimes map - the structure is identitical for all successor states, they only differ by the action's saliency
    // This procedure avoid expanding states unnecessary often (e.g. when the outer loop is over all starting times) and summing over all weights too often

    // For the agents sequentially expand the states
    // That is start with the single particle, expand states for the first agent
    // take all the expanded states and expand them all for the second agent, and so on

    // In all but the last iteration over the agents, read particles from particlesFrom and write them to particlesTo
    // Then, swap both.
    // Before the last iteration, make particlesTo point to newParticles, to add the final
    // particles to the resulting list
    ParticleList particlesFrom_;
    ParticleList particlesTo_;
    ParticleList* particlesFrom = &particlesFrom_;
    ParticleList* particlesTo = &particlesTo_;

    // temporary particle set for merging "unfinished" particles.
    // only if the last agent has been executed, particles should be merged with the global particleSet
    // (particle from previous loop iterations of agents must not be merged)
    ParticleSet<Particle*> tempSet;
    ParticleSet<Particle*>* set = &tempSet;

    // pStop for the indidual starting times for every agent
    // Allocate this vector only once, and use it for every particle
    std::vector<Weight> pStop;
    pStop.reserve(particle->state.startTimes.size());

    particlesTo->push_back(particle); // from and to will be swapped at begin of loop over the agents
    particle = NULL; // do not try to use this particle, it will be re-used or deleted somewhere in this loop

    for (size_t agid = 0; agid < Model::n_agents; ++agid) {
        // invariants:
        // particlesTo contains particles expanded from the last iteration
        // particlesFrom is empty

        if (agid < Model::n_agents - 1) {
            // swap from and to
            swap(particlesFrom, particlesTo);
            set->clear();
        } else {
            // last iteration: directly write to result list
            particlesFrom = particlesTo;
            particlesTo = &newParticles;
            set = &particleSet;
        }

        while (!particlesFrom->empty()) {
            // for every state, compute all possible successor states for the current agent
            Particle* p = particlesFrom->back();
            particlesFrom->pop_back();

            expandState(states, agid, p->state.modelState);
            StateDataRec &d = p->state.modelState->second;

            // compute and normalise action probability
            Weight saliencies[NOPS];
            Sum<Weight> sumSaliency = ZeroWeight;

#ifdef NORMALIZE_DISTANCE
            Weight max = -infinity;
            for (int i = 0; i < d.appl[agid]; ++i) {
                Weight sal = calcWeight(*p, i, agid);
                max = std::max(max,sal);
            }
            for (int i = 0; i < d.appl[agid]; ++i) {
                Weight sal = calcWeight(*p, i, agid);
                saliencies[i] = exp(sal-max);
                sumSaliency += saliencies[i];
            }
#else
            for (int i = 0; i < d.appl[agid]; ++i) {
                saliencies[i] = calcWeight(*p, i, agid);
                sumSaliency += saliencies[i];
            }
#endif

            bool deadEnd = prob(sumSaliency) == 0;

#if ALLOW_AGENT_BLOCKING
            // set to false if we could expand at least one state
            bool agentBlocked = true;
#endif

            // compute pStop values for this particle/agent
            pStop.clear();
            for (const typename WeightMap::value_type &st: p->state.startTimes) {
                double pStop_;
                (model::actions[p->state.opid[agid]]->durationModel)(st.first[agid], &pStop_);
                pStop.push_back(pStop_);
            }

            // if this agent executes at least one action, built a preliminary WeightMap for all successor particles
            // this WeightMap is identical - up to the saliency for the action - for all successor particles
            // each successor then gets a copy of this map, scaled by the saliency
            if (!deadEnd) {
                // update all weights with pStop, set the starting time of the agent, and merge identitical starting time combinations
                WeightMap newStartTimes;
                std::vector<Weight>::const_iterator pStop_it = pStop.begin();
                for (const typename WeightMap::value_type &st: p->state.startTimes) {
                    Weight pStop = *pStop_it;
                    pStop_it++;

                    if (pStop == ZeroWeight)
                        continue;
                    startTime_t newTime = st.first;
                    newTime[agid] = Global::time;
                    newStartTimes.insert(std::move(newTime), st.second * pStop);
                }

                // for each reaching state, create a new particle
                for (int i = 0; i < d.appl[agid]; ++i) {
                    if (saliencies[i] == 0)
                        continue;

#if ALLOW_AGENT_BLOCKING
                    agentBlocked = false;
#endif
#if ENDSTAT_REACH_STATES
                    states.states.second(d.appi[agid][i].reached)->visited = true;
#endif

                    State newState(p->state, newStartTimes); // copy previous state and prepare new startTimes
                    newState.opid[agid] = d.appi[agid][i].opid; // set the new opid
                    newState.modelState = d.appi[agid][i].reached;
                    // scale the weights be the action saliency
                    newState.startTimes *= saliencies[i] / sumSaliency.sum();

                    if (newState.startTimes.empty())
                        continue;

                    // create a new particle
                    Particle* newParticle = new Particle(std::move(newState));

                    addParticle(newParticle, *particlesTo, *set);
                } // loop over all applicable actions
            } // if there are applicable actions


#if ALLOW_AGENT_BLOCKING
            WeightMap oldStartTimes;
            if (!agentBlocked)
                p->allBlocked = false;
            else
                // this particle may become blocked, so save the current starting times
                oldStartTimes = p->state.startTimes;
#endif

            // update the current particle by 1-pStop
            p->state.startTimes.scaleWeights1m(pStop.begin());

            if (!p->state.startTimes.empty()) {
                // this very particle/agent stays executing the current action for at least one starting time
#if ALLOW_AGENT_BLOCKING
                p->allBlocked = false;
#endif
                addParticle(p, *particlesTo, *set);
            } else {
#if !ALLOW_AGENT_BLOCKING
                // this agent does not continue its current action
                delete p;
#else
                // this agent does not continue its current action
                // usually, we simply throw this particle away
                // However, if the agent is blocked, we keep it (some other agent may not be blocked)
                if (agentBlocked && (!(p->allBlocked) || agid < Model::n_agents - 1)) {
                    // this agent is blocked, and other agents are (or may be) not blocked
                    if (agid == 0)
                        p->allBlocked = true;
                    else
                        p->allBlocked &= true;
                    // set opid
                    p->state.opid[agid] = model::NoOpId;

                    // set the starting time of the agent to the current time, but leave the weight untouched
                    // (p->state.startTimes is currently empty and will be set from the oldStartTimes)
                    for (const typename WeightMap::value_type &st: oldStartTimes) {
                        startTime_t newTime = st.first;
                        newTime[agid] = Global::time;
                        p->state.startTimes.insert(std::move(newTime), st.second);
                    }

                    addParticle(p, *particlesTo, *set);
                } else
                    // this particle has been expanded or all agents are blocked → finally throw particle away
                    delete p;
#endif // ALLOW_AGENT_BLOCKING
            } // if particle does not continue
        } // loop over all particles
    } // loop over all agents
#endif
}


#else // DO_MULTI_AGENT


template <class Model>
void Particle<Model>::step(Particle* particle, ParticleList& newParticles, ParticleSet<Particle*>& set, States &states) {
    expandState(states, 0, particle->state.modelState);

    StateDataRec &d = particle->state.modelState->second;
    // compute and normalise action probability
    Weight saliencies[NOPS];
    Sum<Weight> sumSaliency = ZeroWeight;

#ifdef NORMALIZE_DISTANCE
    Weight max = -infinity;
    for (int i = 0; i < d.appl[0]; ++i) {
        Weight sal = calcWeight(*particle, i, 0);
        max = std::max(sal, max);
    }

    for (int i = 0; i < d.appl[0]; ++i) {
        Weight sal = calcWeight(*particle, i, 0);
        saliencies[i] = exp(sal-max);
        sumSaliency += saliencies[i];
    }

#else
    for (int i = 0; i < d.appl[0]; ++i) {
        saliencies[i] = calcWeight(*particle, i, 0);
        sumSaliency += saliencies[i];
    }
#endif

    // pre-compute pStop - this factor is identical for all successor states
    particle->state.startTimes.compute_pStop(particle->state.opid);
    Weight pStopW = particle->state.startTimes.get_pStopW();

    // for each reaching state, create a new particle
    for (int i = 0; sumSaliency.sum() > ZeroWeight && i < d.appl[0]; ++i) {
        Weight weight = saliencies[i] / sumSaliency.sum() * pStopW;
        if (weight == ZeroWeight)
            continue;

#if ENDSTAT_REACH_STATES
        d.appi[0][i].reached->second.visited = true;
#endif

        // new particle → set single starting time
        TimeModel newTimeModel(Global::time, weight);

        // create the new particle state based on the reached modelstate and new start time
        State newState = State(particle->state, std::move(newTimeModel));
        newState.opid[0] = d.appi[0][i].opid;
        newState.modelState = d.appi[0][i].reached;

        // create new particle
        Particle* newParticle = new Particle(std::move(newState));
        addParticle(newParticle, newParticles, set);
    }
}
#endif // DO_MULTI_AGENT



template <class Model>
void Particle<Model>::doCallbacksAndObservations() {
    model::doCallbacksAndObservations(state.opid, state.modelState->first);
}



template <class Model>
void Particle<Model>::update() {
#if !(defined(HAS_CALLBACKS) || defined(USE_ACTION_OBSERVATION))
    StateDataRec &gpt = state.modelState->second;
    // We can base the observation only on the state.
    // This allows us to cache the result.
    //
    // If action callbacks are enabled, instead, we have to make observation every time, as
    // action callbacks depend not only on the state, but also the
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
    model::pre_observe_state(s);

    doCallbacksAndObservations();

    // let the model compute pyx (must be independent of action duration!)
    Probability pyx = 1;
    for (size_t i = 0; i < Model::n_agents; i++)
        pyx *= model::post_observe_action(i);
    pyx *= model::post_observe_state(s);

    set_pyx(pyx);
    updateWeight(pyx);
}



template <class Model>
Probability Particle<Model>::get_pyx() const {
#if (defined(HAS_CALLBACKS) || defined(USE_ACTION_OBSERVATION))
    return pyx;
#else
    StateDataRec &gpt = state.modelState->second;
    if (gpt.time != Global::time) {
        std::cerr << "Particle has old observation (" << gpt.time << " / " <<  Global::time << ")" << std::endl;
        throw std::runtime_error("Cannot use old pyx.");
    }
    return gpt.pyx;
#endif
}



template <class Model>
void Particle<Model>::set_pyx(Probability _pyx) {
#if (defined(HAS_CALLBACKS) || defined(USE_ACTION_OBSERVATION))
    pyx = _pyx;
#else
    StateDataRec &gpt = state.modelState->second;
    gpt.time = Global::time;
    gpt.pyx = _pyx;
#endif
}



// calcWeight is static
template <class Model>
double Particle<Model>::calcWeight(const Particle& particle, int i, int agid) {
    StateDataRec &d = particle.state.modelState->second;
    int curOpid = particle.state.opid[agid];
    ApplInfo ap = d.appi[agid][i];

    model::StatePtr s = &particle.state.modelState->first;
    (void)s;

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

    double sal = ap.weight[particle.state.goalIndex()];
    sal = pow(sal, 1/parameters.nroot);

#ifdef NORMALIZE_DISTANCE
    sal = sal+parameters.weightBias;
#else
    sal = exp(sal+parameters.weightBias);
#endif
    return sal;
}



// Definition of hash

template <class Model>
size_t hash_value(Particle<Model> const & p) {
    size_t hash = 0;
    boost::hash_combine(hash, p.state);
    return hash;
}


// free function for the prediction

template <class Model>
StepResult Particle<Model>::step(ParticleList &particles, States &states) {
    StepResult stepResult;

    ParticleSet<Particle*> particleSet;
    ParticleList newParticles; // successor particles
    ParticleList oldParticles; // continuing particles


    // first generate all new particles, merge them (with a single start time t = Global::time),
    // then merge these with the particles continuing their execution (* 1-pStop)
    // this strategy makes it easier to write the TimeModel::add_point method, because we never need
    // to add a weight to a TimeModel for a starting time which is already there (unless it is the only
    // weight (for all new particles), for which it is a simple addition)

    // TODO multi-agent?

    for (Particle *p : particles) {
        Probability oldWeight = p->getTotalWeight();
        size_t count_new = newParticles.size();

        // create successor particles and compute pStop for all particles
        Particle::step(p, newParticles, particleSet, states);

        // multiply particle with 1-pStop and check if it continues
        p->state.startTimes.pStop1m();

        if (p->state.startTimes.getTotalWeight() != ZeroWeight)
            // this very particle stays executing the current action for at least one starting time
            oldParticles.push_back(p);
        else {
            delete p;

            if (count_new == newParticles.size()) {
                // no successor particles and particle does not continue action
                // this particles is dead
                stepResult.numDead++;
                stepResult.weightDead += oldWeight;
            }
        }
    }

    // now merge successor particles with same state into old particles
    for (Particle *p : oldParticles)
        addParticle(p, newParticles, particleSet);

    // continue with the new particles
    particles.swap(newParticles);

    return stepResult;
}




//      static fields
// #############################################################################


template <class Model>
typename Particle<Model>::Options Particle<Model>::options;



//      explicit template instantiations
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

template class detail::TimeModel<model::Model>;
template struct State<model::Model>;
template class Particle<model::Model>;



} // namespace marginal::particle:: startTimes
} // namespace marginal:: particle
} // namespace marginal
