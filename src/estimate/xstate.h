#pragma once

#include <boost/lexical_cast.hpp>
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/filter/gzip.hpp>

#include "estimate.h"

/*
Estimator for dumping the complete X-state distribution represented by the particles.
The estimator is configured with a base filename.
For every timestep, it will create a gzip compressed file <name><time>.gz containing the header
> Time State Agent Init Goal Action Start Weight
with one line per X-state.
The weights are printed in the log-domain.
The states are given unique numbers to distinguish them (only per time-step). -- maybe we can also only print the pointer address?

The output stream is ignored by this estimator.
*/

template <class Particle>
class XStateEstimator : public Estimator<Particle> {
private:
    typedef typename Particle::Model Model;

    static const boost::iostreams::gzip_params gzip_params;

    /**
     * Base filename for the compressed files created at every time step.
     * The full filename has the form <baseName><time>.gz
     */
    const std::string baseName;

    /// output stream for the current time-step
    boost::iostreams::filtering_ostream ofullXStateDist;

    typedef std::unordered_map<StateTablePtr, size_t, StateTablePtr_Hash, StateTablePtr_Equal> StateNumMap;
    /// Assign unique state numbers, which are not required to be identitical for different time steps
    StateNumMap stateNumMap;
    /// Keeps track of largest state number for the current time-step (for stateNumMap)
    size_t maxStateNum;

public: // public methods

    XStateEstimator(std::string baseName) :
        baseName(baseName)
    { }

    // print a header if the output format allows this, (the first column is mostly "Time")
    std::ostream& printHeader(std::ostream &os) const {
        // start method prints header for every time-step
        return os;
    }

    // called once per time-step before the estimation starts
    void start(double time) {
        namespace bio = boost::iostreams;
        std::string filename = baseName + boost::lexical_cast<std::string>(time) + ".gz";

        // stream has been resetted previously in finish, thus we can add new streams to the chain
        ofullXStateDist.push(bio::gzip_compressor(gzip_params));
        ofullXStateDist.push(bio::file_sink(filename));

        ofullXStateDist << "Time State Agent Init Goal Action Start Weight\n";

        stateNumMap.clear();
        maxStateNum = 0;
    }

    void pre_collect(const Particle &p) {
        (void) p;
        // intentionally empty -- not required
    }

    void collect(const Particle &p) {
        // iterate over all <S, A, T, I, G> states
        // the iterator may either be over std::pair< ::State<Model>, Weight> or std::pair<const ::State<Model>&, Weight>

        for(const auto &stateEst : p.template getDistribution<::State<Model>>()) {
            const ::State<Model> &state = stateEst.first;
            const Weight w = stateEst.second;

            auto stateNumIt = stateNumMap.find(state.modelState);
            size_t stateNum;
            if (stateNumIt == stateNumMap.end()) {
                stateNum = maxStateNum++;
                stateNumMap.insert(StateNumMap::value_type(state.modelState, stateNum));
            } else
                stateNum = stateNumIt->second;

            for (size_t agid = 0; agid < Model::n_agents; agid++) {
                ofullXStateDist << Global::time << ' ' << stateNum << ' ' << agid << ' ' << state.initialIndex << ' ' << state.goalIndex << ' ' << state.opid[agid] << ' ' << state.startTime[agid] << ' ' << p_logValue(prob(w)) << '\n';
            }
        }
    }

    void finish() {
        ofullXStateDist.flush();
        ofullXStateDist.reset(); // closes filters and devices in chain
    }

    std::ostream& print(std::ostream &os) const {
        // everything is printed in the collect method
        return os;
    }
}; // class Estimator


// "best" values using maximum memory to compress as best as possible
template <class Particle>
const boost::iostreams::gzip_params XStateEstimator<Particle>::gzip_params(boost::iostreams::zlib::default_compression, boost::iostreams::zlib::deflated, 15, 9);
