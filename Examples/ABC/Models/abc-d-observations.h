#pragma once

#define HAS_ESTIMATOR

#define NLoc 7
#define NAgnt 2

#define SUPPORTS_OBS_EQUIV 1
#define SUPPORTS_OBS_HASH 1

extern const char *agtNames[];
extern const char *locNames[];

extern int agentLocs[NAgnt][NLoc];

void signalExpectedFromSensor(int sid);
void setAgentLoc(int agent, int locid);

template <class Particle>
class Estimate : public ::Estimator<Particle> {
    Weight avals[NAgnt][NLoc];
    double time;
public:
    std::ostream& printHeader(std::ostream &os) const {
        os << "Time LL";
        for(int i=0;i<NAgnt;i++)
            for(int j=0;j<NLoc;j++)
                os << ' ' << agtNames[i] << '.' << locNames[j];
        os << std::endl;
        return os;
    }

    void start(double time) {
        this->time = time;
        for (int i = 0;i < NAgnt; i++)
            for (int j = 0;j < NLoc; j++) {
                avals[i][j] = ZeroWeight;
            }
    }

    void pre_collect(const Particle &p) {
        (void) p;

        for (int i = 0; i < NAgnt; i++)
            for (int j = 0; j < NLoc; j++)
                agentLocs[i][j] = 0;
    }

    // FIXME TODO the callbacks should be done for each state of the Particle's state-distribution separately
    // Every state this particle represents may be agents at different locations
    void collect(const Particle &p) {
        for (int i = 0; i < NAgnt; i++)
            for(int j = 0; j < NLoc; j++)
                avals[i][j] += Weight(agentLocs[i][j]) * p.getTotalWeight();
    }

    void finish() {}

    std::ostream& print(std::ostream &os) const {
        os << time << ' ' << theLoglik();
        for(int i=0;i<NAgnt;i++) {
            for(int j=0;j<NLoc;j++)
                os << ' ' << avals[i][j];
        }
        os << std::endl;
        return os;
    }
};
