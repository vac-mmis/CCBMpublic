#pragma once

class StepInfo {
public:
    StepInfo(std::string _name) {
        name = _name;
        isNew = false;
        pStop = LogProbability(0);
        saliency = LogProbability(0);
        wSaliency = 0.0;
        pSaliency = LogProbability(0);
        specificity = LogProbability(0);
        wSpecificity = 0.0;
        pSpecificity = LogProbability(0);
        goalDistance = 0.0;
        wGoalDistance = 0.0;
        pGoalDistance = LogProbability(0);
        recency = LogProbability(0);
        wRecency = 0.0;
        pRecency = LogProbability(0);
        refractoriness = LogProbability(0);
        wRefractoriness = 0.0;
        pRefractoriness = LogProbability(0);
        revisit = 0.0;
        pRevisit = LogProbability(0);
        pat = LogProbability(0); // prob. of action
        bfactor = 0;
        pyx = LogProbability(0);
        paAgent1t = LogProbability(1);
    }

    StepInfo() :
        StepInfo("")
    { }

    StepInfo& operator+=(const StepInfo &rhs) {
        saliency += rhs.saliency;
        specificity += rhs.specificity;
        recency += rhs.recency;
        refractoriness += rhs.refractoriness;
        pat += rhs.pat;

        revisit += exp(rhs.revisit);

        wGoalDistance += exp(rhs.wGoalDistance);
        return *this;
    }

    void normalizeP(const StepInfo &sum) {
        pSaliency = saliency / sum.saliency;
        pSpecificity = specificity / sum.specificity;
        pRecency = recency / sum.recency;
        pRefractoriness = refractoriness / sum.refractoriness;

        pRevisit = LogProbability(revisit, log_domain) / LogProbability(sum.revisit); // exp(revisit) / sum.revisit
        pGoalDistance = LogProbability(wGoalDistance, log_domain) / LogProbability(sum.wGoalDistance); // exp(wGoalDistance) / sum.wGoalDistance
    }

// step infomation
    bool isNew;
    std::string name;
// step probabilities
    LogProbability pStop;
    LogProbability saliency;
    double wSaliency;
    LogProbability pSaliency;
    LogProbability specificity;
    double wSpecificity;
    LogProbability pSpecificity;
    double goalDistance;
    double wGoalDistance;
    LogProbability pGoalDistance;
    LogProbability recency;
    double wRecency;
    LogProbability pRecency;
    LogProbability refractoriness;
    double wRefractoriness;
    LogProbability pRefractoriness;
    double revisit;
    LogProbability pRevisit;
    LogProbability pat; //  prob. of action
    LogProbability patsum; //  summed prob. of all applicable actions,
    LogProbability patn; // normalized prob. of action
    int bfactor; // branching factor
    double hitcount;
    double entropy;

// agent plan probabilities until now
    LogProbability paAgent1t; // agents plan probability (not incl. pStop)

// values for the whole timestep
    static LogProbability paTeam1t; // prob. of plan so far (product of all single agents plan probability )
    static LogProbability pyx; // prob. of sensor data for the current state
    static LogProbability paTeam1ty; // prob of plan so far including sensordata (incl. pStop)
    static LogProbability fll;
    static LogProbability pyx1t; // prob of this step (incl. pStop)


    void print(std::ostream& o) {
        o << (isNew ? "*;" : ";");
        o << name << ";";
        o << pStop << ";"
          << saliency << ";" << wSaliency << ";" << pSaliency << ";"
          << specificity << ";" << wSpecificity << ";" << pSpecificity << ";"
          << goalDistance << ";" << wGoalDistance << ";" << pGoalDistance << ";"
          << recency << ";" << wRecency << ";" << pRecency << ";"
          << refractoriness << ";" << wRefractoriness << ";" << pRefractoriness << ";" 
          << revisit << ";" << pRevisit << ";"
          << bfactor << ";" << hitcount << ";" << (0-entropy)
          << ";" << pat << ";" << patsum << ";" << patn << ";"
          << p_value(isNew ? paAgent1t : (paAgent1t * pStop)) << ";" // for new actions the durationprob is already included
          << p_logValue(isNew ? paAgent1t : (paAgent1t * pStop)); // for new actions the durationprob is already included
    }
    void header(std::ostream &o, int agid) {
        o << agid << "_isnew;"
          << agid << "_name;"
          << agid << "_pStop;"
          << agid << "_sal;"
          << agid << "_wsal;"
          << agid << "_psal;"
          << agid << "_spec;"
          << agid << "_wspec;"
          << agid << "_pspec;"
          << agid << "_gd;"
          << agid << "_wgd;"
          << agid << "_pgd;"
          << agid << "_rec;"
          << agid << "_wrec;"
          << agid << "_prec;"
          << agid << "_ref;"
          << agid << "_wref;"
          << agid << "_pref;"
          << agid << "_revisit;"
          << agid << "_previsit;"
          << agid << "_bfactor;"
          << agid << "_hitcount;"
          << agid << "_entropy;"
          << agid << "_pat;"
          << agid << "_patsum;"
          << agid << "_patn;"
          << agid << "_paAgent1t;"
          << agid << "_logpaAgent1t";
    }

    void reset() {
        isNew = false;
        name = "";
        pStop = LogProbability(0);
        hitcount = 1.0;

        saliency = LogProbability(0);
        wSaliency = 0.0;
        pSaliency = LogProbability(0);
        specificity = LogProbability(0);
        wSpecificity = 0.0;
        pSpecificity = LogProbability(0);
        goalDistance = 0;
        wGoalDistance = 0.0;
        pGoalDistance = LogProbability(0);
        recency = LogProbability(0);
        wRecency = 0;
        pRecency = LogProbability(0);
        refractoriness = LogProbability(0);
        wRefractoriness = 0;
        pRefractoriness = LogProbability(0);
        revisit = 0;
        pRevisit = LogProbability(0);
        pat = LogProbability(0); //  prob. of action
        patsum = LogProbability(0); //  summed prob. of all applicable actions
        patn = LogProbability(0); // normalized prob. of action
        bfactor = 0;
        entropy = 0;
    }
};

std::ostream& operator<<(std::ostream& os, StepInfo sf) {
    sf.print(os);
    return os;
}

LogProbability StepInfo::pyx(1.0);

LogProbability StepInfo::paTeam1t(1.0);
LogProbability StepInfo::paTeam1ty(1.0);
LogProbability StepInfo::pyx1t(1.0);
LogProbability StepInfo::fll(1.0);
