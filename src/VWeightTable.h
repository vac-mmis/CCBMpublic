#include <iostream>
#include <vector>

#include "global.h"

struct WeightDist {

    double sal;
    double spec;
    double rec;
    double ref;
    double revisit;
    double goalDistance;

    std::istream& read(std::istream& i) {
        i >> sal >> spec >> rec >> ref >> revisit >> goalDistance;
        return i;
    }
};

std::istream& operator>>(std::istream& i, WeightDist &w) {
    return w.read(i);
}


class CVweightTable {
private:
    unsigned int index;
    std::vector<WeightDist> weights;

public:
    CVweightTable();
    bool init(char* filename);
    bool next();
    double getSal() {
        return weights[index].sal;
    }
    double getSpec() {
        return weights[index].spec;
    }
    double getRec() {
        return weights[index].rec;
    }
    double getRef() {
        return weights[index].ref;
    }
    double getRevisit() {
        return weights[index].revisit;
    }
    double getGoalDistance() {
        return weights[index].goalDistance;
    }

    void adjustWeights();
};

CVweightTable::CVweightTable() {
    index = 0;
}

bool CVweightTable::init(char * filename) {
    std::ifstream wFile(filename, std::ifstream::in);
    if (weights.size() != 0) {
        std::cerr << "Weight table was already initialized" << std::endl;
        return false;
    }

    while (wFile.good()) {
        WeightDist w;
        wFile >> w;
        weights.push_back(w);
    }
    // check for fail cases
    return (weights.size() > 0);
}

bool CVweightTable::next() {
    if (weights.size() > index) {
        index++;
        return true;
    }
    return false;
}

void CVweightTable::adjustWeights() {
    parameters.weightFactor = getGoalDistance();
    parameters.weightSaliency = getSal();
    parameters.weightRecency = getRec();
    parameters.weightRefractoriness = getRef();
    parameters.weightSpecificity = getSpec();
    parameters.revisitingFactor = getRevisit();
}
