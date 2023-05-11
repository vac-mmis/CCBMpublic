#pragma once

#include <vector>
#include <istream>

class CVActionTable {
private:
    unsigned int index;
    std::vector<std::vector<double> > table;

public:
    CVActionTable();
    bool init(const char* filename);
    bool next();
    double getP(int index);
};

CVActionTable::CVActionTable() {
    index=0;
}

bool CVActionTable::init(const char* filename) {
    std::ifstream wFile(filename, std::ifstream::in);
    if (table.size() != 0) {
        std::cerr << "Action table was already initialized" << std::endl;
        return false;
    }

    while (wFile.good()) {
        std::vector<double> weights;
        for (int i=0; i<NOPS; i++) {
            double w;
            wFile >> w;
            weights.push_back(w);
        }
        table.push_back(weights);
        weights.clear();

    }
    // check for fail cases
    return (table.size() > 0);

}

bool CVActionTable::next() {
    if (table.size() > index) {
        index++;
        return true;
    }
    return false;
}

double CVActionTable::getP(int acIndex) {
    return table[index][acIndex];
}
