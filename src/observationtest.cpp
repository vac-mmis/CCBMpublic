#include <iostream>
#include <stdexcept>

#include "util/random.h"

#include "model.h"
#include "obsmodel.h"

using namespace std;


void setStateBit(model::StateRec *, unsigned int bitNum);


int main(int argc, char** argv) {
    std::ios::sync_with_stdio(false);
    try {
        model::StateRec rec;
        for (int i=1; i<argc; ++i) {
            setStateBit(&rec, std::atoi(argv[i]));
        }
        while (std::cin) {
            model::fetchObservation();
            for (size_t agid = 0; agid < model::Model::n_agents; agid++)
                model::pre_observe_action(agid);
            model::pre_observe_state(&rec);

            Probability pyx = 1;
            for (size_t agid = 0; agid < model::Model::n_agents; agid++)
                pyx *= model::post_observe_action(agid);
            pyx *= model::post_observe_state(&rec);

            std::cout << pyx << '\n';
        }
    } catch (const exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
    } //try-block
    return 0;
}

void setStateBit(model::StateRec *state, unsigned int bitNum) {
    //FIXME
    //StateRec is not a POD type by the C++03 standard
    //(non-trivial constructor) so this may not be portable.
    //StateRec should provide a method for accessing different bits
    //at runtime, or better yet, replace StateRec with a
    //std::bitset which already provides this functionality.
    //But std::bitset is not designed for efficiency
    typedef unsigned int word;
    const word bitsPerWord = CHAR_BIT * sizeof(word);
    if (bitNum > sizeof(*state) * CHAR_BIT) {
        throw std::runtime_error("Bit number too large!");
    }
    word *currentWord = reinterpret_cast<word*>(state);
    while (bitNum > (bitsPerWord-1)) {
        ++currentWord;
        bitNum -= bitsPerWord;
    }
    *currentWord |= 1 << bitNum;
}
