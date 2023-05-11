#pragma once

#include <cstdlib>
#include <iostream>
#include <random>

class Random {
public:
    virtual void seed(int i) = 0;
    virtual double random() = 0;
//virtual double operator()(){ return random(); };
};


class RandomM : public Random {
private:
    std::mt19937    rng;
    std::uniform_real_distribution<double> dist;
public:
    RandomM():
        rng(0),
        dist(0.0, 1.0)
    { }
    
    void seed(int i) { rng.seed(i); }
    double random() { return dist(rng); }

};


class Random48 : public Random {
    std::minstd_rand rng;
    std::uniform_real_distribution<double> dist;
public:
    Random48():
        rng(0),
        dist(0.0, 1.0)
    { }
    
    void seed(int i) { rng.seed(i); }
    double random() { return dist(rng); }
};


// returns random numbers in the range [0, i-1] when calling operator()(i)
class RandomInt {
    std::mt19937    rng;

public:
    explicit RandomInt() : rng(0)
    { }

    void seed(std::mt19937::result_type s) { rng.seed(s); }

    int operator()(unsigned int i) {
        std::uniform_int_distribution<unsigned int> dist(0, i-1);
        return dist(rng);
    }
};
