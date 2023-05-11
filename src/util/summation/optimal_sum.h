#pragma once
#include <algorithm>
#include <vector>
#include "basic_sum.h"

/**
 * Addition with sorted list
 */
template <typename InputIt,typename T>
T optimal_add(InputIt begin,InputIt end) {
    T result = 0;
    std::sort(begin,end);
    for (InputIt i=begin; i!=end; ++i)
        result += *i;
    return result;
}

template <typename T>
class OptimalSum : public basic_sum<T> {
    // having values_ mutable is important for having sum() const
    // which again is important for various operatorXY()
    mutable std::vector<T> values_;

public:
    OptimalSum();
    OptimalSum(const T &init);

    template <typename InputIt>
    OptimalSum(InputIt begin,InputIt end);

    T sum() const {
        return optimal_add<typename std::vector<T>::iterator, T>(values_.begin(),values_.end());
    }

    void add(const T &x) {
        values_.push_back(x);
    }

    void reset(const T &x = 0) {
        values_.clear();
        if (x != 0)
            add(x);
    }

    OptimalSum<T>& operator+=(const T &x) {
        add(x);
        return *this;
    }

    OptimalSum<T>& operator+=(const OptimalSum<T> &x) {
        add(x.sum());
        return *this;
    }

    OptimalSum<T>& operator-=(const T &x) {
        add(-x);
        return *this;
    }

    OptimalSum<T>& operator-=(const OptimalSum<T> &x) {
        add(-x.sum());
        return *this;
    }
};

template <typename T>
OptimalSum<T>::OptimalSum()
    : values_() {
}

template <typename T>
OptimalSum<T>::OptimalSum(const T &init)
    : values_() {
    add(init);
}

template <typename T>
template <typename InputIt>
OptimalSum<T>::OptimalSum(InputIt begin, InputIt end)
    : values_(begin,end) {
}

template <typename T>
inline OptimalSum<T> operator+(OptimalSum<T> lhs, const T &rhs) {
    lhs += rhs;
    return lhs;
}

template <typename T>
inline OptimalSum<T> operator+(const T &lhs, OptimalSum<T> rhs) {
    return rhs + lhs;
}

template <typename T>
inline OptimalSum<T> operator+(OptimalSum<T> lhs, const OptimalSum<T> &rhs) {
    lhs += rhs;
    return lhs;
}

template <typename T>
inline OptimalSum<T> operator-(OptimalSum<T> lhs, const T &rhs) {
    lhs -= rhs;
    return lhs;
}

template <typename T>
inline OptimalSum<T> operator-(const T &lhs, OptimalSum<T> rhs) {
    return rhs - lhs;
}

template <typename T>
inline OptimalSum<T> operator-(OptimalSum<T> lhs, const OptimalSum<T> &rhs) {
    lhs -= rhs;
    return lhs;
}

template <typename T>
inline OptimalSum<T> operator-(const OptimalSum<T> &sum) {
    return 0-sum;
}
