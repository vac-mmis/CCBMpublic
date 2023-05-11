#pragma once
#include "basic_sum.h"

/**
 * Standard addition
 */
template <typename InputIt,typename T>
inline T standard_add(InputIt begin,InputIt end) {
    T result = 0;
    for (InputIt i=begin; i!=end; ++i)
        result += *i;
    return result;
}

template <typename T>
class StandardSum : public basic_sum<T> {
    T value_;

public:
    StandardSum();
    StandardSum(const T &value);

    template <typename InputIt>
    StandardSum(InputIt begin,InputIt end);

    T sum() const {
        return value_;
    }

    void add(const T &x) {
        value_ += x;
    }

    void reset(const T &x = 0) {
        value_ = x;
    }
};

template <typename T>
StandardSum<T>::StandardSum()
    : value_(0) {
}

template <typename T>
StandardSum<T>::StandardSum(const T &value)
    : value_(value) {
}

template <typename T>
template <typename InputIt>
StandardSum<T>::StandardSum(InputIt begin, InputIt end)
    : value_(standard_add(begin,end)) {
}
