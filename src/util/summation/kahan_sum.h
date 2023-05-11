#pragma once
#include "basic_sum.h"

/**
 * Addition with Kahan
 */
template <typename InputIt,typename T>
T kahan_add(InputIt begin,InputIt end) {
    T result = 0;
    T c = 0.;
    for (InputIt i=begin; i!=end; ++i) {
        T y = *i - c;
        T t = result + y;
        c = ( t - result ) - y;
        result = t;
    }
    return result;
}

template <typename T>
class KahanSum : public basic_sum<T> {
    T value_;
    T carry_;

public:
    KahanSum();
    KahanSum(const T &value);

    template <typename InputIt>
    KahanSum(InputIt begin,InputIt end);

    T sum() const {
        return value_;
    }

    void add(const T &x) {
        T y = x - carry_;
        T t = value_ + y;
        carry_ = ( t - value_ ) - y;
        value_ = t;
    }

    void reset(const T &x = 0) {
        value_ = x;
        carry_ = 0;
    }

    bool operator==(const KahanSum<T> &x) const {
        return value_ == x.value_ && carry_ == x.carry_;
    }
    bool operator!=(const KahanSum<T> &x) const { return !(*this == x); }
    bool operator> (const KahanSum<T> &x) const {
        if (value_ != x.value_)
            return value_ > x.value_;
        return carry_ < x.carry_;
    }
    bool operator>=(const KahanSum<T> &x) const {
        if (value_ != x.value_)
            return value_ >= x.value_;
        return carry_ <= x.carry_;
    }
    bool operator< (const KahanSum<T> &x) const { return !(*this >= x); }
    bool operator<=(const KahanSum<T> &x) const { return !(*this > x); }
};

template <typename T>
KahanSum<T>::KahanSum()
    : value_(0),carry_(0) {
}

template <typename T>
KahanSum<T>::KahanSum(const T &value)
    : value_(value),carry_(0) {
}

template <typename T>
template <typename InputIt>
KahanSum<T>::KahanSum(InputIt begin,InputIt end)
    : value_(0),carry_(0) {
    for (InputIt iter=begin; iter!=end; ++iter)
        add(*iter);
}
