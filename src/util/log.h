#pragma once

#include <cmath>
#include <limits>
#include <iosfwd>

#include <stdexcept>
#include <boost/functional/hash.hpp>

#include "summation.h"

// arithmetic in the log-domain

inline double log_add(double x, double y);
inline double log_subtract(double x, double y);
inline double log_mult(double x, double y);
inline double log_divide(double x, double y);

struct log_domain_t {};
extern const log_domain_t log_domain;


// class encapsulating the log-value, overriding operators
// the class provides explicit conversion operators to convert between log-domain and value-domain
// to prevent accidential mixing of operators, these conversions are always explicit
template<bool allow_negative=false>
class Log {
protected:
    double l;

public:
    // Initialise to 0
    Log() :
        l(-std::numeric_limits<double>::infinity())
    { }

    // conversion from the value-domain
    Log(double value) :
        l(::log(value))
    { }

    // construct a Log-object directly from the log-domain
    Log(double log, log_domain_t) :
        l(log)
    { }

    // conversion back to the value-domain
    explicit operator double() const { return exp(l); }

    double log() const { return l; }

    constexpr bool is_negative() const { return false; }

    Log& operator+=(const Log& other) {
        l = log_add(l, other.l);
        return *this;
    }

    Log& operator-=(const Log& other) {
        l = log_subtract(l, other.l);
        return *this;
    }

    Log& operator*=(const Log& other) {
        l = log_mult(l, other.l);
        return *this;
    }

    Log& operator/=(const Log& other) {
        l = log_divide(l, other.l);
        return *this;
    }

    bool operator==(const Log& rhs) const { return l == rhs.l; }
    bool operator!=(const Log& rhs) const { return !(*this == rhs); }
    bool operator<(const Log& rhs) const { return l < rhs.l; }
    bool operator>(const Log& rhs) const { return l > rhs.l; }
    bool operator>=(const Log& rhs) const { return l >= rhs.l; }
    bool operator<=(const Log& rhs) const { return l <= rhs.l; }
};

// template specialisation also allowing negative values
template <>
class Log<true> {
protected:
    double l;
    bool neg;

public:
    // Initialise to 0
    Log() :
        l(-std::numeric_limits<double>::infinity()),
        neg(false)
    { }

    // conversion from the value-domain
    Log(double value) :
        l(::log(std::abs(value))),
        neg(value < 0)
    { }

    // construct a Log-object directly from the log-domain
    Log(double log, log_domain_t) :
        l(log),
        neg(false)
    { }

    // construct a Log-object directly from the log-domain
    Log(double log, bool negative, log_domain_t) :
        l(log),
        neg(negative)
    { }

    // conversion from non-negative log-value
    Log(const Log<false> l) :
        l(l.log()),
        neg(false)
    { }

    // conversion back to the value-domain
    explicit operator double() const { return neg ? -exp(l) : exp(l); }

    double log() const { return l; }

    bool is_negative() const { return neg; }

private:
    // either add x + y (leave sign unchanged), or compute x - y and invert sign if x < y
    void add_sub(double other, bool add) {
        constexpr double ninf = -std::numeric_limits<double>::infinity();
        if (add) {
            l = log_add(l, other);
            // neg unchanged
        } else {
            if (l >= other) {
                l = log_subtract(l, other);
                // neg unchanged, except for 0
                if (l == ninf)
                    neg = false;
            } else {
                l = log_subtract(other, l);
                if (l == ninf)
                    neg = false;
                else
                    neg = !neg;
            }
        }

    }

public:
    Log& operator+=(const Log& other) {
        // x + y and -x + -y = -(x + y) need add with sign unchanged
        // -x + y = -(x - y) and x - y need subtract with sign inverted if x < y
        add_sub(other.l, neg == other.neg);
        return *this;
    }

    Log& operator-=(const Log& other) {
        // x - -y = x + y and -x - y = -(x + y) need add with sign unchanged
        // x - y and -x - -y = -(x - y) need subtract with sign inverted if x < y
        add_sub(other.l, neg != other.neg);
        return *this;
    }

    Log& operator*=(const Log& other) {
        l = log_mult(l, other.l);
        neg = neg != other.neg;
        return *this;
    }

    Log& operator/=(const Log& other) {
        l = log_divide(l, other.l);
        neg = neg != other.neg;
        return *this;
    }

    bool operator==(const Log& rhs) const { return l == rhs.l && neg == rhs.neg; }
    bool operator!=(const Log& rhs) const { return !(*this == rhs); }
    bool operator<(const Log& rhs) const {
        if (neg != rhs.neg) return neg;
        if (!neg) return l < rhs.l;
        return l > rhs.l;
    }
    bool operator<=(const Log& rhs) const { return *this == rhs || *this < rhs; }
    bool operator>(const Log& rhs) const { return !(*this <= rhs); }
    bool operator>=(const Log& rhs) const { return !(*this < rhs); }
};

template <bool N>
inline Log<N> operator+(const Log<N>& lhs, const Log<N>& rhs) {
    Log<N> result = lhs;
    result += rhs;
    return result;
}

template <bool N>
inline Log<N> operator-(const Log<N>& lhs, const Log<N>& rhs) {
    Log<N> result = lhs;
    result -= rhs;
    return result;
}

template <bool N>
inline Log<N> operator*(const Log<N>& lhs, const Log<N>& rhs) {
    Log<N> result = lhs;
    result *= rhs;
    return result;
}

template <bool N>
inline Log<N> operator/(const Log<N>& lhs, const Log<N>& rhs) {
    Log<N> result = lhs;
    result /= rhs;
    return result;
}

template <bool N>
inline Log<N> pow(const Log<N> x, double y) { return Log<N>(x.log() * y, log_domain); }

template <bool N>
inline Log<N> exp(const Log<N> x) { return Log<N>((double) x, log_domain); }

template <bool N>
inline bool isnan(const Log<N> x) { return std::isnan(x.log()); }

// isinf returns 1 for infinity, -1 for -infinity
inline int isinf(const Log<false> x) { return x.log() == std::numeric_limits<double>::infinity() ? 1 : 0; }
inline int isinf(const Log<true> x) {
    if (x.log() == std::numeric_limits<double>::infinity())
        return x.is_negative() ? -1 : 1;
    return 0;
}

template <bool N>
inline size_t hash_value(const Log<N> &x) {
    size_t hash = 0;
    boost::hash_combine(hash, x.log());
    boost::hash_combine(hash, x.is_negative());
    return hash;
}


template <bool N>
inline std::ostream& operator<<(std::ostream& s, const Log<N> x) {
    if (x.log() == -std::numeric_limits<double>::infinity())
        s << "0";
    else if (x.log() < -575) {
        // < 10^-250
        // print only the log value
        if (x.is_negative())
            s << '-';
        s << "1e" << (long) round(x.log() / log(10.0));
    } else
        s << (double) x;
    return s;
}

template <bool N>
inline std::istream& operator>>(std::istream& s, Log<N> &x) {
    double tmp;
    s >> tmp;
    x = Log<N>(tmp);
    return s;
}

inline double log_add(double x, double y) {
    if (y > x)
        std::swap(x, y);
    if (y == -std::numeric_limits<double>::infinity())
        // x + 0 = x
        return x;
    return x + log1p(exp(y - x));
}

inline double log_subtract(double x, double y) {
    if (y > x)
        throw std::runtime_error("log_subtract: cannot use log with negative values");
    if (x == y)
        // result is zero
        return -std::numeric_limits<double>::infinity();
    if (y == -std::numeric_limits<double>::infinity())
        // subtracting zero does not change value
        return x;

    return x + log1p(-exp(y - x));
}

inline double log_mult(double x, double y) { return x + y; }

inline double log_divide(double x, double y) { return x - y; }


// Never use Kahan summation for LogProbabilities (they cannot represent negative values)
// LogProbabilities should represent sufficient detail to not require any other summation algorithm
template<>
class KahanSum<Log<false>> : public StandardSum<Log<false>> { };
