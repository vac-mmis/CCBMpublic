#pragma once

#include <iosfwd>
#include <type_traits>

#include <boost/utility.hpp>

// virtual basic sum structure template
template <typename T>
struct basic_sum {
    // sum() _needs_ to be declared as const for non-member operatorXY()
    // sum() should not chane the semantic value. Use mutable members if you need to
    // change non-semantic fields such as caches.
    virtual T sum() const = 0;

    // sum implementations have to define a way to add given values
    virtual void add(const T&) = 0;
    virtual void reset(const T& = 0) = 0;

    operator T () const { return sum(); }

    // mathematical operations use sum()
    basic_sum<T>& operator+=(const T &x) { add(x); return *this; }
    basic_sum<T>& operator-=(const T &x) { add(-x); return *this; }
    basic_sum<T>& operator*=(const T &x) { reset(sum() * x); return *this; }
    basic_sum<T>& operator/=(const T &x) { reset(sum() / x); return *this; }

    basic_sum<T>& operator=(const T &x) { reset(x); return *this; }

    bool operator==(const T &x) const { return sum() == x; }
    bool operator!=(const T &x) const { return sum() != x; }
    bool operator> (const T &x) const { return sum() > x; }
    bool operator>=(const T &x) const { return sum() >= x; }
    bool operator< (const T &x) const { return sum() < x; }
    bool operator<=(const T &x) const { return sum() <= x; }

    bool operator==(const basic_sum<T> &x) const { return sum() == x.sum(); }
    bool operator!=(const basic_sum<T> &x) const { return sum() != x.sum(); }
    bool operator> (const basic_sum<T> &x) const { return sum() > x.sum(); }
    bool operator>=(const basic_sum<T> &x) const { return sum() >= x.sum(); }
    bool operator< (const basic_sum<T> &x) const { return sum() < x.sum(); }
    bool operator<=(const basic_sum<T> &x) const { return sum() <= x.sum(); }
};

// Math operators
template <template <typename> class S, typename T,
    class = typename std::enable_if<std::is_base_of<basic_sum<T>, S<T>>::value>::type>
inline S<T> operator+(S<T> lhs, const S<T> &rhs) {
    lhs += rhs;
    return lhs;
}

template <template <typename> class S, typename T,
    class = typename std::enable_if<std::is_base_of<basic_sum<T>, S<T>>::value>::type>
inline S<T> operator+(S<T> lhs, const T &rhs) {
    lhs += rhs;
    return lhs;
}

template <template <typename> class S, typename T,
    class = typename std::enable_if<std::is_base_of<basic_sum<T>, S<T>>::value>::type>
inline S<T> operator+(const T &lhs, S<T> rhs) {
    rhs += lhs;
    return rhs;
}

template <template <typename> class S, typename T,
    class = typename std::enable_if<std::is_base_of<basic_sum<T>, S<T>>::value>::type>
inline S<T> operator-(S<T> lhs, const S<T> &rhs) {
    lhs -= rhs;
    return lhs;
}

template <template <typename> class S, typename T,
    class = typename std::enable_if<std::is_base_of<basic_sum<T>, S<T>>::value>::type>
inline S<T> operator-(S<T> lhs, const T &rhs) {
    lhs -= rhs;
    return lhs;
}

template <template <typename> class S, typename T,
    class = typename std::enable_if<std::is_base_of<basic_sum<T>, S<T>>::value>::type>
inline S<T> operator-(const T &lhs, S<T> rhs) {
    S<T> x(lhs);
    x -= rhs;
    return x;
}

template <template <typename> class S, typename T,
    class = typename std::enable_if<std::is_base_of<basic_sum<T>, S<T>>::value>::type>
inline S<T> operator*(S<T> lhs, const S<T> &rhs) {
    lhs *= rhs;
    return lhs;
}

template <template <typename> class S, typename T,
    class = typename std::enable_if<std::is_base_of<basic_sum<T>, S<T>>::value>::type>
inline S<T> operator*(S<T> lhs, const T &rhs) {
    lhs *= rhs;
    return lhs;
}

template <template <typename> class S, typename T,
    class = typename std::enable_if<std::is_base_of<basic_sum<T>, S<T>>::value>::type>
inline S<T> operator*(const T &lhs, S<T> rhs) {
    rhs *= lhs;
    return rhs;
}

template <template <typename> class S, typename T,
    class = typename std::enable_if<std::is_base_of<basic_sum<T>, S<T>>::value>::type>
inline S<T> operator/(S<T> lhs, const S<T> &rhs) {
    lhs /= rhs;
    return lhs;
}

template <template <typename> class S, typename T,
    class = typename std::enable_if<std::is_base_of<basic_sum<T>, S<T>>::value>::type>
inline S<T> operator/(S<T> lhs, const T &rhs) {
    lhs /= rhs;
    return lhs;
}

template <template <typename> class S, typename T,
    class = typename std::enable_if<std::is_base_of<basic_sum<T>, S<T>>::value>::type>
inline S<T> operator/(const T &lhs, S<T> rhs) {
    S<T> x(lhs);
    x /= rhs;
    return x;
}

// stream

template <template <typename> class S, typename T>
std::ostream& operator<<(std::ostream &os, const S<T> &s) {
    os << s.sum();
    return os;
}
