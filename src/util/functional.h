#pragma once

namespace util {

// creates a callable object that does nothing

template <typename P1 = void, typename P2 = void, typename P3 = void>
struct empty_function {
    void operator() (P1, P2, P3) { }
};

template <typename P1, typename P2>
struct empty_function<P1, P2, void> {
    void operator() (P1, P2) { }
};

template <typename P1>
struct empty_function<P1, void, void> {
    void operator() (P1) { }
};

template <>
struct empty_function<void, void, void> {
    void operator() () { }
};


// creates a callable object that returns a constant

template <typename R, typename P1 = void, typename P2 = void, typename P3 = void>
struct constant_function {
    R r;

    constant_function(const R &r) :
        r(r)
    {}

    R operator() (P1, P2, P3) { return r; }
};

template <typename R, typename P1, typename P2>
struct constant_function<R, P1, P2, void> {
    R r;

    constant_function(const R &r) :
        r(r)
    {}

    R operator() (P1, P2) { return r; }
};

template <typename R, typename P1>
struct constant_function<R, P1, void, void> {
    R r;

    constant_function(const R &r) :
        r(r)
    {}

    R operator() (P1) { return r; }
};

template <typename R>
struct constant_function<R, void, void, void> {
    R r;

    constant_function(const R &r) :
        r(r)
    {}

    R operator() () { return r; }
};

} // namespace util
