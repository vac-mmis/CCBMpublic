#pragma once

#include <cassert>
#include <cmath>
#include <limits>

#include "summation.h"
#include "log.h"

namespace linmodel {

//      class Line
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// a single linear model y = a + b*x
// this only works with plain double values
class Line {
public:
    size_t n; // the number of data-points
    // several statistics of the data-points
    Sum<double> sum_x;
    Sum<double> sum_y;
    Sum<double> sum_x2; // x^2
    Sum<double> sum_xy; // x*y

    // the actual parameters
    double a;
    double b;

    // an "empty" line
    Line() :
        n(0),
        sum_x(0),
        sum_y(0),
        sum_x2(0),
        sum_xy(0)
    { }

    // a line approximating a single point
    Line(double x, double y) :
        n(1),
        sum_x(x), sum_y(y), sum_x2(x*x), sum_xy(x*y),
        a(y), b(0)
    {
        assert(std::isfinite(y) && std::isfinite(x) && "x and y must be finite");
    }

    // add a new point (x, y) and update the model
    void add_point(double x, double y);

    // merge another linear model into this model
    void merge(const Line &other);

    // predict a value at a specific point x
    double predict(double x) const { return a + b * x; }

    // recompute the y, x*y values based on a, b, n, x, x^2
    // has to be called after changing a and b to model changes on the y values (where the x-values are fixed)
    void recompute_y();

    // compute a line model for y(x) = y(x) + rhs(x)
    // this only works if the learned x values for both models are identically distributed
    // (i.e. from a Segment of the same interval). Better use Segment::operator+=.
    Line& operator+=(const Line &rhs);

    // shift the model by a constant y-offset
    Line& operator+=(double y);

    // shift the model by a constant y-offset
    Line& operator-=(double y);

    // scale the model by a constant factor f
    Line& operator*=(double f);

    // divide the model by a constant factor f
    Line& operator/=(double f);

}; // class Line


//      class Segment_base
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

// CRTP for implementing common methods shared by Segment and LogSegment
// we prefer CRTP over virtual methods to guarantee that no virtual call occurs.
// template parameters:
// S: the Segment class
// T: the value class of the model
template <class S, typename T>
class Segment_base {
    const S& self() const { return static_cast<const S&>(*this); }
    S& self() { return static_cast<S&>(*this); }
public:
    // The start-time and end-time (both inclusive) of this line segment
    double start;
    double end;

    // an empty segment
    Segment_base() :
        start(std::numeric_limits<double>::infinity()),
        end(-std::numeric_limits<double>::infinity())
    { }

    // a segment with a single time-point
    Segment_base(double x) :
        start(x), end(x)
    { }

    void add_point(double x, T y);

    void merge(const S &other);

    T predict(double x) const { return self().line.predict(x); }

    // re-compute the line-segment's statistical variables sum_* as-if the model was learned by n
    // data-points. This compute these variables *as-if* n samples were drawn from that model.
    // a and b are unchanged (the shape of the line itself does not change)
    // the sum_* variables are re-computed to fit to the new interval
    // * the x values are taken uniformly from the interval (this is the only loss of information)
    // * the y and x*y values are computed by re-formulating the simple regression formulas and using a, b, and the x-values
    void resample(size_t n);

    // set a new [start, end] interval for this line segment, keeping as much statistical information about the data points as possible
    // this is mostly used to shrink this segment to a smaller interval
    // a and b are unchanged (the shape of the line itself does not change), the sum_* variables are re-computed
    // using resample(n).
    // If n is zero or not passed as argument, it grows/shrinks proportionally to the change of size of the interval
    void resize(double start, double end, size_t n = 0);

    // scale the model by a constant y factor
    S& operator*=(T y) { self().line *= y; return self(); }

    // divide the model by a constant y factor
    S& operator/=(T y) { self().line /= y; return self(); }
}; // class Segment_base



//      class Segment
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// a Line with a start-point and end-point
class Segment: public Segment_base<Segment, double> {
    typedef Segment_base<Segment, double> Base;
public:
    // the underlying line
    Line line;

    Segment() : Segment_base()
    { }

    Segment(double x, double y) : Segment_base(x),
        line(x, y)
    { }

    // returns the sum of all discrete predicted y-values in the interval [start, end]
    double sum() const { return (predict(start) + predict(end))/2 * (end - start + 1); }

    // returns the definite integral of the model's y-values in the interval [x1, x2]
    // in contrast to sum, this works on the complete continuous interval, not just the discrete integer values
    double integral(double x1, double x2) const { return (x2 - x1) * (2*line.a + line.b * (x1 + x2)) / 2; }

    // add the other line segment to this, such that y(x) = y(x) + rhs(x)
    // this only works if the intervals and number of points match exactly
    // (otherwise, call resize or resample first)
    Segment& operator+=(const Segment &rhs);

}; // class Segment


//      class LogLine
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// a log-linear model log(y) = a + b*x
// the y values are given and returned as Log<N> values, internally a linear model of log(y) is fitted
// Adding zero y values results in undefined behaviour
// template parameter:
// * N: forwarded to Log<N>, set to yes if you want to use negative values
template <bool N>
class LogLine: protected Line {
    // we use a simple linear model as the underlying model
    // we inherit only protected so
    // * we can re-use the methods
    // * have the fields publicly available by using "using"
    // * but hide the shift() method to replace it with scale
public:
    using Line::n;
    using Line::sum_x;
    using Line::sum_y;
    using Line::sum_x2;
    using Line::sum_xy;
    using Line::a;
    using Line::b;

    // an "empty" line
    LogLine() : Line()
    { }

    // a line approximating a single point
    LogLine(double x, Log<N> y) : Line(x, y.log())
    { }

    // add a new point (x, y) and update the model
    void add_point(double x, Log<N> y) { assert(y != Log<N>(0) && "LogLine: zero value"); Line::add_point(x, y.log()); }

    // merge another linear model into this model
    void merge(const LogLine &other) { Line::merge(static_cast<Line>(other)); }

    // predict a value at a specific point x
    Log<N> predict(double x) const { return Log<N>(a + b * x, log_domain); }

    using Line::recompute_y;

    // compute a line model for log(y(x)) = log( y(x) + rhs(x) )
    // this only works if the learned x values for both models are identically distributed
    // (i.e. from a Segment of the same interval). Better use LogSegment<N>::operator+=.
    // XXX this does not seem to be easy
    // we may approximate sum_y = sum log(y) = sum log(y1(x) + y2(x)) by an integral,
    // however wolframalpha cannot compute the required integral for x * log(y1(x) + y2(x)
    // LogLine& operator+=(const LogLine &rhs);

    // compute a line model for log(y(x)) = log( y(x) * rhs(x) )
    // this only works if the learned x values for both models are identically distributed
    // (i.e. from a Segment of the same interval). Better use LogSegment<N>::operator*=.
    LogLine& operator*=(const LogLine &rhs);

    // scale the model by a constant factor f
    LogLine& operator*=(Log<N> f) { Line::operator+=(f.log()); return *this; }

    // scale the model by a constant factor f
    LogLine& operator/=(Log<N> f) { Line::operator-=(f.log()); return *this; }
}; // class LogLine


//      class LogSegment
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// a LogLine with a start-point and end-point
template <bool N>
class LogSegment: public Segment_base<LogSegment<N>, Log<N>> {
    typedef Segment_base<LogSegment<N>, Log<N>> Base;
public:
    // the underlying line
    LogLine<N> line;

    LogSegment() : Base()
    { }

    LogSegment(double x, Log<N> y) : Base(x),
        line(x, y)
    { }

    // returns the sum of all discrete predicted y-values in the interval [start, end]
    Log<N> sum() const;

    // returns the definite integral of the model's y-values in the interval [start, end]
    // in contrast to sum, this works on the complete continuous interval, not just the discrete integer values
    Log<N> integral(double x1, double x2) const;

    using Base::operator*=;
    using Base::operator/=;

    // compute a line model for log(y(x)) = log( y(x) * rhs(x) )
    LogSegment<N>& operator*=(const LogSegment<N> &rhs) { line *= rhs.line; return *this; }
}; // class LogSegment



//      external explicit template instantiations
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extern template class LogLine<false>;
extern template class LogLine<true>;
extern template class Segment_base<Segment, double>;
extern template class Segment_base<LogSegment<false>, Log<false>>;
extern template class Segment_base<LogSegment<true>, Log<true>>;
extern template class LogSegment<false>;
extern template class LogSegment<true>;

} // namespace linmodel
