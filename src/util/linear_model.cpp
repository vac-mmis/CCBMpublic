#include <cassert>
#include <cmath>

#include "linear_model.h"

namespace linmodel {


//      class Line
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


void Line::add_point(double x, double y) {
    using std::isfinite;
    // x and y must be finite, otherwise all the values are infinite and model's error is infinite
    assert(isfinite(y) && isfinite(x) && "x and y must be finite");

    n++;
    sum_x += x;
    sum_y += y;
    sum_x2 += x*x;
    sum_xy += x*y;

    if (n == 1) {
        // for n == 1, the generic formula will set b = 0/0
        a = y;
        b = 0;
    } else {
        double sx = sum_x;
        double sy = sum_y;
        b = (sum_xy - sx * sy / n) / (sum_x2 - sx * sx / n);
        a = (sy - b * sx) / n;
    }

    // this may happen when adding identical x-values
    assert(isfinite(a) && isfinite(b) && "error in the computation of a or b");
}

void Line::recompute_y() {
    // the x-values are unchanged, but sum_y and sum_xy need to be updated
    // do this by inversion of the formulae for a and b
    double sx = sum_x;
    sum_y = a * n + b * sx;
    sum_xy = b * (sum_x2 - sx * sx / n) + sx * sum_y / n;
}

Line& Line::operator+=(double y) {
    // simply shift the intercept
    a += y;
    recompute_y();
    return *this;
}

Line& Line::operator-=(double y) {
    // simply shift the intercept
    a -= y;
    recompute_y();
    return *this;
}

Line& Line::operator*=(double f) {
    // f*y = f*a + f*b*x
    a *= f;
    b *= f;
    recompute_y();
    return *this;
}

Line& Line::operator/=(double f) {
    // y/f = a/f + b*x/f
    a /= f;
    b /= f;
    recompute_y();
    return *this;
}

void Line::merge(const Line &other) {
    if (other.n == 0)
        return;

    n += other.n;
    sum_x += other.sum_x;
    sum_y += other.sum_y;
    sum_x2 += other.sum_x2;
    sum_xy += other.sum_xy;

    if (n == 1) {
        // for n == 1, the generic formula will set b = 0/0
        a = other.a;
        b = 0;
    } else {
        double sx = sum_x;
        double sy = sum_y;
        b = (sum_xy - sx * sy / n) / (sum_x2 - sx * sx / n);
        a = (sy - b * sx) / n;
    }
}

Line& Line::operator+=(const Line &rhs) {
    assert(n == rhs.n && "The number of data-points of the lines must match exactly");

    // the expected sum x is identical for the intervals
    // E[sum x] = E[sum x1] = E[sum x2]
    // simply compute the mean to approximate E[sum x]
    sum_x = (sum_x + rhs.sum_x) / 2;
    // same for x^2
    sum_x2 = (sum_x2 + rhs.sum_x2) / 2;

    // the y values simply add, so do x*y:
    // E[sum x*y] = E[sum x*(y1+y2)] = E[sum x*y1] + E[sum x*y2]
    sum_y += rhs.sum_y;
    sum_xy += rhs.sum_xy;

    return *this;
}


//      class Segment_base
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


template <class S, typename T>
void Segment_base<S, T>::add_point(double x, T y) {
    if (x > end)
        end = x;
    if (x < start)
        start = x;
    self().line.add_point(x, y);
}

template <class S, typename T>
void Segment_base<S, T>::merge(const S &other) {
    if (other.start < start)
        start = other.start;
    if (other.end > end)
        end = other.end;
    self().line.merge(other.line);
}

template <class S, typename T>
void Segment_base<S, T>::resample(size_t n) {
    auto &l = self().line;
    l.n = n;
    // approximate the different sums, i.e. compute the "expected sums" E[sum ...] given n samples of x

    // E[sum of x] = sum of E[x] = n * "average value"
    // note that this is identical to (sum x from start to end) * n / (end - start + 1),
    //  where (sum x from start to end) = (sum 1..end - sum 1..(start-1)) = end*(end+1)/2 - start*(start-1)/2
    l.sum_x = l.n * (start + end) / 2;

    // E[sum_n of x^2] = sum_n (E[x^2]) = sum_n (sum_i i^2 * p(i))
    //      p(i) = 1/(end-start+1)
    //  = 1/(end-start+1) sum_n (sum (i^2))
    //      sum_{i=start}^end (i^2) = 1/6 (e-s+1) (2s^2 + 2e^2 + 2se + e - s)
    //  = sum_n 1/6 (2s^2 + 2e^2 + 2se + e - s)
    //  = n/6 (2s^2 + 2e^2 + 2se + e - s)
    l.sum_x2 = l.n / 6 * (2*start*start + 2*end*end + 2*start*end + end - start);

    // sum_y and sum_xy can be computed based on the regression formula
    l.recompute_y();
}

template <class S, typename T>
void Segment_base<S, T>::resize(double new_start, double new_end, size_t n) {
    if (start == new_start && end == new_end && (n == 0 || n == self().line.n))
        // nothing changes
        return;

    if (n == 0) {
        double length_old = end - start + 1;
        // scale proportionally to change in interval size
        n = (new_end - new_start + 1) / length_old * self().line.n;
        if (n < 1)
            // ensure we have at least one sample
            n = 1;
    }

    // update interval and recompute sum_* by "re-sampling" n points
    start = new_start;
    end = new_end;
    resample(n);
}



//      class Segment
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


Segment& Segment::operator+=(const Segment &rhs) {
    assert(start == rhs.start && end == rhs.end && "The intervals of the segments must match exactly");

    line += rhs.line;
    return *this;
}



//      class LogLine
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// compute a line model for log(y(x)) = log( y(x) * rhs(x) )
// this only works if the learned x values for both models are identically distributed
// (i.e. from a Segment of the same interval). Better use LogSegment<N>::operator*=.
template <bool N>
LogLine<N>& LogLine<N>::operator*=(const LogLine<N> &rhs) {
    // e^(a1 + b1*x) * e^(a2 + b2*x) = e^(a1+a2 + (b1+b2)*x)
    a += rhs.a;
    b += rhs.b;
    // the x-values stay the same
    recompute_y();
    return *this;
}


//      class LogSegment
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


template <bool N>
Log<N> LogSegment<N>::sum() const {
    // the line segment is a log-linear model - the weights are actually exponential

    if (line.n == 1)
        // if n=1, then we can simply return a (and probably the formula might return NaN)
        return Log<N>(line.a, log_domain);

    if (line.b == 0) {
        // if b=0, the weights are all constant and we multiply it with the segment length
        // in this case, it is not really a log-linear model, but this special case may occur
        // (esp. if multiplying to log-linear models exp(a1+b1*x) * exp(a2+b2*x) with b1 = -b2
        return Log<N>(line.a, log_domain) * Log<N>(this->end - this->start + 1);
    }

    // the sum of all exp(a + b * start:end) can be computed when viewing the distribution
    // as continuous, and computing the definite integral F [start-0.5, end+0.5].
    // F(x) = exp(a+b*x)/b
    // by doing some calculus, one can find that
    // sum(exp(a + b * start:end)) = 1/(exp(0.5*b) - exp(-0.5*b)) * (F(a+end+0.5) - F(a+start-0.5))
    // (in particular note that exp(b*x) = (F(b*(x+0.5)) - F(b*(x-0.5))) / (exp(0.5*b)-exp(-0.5*b)))

    Log<N> w1 = this->predict(this->start - 0.5);
    Log<N> w2 = this->predict(this->end + 0.5);

    Log<N> b2 = Log<N>(0.5 * line.b, log_domain);
    if (static_cast<double>(b2) == 1.0) {
        // b is nearly 0, and the line is nearly constant, so the formula below
        // would return infinity, because exp(0.5*b) - exp(-0.5*b) == 0
        // thus approximate it as being constant:
        // compute the mean of w1, w2 and multiply it with the length
        return (w1 + w2)/Log<N>(2) * Log<N>(this->end - this->start + 1);
    }

    // actually, the integral is (w2-w1)/b
    // devide by b is not necessary, as it would be multiplied by b at the end
    // however, if b<0 we need to compute -(w2-w1) = w1-w2 in case N=false
    Log<N> sum = line.b < 0 ? w1 - w2 : w2 - w1;

    // actually, the formula would be sum * b / (exp(0.5*b) - exp(-0.5*b))
    // however, we did not divide by b in sum, so we do not need it here either
    // if b < 0, we need to compute -(exp(0.5*b) - exp(-0.5*b))
    if (line.b < 0) {
        return sum / (Log<N>(-0.5 * line.b, log_domain) - b2);
    } else {
        return sum / (b2 - Log<N>(-0.5 * line.b, log_domain));
    }
}

template <bool N>
Log<N> LogSegment<N>::integral(double x1, double x2) const {
    // the line segment is a log-linear model - the weights are actually exponential

    // special case: the model is constant
    if (line.b == 0) {
        // integral = weight * length of integral
        Log<N> w = this->predict(x1);
        return w * Log<N>(x2-x1);
    }

    // indefinite integral F(x) = exp(a+b*x)/b

    // the definite integral is 1/b * (exp(a+b*x2) - exp(a+b*x1))
    Log<N> w1 = this->predict(x1);
    Log<N> w2 = this->predict(x2);
    // perform the computations completely in the log-domain
    if (line.b < 0)
        return (w1 - w2) / Log<N>(-line.b);
    else
        return (w2 - w1) / Log<N>(line.b);
}



//      explicit template instantiations
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

template class LogLine<false>;
template class LogLine<true>;
template class Segment_base<Segment, double>;
template class Segment_base<LogSegment<false>, Log<false>>;
template class Segment_base<LogSegment<true>, Log<true>>;
template class LogSegment<false>;
template class LogSegment<true>;

} // namespace linmodel
