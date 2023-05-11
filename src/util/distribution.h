#pragma once

#include "iterators.h"

#include "../weight.h"

namespace distribution {

// basic classes representing empirical distributions and estimates
// The estimates associate some values with probabilities
// (a distribution is a full estimate over all possible values that integrates to 1)
// estimates / distributions are represented by a range, i.e. a pair of iterator [begin, end)

// An estimate over a singleton value
template <class T>
struct single_estimate : boost::iterator_range<value_const_iterator<std::pair<T, Weight> > > {
    typedef boost::iterator_range<value_const_iterator<std::pair<T, Weight> > > range;
    typedef typename range::iterator iterator;
    typedef typename range::const_iterator const_iterator;

    // we cannot default-construct T in all case (e.g. when it is a reference, like ::State<Model>&)
    // therefore, we cannot default-construct iterator() to have an end-iterator
    // instead, we make a copy of the singleton_iterator and increment that, which is also the end
    static range make_range(iterator it) {
        iterator end = it;
        ++end;
        return range(it, end);
    }

    single_estimate(const T &value, Weight w) :
        range(make_range(iterator(std::pair<T, Weight>(value, w))))
    { }
}; // struct single_estimate


} // namespace distribution
