#pragma once

#include <boost/functional/hash.hpp>

//      Forward declarations
// #############################################################################

template <class Map>
class weight_map;


//      Declarations
// #############################################################################



//  class map_statistics
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

namespace weight_map_detail {

// map_statistics tracks at least the total weight
// optionally, it also tracks the max/min Weight
template <typename T, bool enable>
class map_statistics {
private:
    Sum<T> totalWeight;
    // to provide efficient removal/updates, we also track removed weights
    Sum<T> removedWeight;
public:
    map_statistics() :
        totalWeight(T(0)),
        removedWeight(T(0))
    { }

    void clear() { totalWeight = T(0); removedWeight = T(0); }
    void add(T weight) { updateBounds(weight); addTotal(weight); }
    void remove(T weight) { removedWeight += weight; }
    void updateBounds(T) { }
    void addTotal(T weight) { totalWeight += weight; }

    void scale(T factor) {
        totalWeight = totalWeight.sum() * factor;
        removedWeight = removedWeight.sum() * factor;
    }

    T getMaxWeight() const { return T(0); }
    T getMinWeight() const { return T(1); }

    T getTotalWeight() const {
        if (removedWeight > totalWeight)
            return T(0);
        return totalWeight.sum() - removedWeight.sum();
    }
}; // class map_statistics<T, false>


template <typename T>
class map_statistics<T, true> : public map_statistics<T, false> {
private:
    typedef map_statistics<T, false> base;
    // the current maximum and minimum weight, used to limit the states represented
    T maxWeight, minWeight;
public:
    map_statistics() :
        base(),
        maxWeight(T(0)),
        minWeight(T(1))
    { }

    void clear() {
        base::clear();
        maxWeight = T(0);
        minWeight = T(1);
    }

    void add(T weight) { updateBounds(weight); base::addTotal(weight); }

    void updateBounds(T weight) {
        if (weight > maxWeight)
            maxWeight = weight;
        if (weight < minWeight)
            minWeight = weight;
    }

    void scale(T factor) {
        base::scale(factor);
        maxWeight *= factor;
        minWeight *= factor;
    }

    T getMaxWeight() const { return maxWeight; }
    T getMinWeight() const { return minWeight; }
}; // class map_statistics<T, true>

} // namespace weight_map_detail



//      class weight_map
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



// This class encapsulates the association <model substate> → weight, e.g. <S, A> → weight
// To limit the number of represented states to a reasonable limit, this map keeps only states which weight is at least WEIGHT_MAX_THRESH of the maximum weight
// In addition to mere storing and retrieving mappings, this map can also be used to do calculations (operator+, operator*, …) on the individual elements
//
// All iterators returned by this map are const iterators, because the map tracks internal statistics (e.g. the total weight) of all its elements
// If you want to modify an element, use the update method
//
// weight_map is parameterised by two constructor parameters:
// * max_thresh: any value smaller than (<max value> / max_thresh) will not be inserted into this map. Set this to 0 to always keep all states
// * min_thresh: if <min value> is smaller than (<max_value> / min_thresh), remove all weights that violate max_thresh
//
// Map: the underlying map to use
template <class Map>
class weight_map {
protected:
    typedef typename Map::iterator map_iterator;
    typedef typename Map::const_iterator map_const_iterator;

public:
    typedef typename Map::key_type key_type;
    typedef typename Map::mapped_type mapped_type;
    typedef typename Map::value_type value_type;

    typedef typename Map::const_iterator iterator; // all iterators returned are constant, because we need to track the map_statistics
    typedef typename Map::const_iterator const_iterator;

protected:
    typedef key_type Key;
    typedef mapped_type T;

    // the map from State -> T and a "stats" field for optional statistics
    Map weights;

    int max_thresh;
    int min_thresh;

    weight_map_detail::map_statistics<T, true> stats;

public:

    // constructor
    weight_map(int max_thresh = 100, int min_thresh = 1000) :
        max_thresh(max_thresh),
        min_thresh(min_thresh)
    { }

    // iterators and querying properties

    size_t size() const { return weights.size(); }
    bool empty() const { return weights.empty(); }
    T getTotalWeight() const { return stats.getTotalWeight(); }

    iterator begin() const { return weights.cbegin(); }
    iterator end() const { return weights.cend(); }
    const_iterator cbegin() const { return weights.cbegin(); }
    const_iterator cend() const { return weights.cend(); }


    // Modifiers

    // add the weight to the specified state
    void insert(Key state, T weight);

    void clear() {
        weights.clear();
        stats.clear();
    }

    // re-computes the total weight and is therefore slow
    iterator erase(const_iterator position);

    void update (Key state, T newWeight);

    void removeSmall();

    // element lookup

    const_iterator find(const Key& state) const { return weights.find(state); }
    size_t count(const Key& state) const { return weights.count(state); }


    // arithmetic operations to combine two weight_maps element-wise
    // any missing element is assumed to be 0

    // Multiply all weights by (1-factor), for each factor starting from begin. Zero weights are removed.
    // begin must be some forward iterator of Weights, and there must be at least elements as in this map
    template <class I>
    void scaleWeights1m(const I& begin);

    // Add two weight_maps element-wise
    // the resulting keys are the union of both keys-sets (missing values are treated as 0)
    weight_map& operator+=(const weight_map<Map>& other);

    // Multiply two weight_maps element-wise
    // the resulting keys are the intersection of both keys-sets (missing values are treated as 0)
    weight_map& operator*=(const weight_map<Map>& other);

    // Dividy two weight_maps element-wise
    // if any key cannot by found in the other weight_map, a std::range_error indicating division by zero is thrown
    // Otherwise, the resulting key-set is this weight_map's key-set
    weight_map& operator/=(const weight_map<Map>& other);

    // multiply with single scalar factor
    weight_map& operator*=(const T& factor);

    // dividy by single scalar factor
    weight_map& operator/=(const T& factor);

    // Multiply all weights by factor, for each factor starting from begin.
    // begin must be some forward iterator of Weights, and there must be at least elements as in this map
    template <class I>
    weight_map& operator*=(const I& begin);

    // Dividy all weights by factor, for each factor starting from begin.
    // begin must be some forward iterator of Weights, and there must be at least elements as in this map
    template <class I>
    weight_map& operator/=(const I& begin);


    // Anything else

    bool operator==(const weight_map &other) const { return weights == other.weights; }
    bool operator!=(const weight_map &other) const { return !(*this == other); }
    size_t hash_value() const;

    void swap(weight_map &other) { std::swap(weights, other.weights); }

    void print() const
    // this is a function used for debugging
    // thus prevent this function from being inlined or removed
    __attribute__((noinline,used));
}; // class weight_map



//      Definitions
// #############################################################################



//  class weight_map
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// add the weight to the specified state
template <class Map>
void weight_map<Map>::insert(Key state, T weight) {
    if (weight == T(0))
        return;

    map_iterator st = weights.find(state);
    if (st == weights.end()) {
        // state not present
        // add, if this has at least some weight
        if (max_thresh <= 0 || weight * T(max_thresh) >= stats.getMaxWeight()) {
            weights.insert(std::make_pair(state, weight));
            stats.add(weight);
        }
    } else {
        // the key already existed, add the weight to it
        st->second += weight;
        stats.add(st->second);
    }

    removeSmall();
}


template <class Map>
typename weight_map<Map>::iterator weight_map<Map>::erase(const_iterator position) {
    stats.remove(position->second);
    iterator result = weights.erase(position);
    return result;
}


template <class Map>
void weight_map<Map>::update(Key key, T newWeight) {
    map_iterator it = weights.find(key);
    stats.remove(it->second);
    it->second = newWeight;
    stats.add(newWeight);
}


template <class Map>
void weight_map<Map>::removeSmall() {
    // Only iterate of the minWeight is very small, compared to the max weight
    if (max_thresh <= 0 || stats.getMinWeight() * T(min_thresh) > stats.getMaxWeight())
        return;

    T maxWeight = stats.getMaxWeight();
    stats.clear();

    for (map_iterator it = weights.begin(); it != weights.end(); /* iterator increased at end of loop */) {
        if (it->second * T(max_thresh) < maxWeight)
            it = weights.erase(it); // does not invalidate iterators
        else {
            stats.add(it->second);
            ++it;
        }
    }
}



//      arithmetic operators
// ---------------------------------------------------------


// Multiply all weights by (1-factor), for each factor starting from begin. Zero weights are removed.
// begin must be some forward iterator of Weights, and there must be at least elements as in this map
template <class Map>
template <typename I>
void weight_map<Map>::scaleWeights1m(const I& begin) {
    stats.clear();

    map_iterator stIt = weights.begin();
    I fIt = begin;
    // update all weights with 1 - factor
    while (stIt != weights.end()) {
        stIt->second *= 1 - *fIt;
        if (*fIt == T(1) || (max_thresh > 0 && stIt->second * T(max_thresh) < stats.getMaxWeight()))
            // 1 - factor = 0 or the weight is much smaller than the maxWeight
            // C++14 guarantees the iteration order is preseserved after erase - we rely on this for iterating over the factors in parallel
            stIt = weights.erase(stIt);
        else {
            stats.add(stIt->second);
            ++stIt;
        }
        ++fIt;
    }

    removeSmall();
}


template <class Map>
weight_map<Map>& weight_map<Map>::operator+=(const weight_map<Map> &other) {
    // iterate over weights of the other map
    // if weight is in this map: update weight this += other
    // if weight is not in this map: add other weight (0 + other = other)
    for (map_const_iterator it = other.weights.cbegin(); it != other.weights.cend(); ++it) {
        map_iterator st = weights.find(it->first);
        if (st == weights.end()) {
            // state not present in this map, add it as-is
            // but add only if this has at least some weight
            if (max_thresh <= 0 || it->second * T(max_thresh) >= stats.getMaxWeight()) {
                weights.insert(*it);
                stats.updateBounds(it->second);
            }
        } else {
            // the key already existed, add the weight to it
            st->second += it->second;
            stats.updateBounds(it->second);
        }
    }

    // update totalWeight: we added (almost) all the weight to the existing particle
    // the lost weight is probably negligable
    stats.addTotal(other.getTotalWeight());

    removeSmall();

    return *this;
}


template <class Map>
weight_map<Map>& weight_map<Map>::operator*=(const weight_map<Map> &other) {
    stats.clear();

    // iterator over weights in this map
    // if weight is in the other map: update weight this *= other
    // if weight is not in the other map: remove this weight (x * 0 = 0)
    for (map_iterator it = weights.begin(); it != weights.end(); /* iterator increased at end of loop*/) {
        map_const_iterator st = other.weights.find(it->first);
        if (st == other.weights.end()) {
            // state not present in the other map, remove this weight
            it = weights.erase(it);
        } else {
            // both weights exist, multiply
            it->second *= st->second;
            stats.add(it->second);
            ++it;
        }
    }

    removeSmall();

    return *this;
}


template <class Map>
weight_map<Map>& weight_map<Map>::operator/=(const weight_map<Map> &other) {
    stats.clear();

    // iterator over weights in this map
    // if weight is in the other map: update weight this /= other
    // if weight is not in the other map: division by zero
    for (map_iterator it = weights.begin(); it != weights.end(); ++it) {
        map_const_iterator st = other.weights.find(it->first);
        if (st == other.weights.end()) {
            // state not present in the other map
            throw std::overflow_error("weight_map::operator/=: division by zero");
        } else {
            // both weights exist, divide
            it->second /= st->second;
            stats.add(it->second);
        }
    }

    removeSmall();

    return *this;
}


// Multiply all weights by factor, zero weights are removed
template <class Map>
weight_map<Map>& weight_map<Map>::operator*=(const T &factor) {
    if (factor == T(0)) {
        clear();
        return *this;
    }

    for (map_iterator it = weights.begin(); it != weights.end(); /* iterator increased at end of loop */) {
        it->second *= factor;
        if (it->second == T(0))
            it = weights.erase(it); // does not invalidate iterators
        else
            ++it;
    }

    // we can simply scale the statistics without adding the individual weights
    stats.scale(factor);

    return *this;
}


// Divide all weights by factor
template <class Map>
weight_map<Map>& weight_map<Map>::operator/=(const T &factor) {
    if (factor == T(1)) {
        return *this;
    }

    for (map_iterator it = weights.begin(); it != weights.end(); ++it) {
        it->second /= factor;
    }

    // we can simply scale the statistics without adding the individual weights
    stats.scale(T(1)/factor);

    return *this;
}


// Multiply all weights by factor, for each factor starting from begin. Zero weights are removed.
// begin must be some forward iterator of Weights, and there must be at least elements as in this map
template <class Map>
template <typename I>
weight_map<Map>& weight_map<Map>::operator*=(const I &begin) {
    stats.clear();

    map_iterator stIt = weights.begin();
    I fIt = begin;
    while (stIt != weights.end()) {
        if (*fIt == T(0) || (max_thresh > 0 && stIt->second * *fIt * T(max_thresh) < stats.getMaxWeight()))
            // the weight is zero or very small
            // C++14 guarantees the iteration order is preseserved after erase - we rely on this for iterating over the factors in parallel
            stIt = weights.erase(stIt);
        else {
            stIt->second *= *fIt;
            stats.add(stIt->second);
            ++stIt;
        }
        ++fIt;
    }

    removeSmall();
    return *this;
}


// Divide all weights by factor, for each factor starting from begin
// begin must be some forward iterator of Weights, and there must be at least elements as in this map
template <class Map>
template <typename I>
weight_map<Map>& weight_map<Map>::operator/=(const I &begin) {
    stats.clear();

    map_iterator stIt = weights.begin();
    I fIt = begin;
    while (stIt != weights.end()) {
        if (max_thresh > 0 && stIt->second / *fIt * T(max_thresh) < stats.getMaxWeight())
            // the weight is zero or very small
            // C++14 guarantees the iteration order is preseserved after erase - we rely on this for iterating over the factors in parallel
            stIt = weights.erase(stIt);
        else {
            stIt->second /= *fIt;
            stats.add(stIt->second);
            ++stIt;
        }
        ++fIt;
    }

    removeSmall();
    return *this;
}



//      Misc
// ---------------------------------------------------------


template <class Map>
void weight_map<Map>::print() const {
    std::cerr << "weight_map of size " << size() << ": [";
    for (const_iterator it = weights.cbegin(); it != weights.cend(); ++it) {
        std::cerr << it->first << " → " << it->second << ", ";
    }
    std::cerr << "]\n";
}


template <class Map>
size_t weight_map<Map>::hash_value() const {
    // hash only the unordered_map, not the statistics
    return boost::hash_range(weights.cbegin(), weights.cend());
}



//      Free functions
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



template <class Map>
void swap(weight_map<Map> &a, weight_map<Map> &b) {
    a.swap(b);
}

template <class Map>
weight_map<Map> operator+(weight_map<Map> a, const weight_map<Map> &b) {
    a += b;
    return a;
}

template <class Map>
weight_map<Map> operator*(weight_map<Map> a, const weight_map<Map> &b) {
    a *= b;
    return a;
}

template <class Map>
weight_map<Map> operator/(weight_map<Map> a, const weight_map<Map> &b) {
    a /= b;
    return a;
}

// scale by factor
template <class Map>
weight_map<Map> operator*(weight_map<Map> a, const typename weight_map<Map>::mapped_type &b) {
    a *= b;
    return a;
}

// scale by factor
template <class Map>
weight_map<Map> operator*(const typename weight_map<Map>::mapped_type &a, weight_map<Map> b) {
    b *= a;
    return b;
}

// scale by iterator
template <class Map, class I>
weight_map<Map> operator*(weight_map<Map> a, const I &begin) {
    a *= begin;
    return a;
}

// scale by iterator
template <class Map, class I>
weight_map<Map> operator*(const I &begin, weight_map<Map> b) {
    b *= begin;
    return b;
}

// divide by factor
template <class Map>
weight_map<Map> operator/(weight_map<Map> a, const typename weight_map<Map>::mapped_type &b) {
    a /= b;
    return a;
}

// divide by iterator
template <class Map, class I>
weight_map<Map> operator/(weight_map<Map> a, const I &begin) {
    a /= begin;
    return a;
}

template <class Map>
size_t hash_value(const weight_map<Map> &weightMap) {
    return weightMap.hash_value();
}
