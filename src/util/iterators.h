#pragma once

#include <iterator>
#include <memory>

#include <boost/range/iterator_range.hpp>
#include <boost/iterator/transform_iterator.hpp>

//      Forward declarations
// #############################################################################

// converts any iterator I to a const_iterator
template <typename I> class const_iterator;

// iterators to just a single value

template <typename T> class singleton_iterator;
template <typename T> class singleton_const_iterator;
template <typename T> class value_iterator;
template <typename T> class value_const_iterator;

// iterator to an infinite sequence of a constant number
template <typename T> class infinite_iterator;

template <class T, class F, class R>
boost::iterator_range<T> make_transform_iterator_range(R &range, F f = F());

//      Declarations
// #############################################################################



//      class const_iterator
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// converts any sufficiently well-behaved iterator to a const_iterator
// well-behaved means: iterator_traits<I>::pointer is iterator_traits<I>::value_type*, same for reference
template <typename I>
class const_iterator :
    public I,
    public std::iterator<typename std::iterator_traits<I>::iterator_category,
                         typename std::iterator_traits<I>::value_type,
                         const typename std::iterator_traits<I>::value_type*,
                         const typename std::iterator_traits<I>::value_type&>
{
public:
    // conversion constructor
    const_iterator(const I &it): I(it) { }

    const typename std::iterator_traits<I>::value_type & operator*() const { return I::operator*(); }
    const typename std::iterator_traits<I>::value_type * operator->() const { return I::operator->(); }

    bool operator==(const const_iterator &other) const { return static_cast<I>(*this) == static_cast<I>(other); }
    bool operator!=(const const_iterator &other) const { return !(*this == other); }
}; // class const_iterator




//      class singleton_iterator
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// An iterator over a single value (this iterator keeps a pointer to the value)
// It is a forward iterator (changing the local value is allowed)
template <typename T>
class singleton_iterator : public std::iterator<std::forward_iterator_tag, T, ptrdiff_t> {
    // an empty pointer indicates this iterator is at the end (i.e. the single value has been read)
    T *value_ptr;

public:

    singleton_iterator() { }
    singleton_iterator(T &value): value_ptr(&value) { }

    T& operator*() const { return *value_ptr; }
    T* operator->() const { return value_ptr; }

    // pre-increment
    singleton_iterator& operator++() {
        // increment this iterator always makes it point to the end
        value_ptr = NULL;
        return *this;
    }

    // post-increment
    singleton_iterator operator++(int) {
        singleton_iterator copy(*this);
        ++*this;
        return copy;
    }

    bool operator==(const singleton_iterator &other) const { return value_ptr == other.value_ptr; }
    bool operator!=(const singleton_iterator &other) const { return !(*this == other); }
}; // class singleton_iterator



//      class singleton_const_iterator
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// A const iterator over a single value (this iterator keeps a pointer to the value)
// It is a constant forward iterator (changing the local value is *not* allowed)
template <typename T>
class singleton_const_iterator : public const_iterator<singleton_iterator<T> > { };



//      class value_iterator
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// An iterator over a single temporary value (this iterator keeps a pointer to a copy of the value itself)
// It is a forward iterator (changing the local value is allowed)
template <typename T>
class value_iterator : public std::iterator<std::forward_iterator_tag, T, ptrdiff_t> {
    // we must keep a shared_ptr to the value, because a copy of this iterator should point to the same value
    // an empty pointer indicates this iterator is at the end (i.e. the single value has been read)
    std::shared_ptr<T> value_ptr;

public:

    value_iterator() { }
    // create a copy
    value_iterator(const T &value): value_ptr(std::make_shared<T>(value)){ }

    T& operator*() const { return *value_ptr; }
    T* operator->() const { return value_ptr.get(); }

    // pre-increment
    value_iterator& operator++() {
        // increment this iterator always makes it point to the end
        value_ptr.reset();
        return *this;
    }

    // post-increment
    value_iterator operator++(int) {
        value_iterator copy(*this);
        ++*this;
        return copy;
    }

    bool operator==(const value_iterator &other) const { return value_ptr == other.value_ptr; }
    bool operator!=(const value_iterator &other) const { return !(*this == other); }
}; // class value_iterator



//      class value_const_iterator
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// An const iterator over a single temporary value (this iterator keeps the value itself, thus it may be inefficient for large types, but efficient for smaller, because no heap allocations / pointers are involved)
// It is constant bidirectional iterator (changing the local value is *not* allowed)
template <typename T>
class value_const_iterator : public std::iterator<std::bidirectional_iterator_tag, T, ptrdiff_t, T const *, T const &> {
    bool atEnd;
    // we must keep a copy of the value, because the value might be just a temporary
    T value;

public:

    value_const_iterator() : atEnd(true) { }
    value_const_iterator(const T &value):
        atEnd(false),
        value(value) // create a copy
    { }

    T const & operator*() const { return value; }
    T const * operator->() const { return &value; }

    // pre-increment
    value_const_iterator& operator++() {
        // increment this iterator always makes it point to the end
        atEnd = true;
        return *this;
    }

    // pre-decrement
    value_const_iterator& operator--() {
        // decrement this iterator always makes it point to the value itself
        atEnd = false;
        return *this;
    }

    // post-increment
    value_const_iterator operator++(int) {
        value_const_iterator copy(*this);
        ++*this;
        return copy;
    }

    // post-decrement
    value_const_iterator operator--(int) {
        value_const_iterator copy(*this);
        --*this;
        return copy;
    }

    bool operator==(const value_const_iterator &other) const {
        // if both iterator are at the end, they must be considered identical
        // (an empty value_ptr and atEnd=true must always represent the end - this may be returned by calls to end())
        if (atEnd && other.atEnd)
            return true;
        return atEnd == other.atEnd && value == other.value;
    }

    bool operator!=(const value_const_iterator &other) const { return !(*this == other); }
}; // class value_const_iterator



//      class infinite_iterator
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


template <typename T>
class infinite_iterator : public std::iterator<std::bidirectional_iterator_tag, T, ptrdiff_t, T const *, T const &> {
    T value;

public:
    // create a copy of the value
    infinite_iterator(const T &value): value(value) { }

    T const & operator*() const { return value; }
    T const * operator->() const { return &value; }

    // pre-increment / pre-decrement
    infinite_iterator& operator++() { return *this; }
    infinite_iterator& operator--() { return *this; }

    // post-increment / post-decrement
    infinite_iterator& operator++(int) { return *this; }
    infinite_iterator& operator--(int) { return *this; }

    bool operator==(const infinite_iterator &other) const { return value == other.value; }
    bool operator!=(const infinite_iterator &other) const { return !(*this == other); }
}; // class infinite_iterator


//      make_transform_iterator_range
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

// transforms a boost::iterator_range into another boost::iterator_range by using boost:transform_iterator
// T: the transform iterator type (required)
// F: the transform function type (optional if f is given)
// R: the input range type (optional and always deduced)
//
// If template agrument F is given, f does not need to be specified - it will be default-constructed
// If argument f is given, F does not need to be specified - it will be automatically deduced
template <class T, class F, class R>
boost::iterator_range<T> make_transform_iterator_range(R &range, F f) {
    return boost::make_iterator_range(T(range.begin(), f), T(range.end(), f));
}
