#pragma once

#include <iosfwd>

#include <boost/functional/hash.hpp>
#include <boost/array.hpp>

namespace util {

template <typename T, size_t N>
class array : public boost::array<T, N> { };

// specialise for one element
// this has been implemented mainly for efficiency of operator==
// In one test, this reduced overall filter run time by 10% (-O2)
template <typename T>
class array<T, 1> {
public:
    T elem;

    // typedefs

    typedef T value_type;
    typedef T* iterator;
    typedef const T* const_iterator;
    typedef T& reference;
    typedef const T& const_reference;
    typedef std::size_t size_type;
    typedef std::ptrdiff_t difference_type;

    // iterator support

    iterator begin() { return &elem; }
    const_iterator begin() const { return &elem; }
    const_iterator cbegin() const { return &elem; }

    iterator end() { return (&elem)+1; }
    const_iterator end() const { return (&elem)+1; }
    const_iterator cend() const { return (&elem)+1; }

    // element access

    reference operator[] (size_type i) { return *((&elem) + i); }
    const_reference operator[] (size_type i) const { return *((&elem) + i); }

    reference front() { return elem; }
    const_reference front() const { return elem; }
    reference back() { return elem; }
    const_reference back() const { return elem; }

    // assign one value to all elements
    void assign (const T& value) { elem = value; }
    void fill   (const T& value) { elem = value; }

    // query

    static size_type size() { return 1; }
    static bool empty() { return false; }
    static size_type max_size() { return 1; }

    // comparison

    bool operator==(const array &rhs) const { return elem == rhs.elem; }
    bool operator!=(const array &rhs) const { return !(*this == rhs); }
    bool operator<(const array &rhs) const { return elem < rhs.elem; }
    bool operator>(const array &rhs) const { return rhs < *this; }
    bool operator<=(const array &rhs) const { return !(rhs < *this); }
    bool operator>=(const array &rhs) const { return !(*this < rhs); }

    // misc

    void swap(array &rhs) {
        using std::swap;
        swap(elem, rhs.elem);
    }

    T* data() { return &elem; }
    const T* data() const { return &elem; }
    T* c_array() { return &elem; }
};

template <typename T>
void swap(const array<T, 1> &a1, const array<T, 1> &a2) {
    a1.swap(a2);
}

template <typename T>
size_t hash_value(const array<T, 1> &arr) {
    boost::hash<T> hasher;
    return hasher(arr.elem);
}


// convenience stream output
template <typename T, size_t N>
std::ostream& operator<<(std::ostream &os, const array<T, N> &arr) {
    os << '[';
    for (size_t i = 0; i < N-1; i++)
        os << arr[i] << ", ";
    os << arr[N-1] << ']';
    return os;
}

} // namespace util
