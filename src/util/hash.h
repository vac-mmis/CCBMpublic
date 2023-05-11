#pragma once

#include <memory>
#include <functional>
#include <type_traits>

#include <boost/functional/hash.hpp>

// C++11: use std::pointer_traits
template<class P>
struct pointer_traits {
    typedef typename P::element_type element_type;
};

template<class T>
struct pointer_traits<T*> {
    typedef T element_type;
};

// Utilities for use with hash classes such as unordered_map


// Generic Equal predicate for pointer (or pointer-like) types which operates on the base type
// template parameters:
// * T: the pointer type - raw pointers, shared_ptr, etc.
// * Pred (optional): the predicate function class, defaults to std::equal_to<X>, where X is the type underlying T
template<class T, class Pred = std::equal_to<typename pointer_traits<T>::element_type>>
class Pointer_Equal {
public:
    // we want to pass shared_ptr etc. by reference (avoid reference increase),
    // but want to pass raw pointers by value (for efficiency)
    typedef typename std::conditional<std::is_pointer<T>::value, T, const T&>::type first_argument_type;
    typedef first_argument_type second_argument_type;
    typedef bool result_type;

    bool operator()(first_argument_type a, second_argument_type b) const {
        Pred equal;
        return equal(*a, *b);
    }
};


// Generic hash function wrapper for pointer (or pointer-like) types which operates on the base type
// template parameters:
// * T: the pointer type - raw pointers, shared_ptr, etc.
// * Hash (optional): the hash function class, defaults to boost::hash<X>, where X is the type underlying T
template<class T, class Hash = boost::hash<typename pointer_traits<T>::element_type>>
class Pointer_Hash {
public:
    // we want to pass shared_ptr etc. by reference (avoid reference increase),
    // but want to pass raw pointers by value (for efficiency)
    typedef typename std::conditional<std::is_pointer<T>::value, T, const T&>::type argument_type;
    typedef size_t result_type;

    size_t operator()(argument_type p) const {
        Hash hasher;
        return hasher(*p);
    }
};


// hash-value for any unordered container, i.e. hash_value is independent of iteration order
// template parameters:
// * T: the container type
// * Hash (optional): the hash function class, defaults to boost::hash<T::value_type>
template <class T, class Hash = boost::hash<typename T::value_type>>
struct hash_unordered {
    size_t operator()(const T &x) const {
        boost::hash<typename T::value_type> hash_value;
        size_t hash = 0;
        for (const auto &value: x) {
            // XOR is commutative, thus the order in which we get the values does not change the result
            hash ^= hash_value(value);
        }
        return hash;
    }
};
