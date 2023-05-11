#pragma once

#include <utility>
#include <functional>
#include <deque>

#include "allocator.h"

namespace util {

// A set implementation that can be used when only equality can be defined
// This implementation is based on a list and has therefore mostly very bad performance
// Thus, consider using unordered_set if you can additionally define a hash function,
// or use std::set if you can define a comparison operator
template <class T, class Pred = std::equal_to<T>, class Alloc = std::allocator<T> >
class list_set {
    typedef std::deque<T, Alloc> list_t;
    list_t list;

public: // typedef

    typedef T key_type;
    typedef T value_type;
    typedef T & reference;
    typedef const T & const_reference;
    typedef T * pointer;
    typedef const T * const_pointer;
    typedef Pred key_equal;

    // set iterators are always const_iterators
    // however, in order to have erase() work we need the corresponding non-const list_t::iterator
    class iterator : public list_t::const_iterator {
        friend class list_set;
        typename list_t::iterator it;
    public:
        iterator(typename list_t::iterator it) :
            list_t::const_iterator(it),
            it(it)
        { }
        // pre-increment
        iterator& operator++() {
            list_t::const_iterator::operator ++();
            ++it;
            return *this;
        }
        // post-increment
        iterator operator++(int) {
            iterator r(*this);
            operator++();
            return r;
        }
    }; // class iterator

    typedef typename list_t::const_iterator const_iterator;
    typedef typename list_t::const_reverse_iterator reverse_iterator;
    typedef typename list_t::const_reverse_iterator const_reverse_iterator;

public: // API

    // iterators

    iterator begin() {
        return iterator(list.begin());
    }

    const_iterator begin() const {
        return list.begin();
    }

    iterator end() {
        return iterator(list.end());
    }

    const_iterator end() const {
        return list.end();
    }

    // capacity

    bool empty() const {
        return list.empty();
    }

    size_t size() const {
        return list.size();
    }

    size_t max_size() const {
        return list.max_size();
    }

    // modifiers

    // insert single element
    std::pair<iterator, bool> insert(const T &val) {
        key_equal equal;
        // check if equivalent element is already in this set
        for (typename list_t::iterator it = list.begin(); it != list.end(); ++it) {
            if (equal(*it, val))
                return std::make_pair(iterator(it), false);
        }

        // element currently not in this set
        list.push_back(val);
        return std::make_pair(end(), true);
    }

    // insert range
    template <class InputIterator>
    void insert(InputIterator first, InputIterator last) {
        while (first != last) {
            insert(*first);
            ++first;
        }
    }

    iterator erase(iterator pos) {
        iterator next = pos;
        next++;
        list.erase(pos.it);
        return next;
    }

    size_t erase(const T &val) {
        key_equal equal;
        for (typename list_t::iterator it = list.begin(); it != list.end(); ++it) {
            if (equal(*it, val)) {
                list.erase(it);
                return 1;
            }
        }
        return 0;
    }

    void swap(list_set &other) {
        using std::swap;
        swap(list, other.list);
    }

    void clear() {
        list.clear();
    }

    // operations

    const_iterator find(const T &val) const {
        key_equal equal;
        for (typename list_t::const_iterator it = list.begin(); it != list.end(); ++it) {
            if (equal(*it, val))
                return it;
        }
        return end();
    }

    iterator find(const T &val) {
        key_equal equal;
        for (typename list_t::iterator it = list.begin(); it != list.end(); ++it) {
            if (equal(*it, val))
                return iterator(it);
        }
        return end();
    }

    size_t count(const T &val) const {
        return find(val) == end() ? 0 : 1;
    }
}; // class list_set

} // namespace util
