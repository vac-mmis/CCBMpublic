#pragma once

#include <utility>
#include <functional>
#include <deque>

#include "allocator.h"

namespace util {

// A map implementation that can be used when only equality can be defined
// This implementation is based on a list and has therefore mostly very bad performance
// Thus, consider using unordered_map if you can additionally define a hash function,
// or use std::map if you can define a comparison operator
template <class K, class T, class Pred = std::equal_to<K>, class Alloc = std::allocator<std::pair<const K, T> > >
class list_map {
public: // typedef

    typedef K key_type;
    typedef T mapped_type;
    typedef std::pair<const K, T> value_type;
    typedef value_type & reference;
    typedef const value_type & const_reference;
    typedef value_type * pointer;
    typedef const value_type * const_pointer;
    typedef Pred key_equal;

private:
    typedef std::deque<value_type, Alloc> list_t;
    list_t list;

public: // typedef

    typedef typename list_t::iterator iterator;
    typedef typename list_t::const_iterator const_iterator;
    typedef typename list_t::reverse_iterator reverse_iterator;
    typedef typename list_t::const_reverse_iterator const_reverse_iterator;

public: // API

    // iterators

    iterator begin() {
        return list.begin();
    }

    const_iterator begin() const {
        return list.begin();
    }

    iterator end() {
        return list.end();
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
    std::pair<iterator, bool> insert(const value_type &val) {
        key_equal equal;
        // check if equivalent key is already in this set
        for (typename list_t::iterator it = list.begin(); it != list.end(); ++it) {
            if (equal(it->first, val.first))
                return std::make_pair(it, false);
        }

        // key currently not in this set
        list.push_back(val);
        return std::make_pair(list.end(), true);
    }

    // insert range
    template <class InputIterator>
    void insert(InputIterator first, InputIterator last) {
        while (first != last) {
            insert(*first);
            ++first;
        }
    }

    // element access
    mapped_type& operator[](const K &k) {
        return insert(std::make_pair(k, mapped_type())).first->second;
    }

    iterator erase(const_iterator pos) {
        iterator next = pos;
        next++;
        list.erase(pos);
        return next;
    }

    size_t erase(const K &k) {
        key_equal equal;
        for (typename list_t::iterator it = list.begin(); it != list.end(); ++it) {
            if (equal(it->first, k)) {
                list.erase(it);
                return 1;
            }
        }
        return 0;
    }

    void swap(list_map &other) {
        using std::swap;
        swap(list, other.list);
    }

    void clear() {
        list.clear();
    }

    // operations

    iterator find(const K &k) {
        key_equal equal;
        for (typename list_t::iterator it = list.begin(); it != list.end(); ++it) {
            if (equal(it->first, k))
                return it;
        }
        return end();
    }

    const_iterator find(const K &k) const {
        key_equal equal;
        for (typename list_t::const_iterator it = list.begin(); it != list.end(); ++it) {
            if (equal(it->first, k))
                return it;
        }
        return end();
    }

    size_t count(const K &k) const {
        return find(k) == end() ? 0 : 1;
    }
}; // class list_map

} // namespace util
