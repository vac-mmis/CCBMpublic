#pragma once

#include <stdexcept>
#include <iterator>
#include <memory>

namespace util {

/**
 * Implements a reference counted,
 * tail sharing list
 * TODO: Check if space efficient enough,
 * possibly replace parts of the list without branching by vectors
 */
template<typename T>
class SharedList {
private:
    struct ListNode;

    typedef typename std::shared_ptr<ListNode> nodePtr;

    struct ListNode {
        T val;
        nodePtr next;
        ListNode(T val, nodePtr next) : val(val), next(next) {}
    };

    struct ListIterator : std::iterator<std::forward_iterator_tag, T> {
        nodePtr current;
        ListIterator(nodePtr current) : current(current) {}
        ListIterator& operator++() {
            current = current->next;
            return *this;
        }
        T& operator*() {
            return current->val;
        }
        const T& operator*() const {
            return current->val;
        }
        T* operator->() {
            return &current->val;
        }
        const T* operator->() const {
            return &current->val;
        }
        bool operator==(const ListIterator& other) const {
            return current == other.current;
        }
        bool operator!=(const ListIterator& other) const {
            return !(*this == other);
        }
    };

    struct ConstListIterator : std::iterator<std::forward_iterator_tag, T, ptrdiff_t, const T*, const T&> {
        nodePtr current;
        ConstListIterator(nodePtr current) : current(current) {}
        ConstListIterator& operator++() {
            current = current->next;
            return *this;
        }
        const T& operator*() const {
            return current->val;
        }
        const T* operator->() const {
            return &current->val;
        }
        bool operator==(const ConstListIterator& other) const {
            return current == other.current;
        }
        bool operator!=(const ConstListIterator& other) const {
            return !(*this == other);
        }

    };

    nodePtr head;

    SharedList(nodePtr head) : head(head) {}

public:
    typedef ListIterator iterator;
    typedef ConstListIterator const_iterator;

    SharedList() {}

    void push_front(T val) {
        nodePtr newHead = nodePtr(new ListNode(val,head));
        head = newHead;
    }

    void pop_front() {
        if (empty())
            throw std::runtime_error("Attempt to pop empty list!");

        head = head->next;
    }

    T front() {
        if (empty())
            throw std::runtime_error("Attempt to get front of empty list!");
        return head->val;
    }

    bool empty() {
        return !head;
    }

    iterator begin() {
        return ListIterator(head);
    }

    const_iterator cbegin() const {
        return ConstListIterator(head);
    }

    iterator end() {
        return ListIterator(nodePtr());
    }

    const_iterator cend() const {
        return ConstListIterator(nodePtr());
    }
};

} // namespace util
