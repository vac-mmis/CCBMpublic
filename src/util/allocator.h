#pragma once

#include <iostream>
#include <cassert>
#include <list>
#include <cstdlib>
#include <limits>
#include <memory>
#include <utility>

#include "../valgrind/memcheck.h"
#include "../config.h"


//      Forward Declarations
// #############################################################################


// There are two variants of the allocator. Use pool_allocator if requests larger than 2^poolExp must be served.
// Otherwise, use pool_small_allocator -- this saves memory and is even a bit faster
// However, for some STL containers like unordered_set/map, vector and deque (!), which may reserve arbitrarily large blocks of memory at once, you have to use the pool_allocator

template <typename T, unsigned poolExp = 23>
class pool_small_allocator;

template <typename T, unsigned poolExp = 23>
class pool_allocator;

namespace util {

template <typename T, class Alloc>
struct Deleter;

template <typename T, class Alloc>
std::shared_ptr<T> make_shared(Alloc &alloc);

template <typename T, class Alloc>
std::shared_ptr<T> make_shared(Alloc &alloc, const T& val);

} // namespace util



// Declarations
// #############################################################################



//      struct pool_allocator_base
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// base class for all pool allocators
// main purpose: free all memory at program exit
// has static field to keep track of all pool_allocators
// definition inside allocator.cpp
struct pool_allocator_base {
protected:
    // must be called by every allocator to register itself
    static void init(std::shared_ptr<pool_allocator_base> alloc);

public:
    // must be overriden by any allocator to free its storage
    virtual void deinit() = 0;

    // frees memory of all pool_allocators - to be called once at program exit
    static void free_all();
}; // class pool_allocator_base


namespace allocator {
namespace detail {


//      struct PoolInfo
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// This is stored at the beginning of every pool
// This allows to access the "freed" count by a simple bit-operation of the pointer
// →
// The pools must be aligned to poolExp addresses (e.g. 8MB = 2^23, i.e. the low 23 bits are different for all pointers, and clearing the low 23 bits results in the PoolInfo struct)
struct PoolInfo {
    // first "free" address
    void *free;

    unsigned alloced; // how many allocations have been made
    unsigned freed; // how many frees have been made

    // the current iterator position in the pool list
    // used to quickly move this pool within the list
    std::list<PoolInfo*>::iterator pool_it;
}; // struct PoolInfo


} // namespace allocator:: detail
} // namespace allocator


#if MEMPOOL==0
// in case no custom memory pools shall be used, simply inherit from the default allocator
// this will ease debugging, esp. with valgrind

template <typename T, unsigned>
class pool_small_allocator : public pool_allocator_base, public std::allocator<T> {
    using std::allocator<T>::allocator;
    void deinit() {}
};
template <typename T, unsigned>
class pool_allocator : public pool_allocator_base, public std::allocator<T> {
    using std::allocator<T>::allocator;
    void deinit() {}
};

#else


//      class pool_small_allocator
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

// TODO if NVALGRIND is NOT defined (i.e. probably running under valgrind), add "red zone" of 16 bytes before and after every allocation to detect overflows (16 is also used by memcheck per default for malloc)

/*
new idea of fast allocator (similar to Java allocator w/o GC?)
This is pretty fast, but may create larger wholes of unused memory

each pool has fixed size of 2^poolExp
It only tracks the start of the free region, and the number of de/allocations.
If everything has been freed, the free region can be re-set to the full buffer

Everything is static because some classes create copies of allocators (e.g. shared_ptr).
This also makes it very lightweight, creating a copy costs nothing

Note: never try to have a static instance of this class, as it has itself static fields which are used in the constructor (static initialization order fiasco)

chunkExp:
    each pool has a size of 2^poolExp (e.g. 23 = 8MB)
*/
template <typename T, unsigned poolExp>
class pool_small_allocator : public pool_allocator_base {
public:
    // typedefs
    typedef T value_type;
    typedef value_type* pointer;
    typedef const value_type* const_pointer;
    typedef value_type& reference;
    typedef const value_type& const_reference;
    typedef std::size_t size_type;
    typedef std::ptrdiff_t difference_type;

    // convert an allocator<T> to allocator<U>, required for allocators
    template<typename U>
    struct rebind {
        typedef pool_small_allocator<U> other;
    };


protected:

    typedef allocator::detail::PoolInfo PoolInfo;

    // list of pointers to the pools
    static std::list<PoolInfo*> pools;

    // reference counter - when it reaches zero, we can free the pool completely
    static size_t count;

    // poolHandle for valgrind and used as indicator of the pool has already been initialised
    static void* poolHandle;

    // create a new pool of size 2^exp and add it to the front of the list
    // returns a pointer to the start of the pool
    static PoolInfo* makePool(unsigned exp);

    // try to allocate in the pool
    // returns the pointer on success, or NULL when no space is free
    static void* try_allocate(PoolInfo *pi, size_t size);

    // internal allocation that is used when the first pool is not free
    // this contains the most complex code of the allocation method:
    // in most cases, what allocate() does directly is sufficient
    // having a separate method should make inlining the actual allocate method more probable
    static T* allocate2(size_t size, PoolInfo *firstPool);

    void deinit();

public:

    void initialize();

    pool_small_allocator() {
        count++;
        initialize();
    }

    // copy constructor
    pool_small_allocator(const pool_small_allocator &alloc) {
        (void) alloc;
        count++;
    }

    // C++11: use delegating constructor
    template <typename U>
    pool_small_allocator(const pool_small_allocator<U> &alloc) {
        (void) alloc;
        // simply create a new, converting the type of the allocator

        count++;
        initialize();
    }

    ~pool_small_allocator() {
        count--;
        // we should not destroy the mempool here, because the allocator may be constructed out of nowhere
        // and it is not guaranteed that no references are still alive
        // however, just for reference, here is how to destroy the pool:
        // if (count == 0) {
        //     VALGRIND_DESTROY_MEMPOOL(poolHandle);
        //     free(poolHandle);
        //     poolHandle = NULL;
        //     for (PoolInfo* p: pools) {
        //         free(p);
        //     }
        // }
    }

    pool_small_allocator& operator=(const pool_small_allocator &alloc) {
        (void) alloc;
        // assignment does not have to do anything
        // before assignment, there are at least two instances
        // after assignment, there are still two instances
        // → the instance counter does not need to be incremented
        return *this;
    }

    // hint argument is unneeded, but needs a default value
    static T* allocate(size_t count, const_pointer hint = 0);

    // deallocate count elements starting at p
    static void deallocate(T* p, size_t count);

    static T* address(T& r) {
        return &r;
    }

    static const T* address(const T& r) {
        return &r;
    }

    static size_t max_size() {
        return (1<<poolExp) - sizeof(PoolInfo);
    }

    template <class U, class... Args>
    static void construct(U* p, Args&&... args) {
        ::new ((void*)p) U (std::forward<Args>(args)...);
    }

    static void destroy(pointer p) {
        p->~T();
    }

    bool operator==(const pool_small_allocator &rhs) const {
        (void) rhs;
        return true;
    }
    bool operator!=(const pool_small_allocator &rhs) const { return !(*this == rhs); }
}; // class pool_small_allocator



//      class pool_allocator
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// The same as above, but allocate can handle arbitrarily large requests
template <typename T, unsigned poolExp>
class pool_allocator : public pool_allocator_base {
public:
    // typedefs
    typedef T value_type;
    typedef value_type* pointer;
    typedef const value_type* const_pointer;
    typedef value_type& reference;
    typedef const value_type& const_reference;
    typedef std::size_t size_type;
    typedef std::ptrdiff_t difference_type;

    // convert an allocator<T> to allocator<U>, required for allocators
    template<typename U>
    struct rebind {
        typedef pool_allocator<U> other;
    };


protected:

    typedef allocator::detail::PoolInfo PoolInfo;

    // list of pointers to the pools
    static std::list<PoolInfo*> pools;

    // reference counter - when it reaches zero, we can free the pool completely
    static size_t count;

    // poolHandle for valgrind and used as indicator of the pool has already been initialised
    static void* poolHandle;

    // create a new pool of size 2^exp and add it to the front of the list
    // returns a pointer to the start of the pool
    static PoolInfo* makePool(unsigned exp);

    // try to allocate in the pool
    // returns the pointer on success, or NULL when no space is free
    static void* try_allocate(PoolInfo *pi, size_t size);

    // internal allocation that is used when the first pool is not free
    // this contains the most complex code of the allocation method:
    // in most cases, what allocate() does directly is sufficient
    // having a separate method should make inlining the actual allocate method more probable
    static T* allocate2(size_t size, PoolInfo *firstPool);

    void deinit();

public:

    void initialize();

    pool_allocator() {
        count++;
        initialize();
    }

    // copy constructor
    pool_allocator(const pool_allocator &alloc) {
        (void) alloc;
        count++;
    }

    // C++11: use delegating constructor
    template <typename U>
    pool_allocator(const pool_allocator<U> &alloc) {
        (void) alloc;
        count++;
        initialize();
    }

    ~pool_allocator() {
        count--;
    }

    pool_allocator& operator=(const pool_allocator &alloc) {
        (void) alloc;
        return *this;
    }

    // hint argument is unneeded, but needs a default value
    static T* allocate(size_t count, const_pointer hint = 0);

    // deallocate count elements starting at p
    static void deallocate(T* p, size_t count);

    static T* address(T& r) {
        return &r;
    }

    static const T* address(const T& r) {
        return &r;
    }

    static size_t max_size() {
        return std::numeric_limits<size_t>::max();
    }

    template <class U, class... Args>
    static void construct(U* p, Args&&... args) {
        ::new ((void*)p) U (std::forward<Args>(args)...);
    }

    static void destroy(pointer p) {
        p->~T();
    }

    bool operator==(const pool_allocator &rhs) const {
        (void) rhs;
        return true;
    }
    bool operator!=(const pool_allocator &rhs) const { return !(*this == rhs); }
}; // class pool_allocator

#endif // MEMPOOL

//      class Deleter
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


namespace util {

template <typename T, class Alloc>
struct Deleter {
    Alloc &alloc;

    Deleter(Alloc &alloc) :
        alloc(alloc)
    { }

    void operator()(T* p) {
        p->~T();
        alloc.deallocate(p, 1);
    }
};

} // namespace util




//      Implementations
// #############################################################################


#if MEMPOOL



//      class pool_small_allocator
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*
 * The Allocator has a list<Pool>
 *     The Pools are sorted according to the following scheme
 *     <currently allocated, still has some free space> <had some free space> … <had some free space> <full> … <full>
 *     The first entries are always those with enough free size
 *     If the first and the second are full - all are full → no need to search the full list
 * Deallocation:
 *     Find buf by clearing lowest poolExp bits, call free(buf, p)
 *     If, by a deallocation, a Pool gets free, move it from its current position to the beginning of the list
 * Allocation:
 *     Try to allocate in first Pool
 *     If fails, move the first pool to end, making the second the new first and check if this Pool is free
 *         * if no, all are full → create new pool and insert at beginning of list
 *
 */

template <typename T, unsigned poolExp>
void pool_small_allocator<T, poolExp>::initialize() {
    if (poolHandle != NULL)
        // the pool has already been initialised
        return;

    // always have at least one pool of the minimum size
    // this avoids the test for an existing pool in the allocate method and saves instructions
    makePool(poolExp);
    poolHandle = malloc(1);
    VALGRIND_CREATE_MEMPOOL(poolHandle, 0, false);

    // call base class's init method to register ourselves
    init(std::make_shared<pool_small_allocator>());
}

template <typename T, unsigned poolExp>
void pool_small_allocator<T, poolExp>::deinit() {
    if (poolHandle == NULL)
        return;

    VALGRIND_DESTROY_MEMPOOL(poolHandle);

    for (PoolInfo *pi: pools) {
        //assert(pi->alloced == pi->freed && "pool_small_allocator: memory leak detected during deinitialization");
        free(pi);
    }
    pools.clear();

    free(poolHandle);
    poolHandle = NULL;
}

template <typename T, unsigned poolExp>
allocator::detail::PoolInfo* pool_small_allocator<T, poolExp>::makePool(unsigned exp) {
    PoolInfo *pi = NULL;
    const size_t size = 1<<exp;
    const size_t align = size;

    posix_memalign(reinterpret_cast<void**>(&pi), align, size);
    // check for proper alignment
    assert(pi != NULL && ( reinterpret_cast<size_t>(pi) & (align-1)) == 0);

    // The first free address is right after the PoolInfo struct
    pi->free = static_cast<void*>(pi+1);
    pi->alloced = 0;
    pi->freed = 0;
    pools.push_front(pi);
    pi->pool_it = pools.begin();

    (void) VALGRIND_MAKE_MEM_NOACCESS(pi->free, size - sizeof(PoolInfo));

    return pi;
}


template <typename T, unsigned poolExp>
void* pool_small_allocator<T, poolExp>::try_allocate(PoolInfo *pi, size_t size) {
    // check if there is enough buffer free (free + size < pool_start + pool_size)
    if (reinterpret_cast<size_t>(pi->free) + size >= reinterpret_cast<size_t>(pi) + (1<<poolExp))
        return NULL;

    pi->alloced++;
    void *result = pi->free;
    pi->free = static_cast<void*>(static_cast<char*>(pi->free) + size);
    return result;
}


template <typename T, unsigned poolExp>
T* pool_small_allocator<T, poolExp>::allocate(size_t count, const_pointer hint) {
    (void) hint;

    // if count == 0, we should actually set the size to 1 to have distinct "objects" returned
    // but this should not happen anyway, so we just hope nobody does allocate(0) (just like malloc(0))

    const size_t size = count * sizeof(T);

    // first optimistically try to allocate some space in the first pool
    // here, we initially do not prematurely check the size (if it is larger than the full pool size)
    // in most cases, the pool will be large enough - this saves some cycles

    PoolInfo *pi = pools.front();
    void *result = try_allocate(pi, size);
    if (result != NULL) {
        VALGRIND_MEMPOOL_ALLOC(poolHandle, result, size);
        return static_cast<T*>(result);
    }

    // the first pool is full
    // try to allocate with more complex code
    // having a rare call here increases the chance of inlining the small code of allocate
    return allocate2(size, pi);
}


template <typename T, unsigned poolExp>
T* pool_small_allocator<T, poolExp>::allocate2(size_t size, PoolInfo *firstPool) {
    PoolInfo *pi = firstPool;

    if (pools.size() > 1) {
        // if we have currently more than one pool:
        // * push the full pool to the end, thus avoid trying to allocate in it over and over again
        // * try second pool, this might have free space since two completely freed pools may be at the front of the list

        pools.erase(pools.begin());
        pools.push_back(pi);
        pi->pool_it = pools.end();
        pi->pool_it--;

        pi = pools.front();
        void *result = try_allocate(pi, size);
        if (result != NULL) {
            VALGRIND_MEMPOOL_ALLOC(poolHandle, result, size);
            return static_cast<T*>(result);
        }

        // second pool is also full
        pools.erase(pools.begin());
        pools.push_back(pi);
        pi->pool_it = pools.end();
        pi->pool_it--;
    }

    // we seem to have no free pool, so create a new pool
    pi = makePool(poolExp);
    void *result = try_allocate(pi, size);
    if (result != NULL) {
        VALGRIND_MEMPOOL_ALLOC(poolHandle, result, size);
        return static_cast<T*>(result);
    }

    // even the new pool cannot allocate
    throw std::bad_alloc();
}


template <typename T, unsigned poolExp>
void pool_small_allocator<T, poolExp>::deallocate(T* p, size_t count) {
    (void) count;

    VALGRIND_MEMPOOL_FREE(poolHandle, p);

    // get pool address: mask alignment bits
    void *addr = reinterpret_cast<void*>(reinterpret_cast<size_t>(p) & ~((1<<poolExp)-1));
    PoolInfo *pi = static_cast<PoolInfo*>(addr);
    pi->freed++;

    if (pi->freed != pi->alloced)
        return;

    // this pool is free, we can clear it
    PoolInfo *pi_front = pools.front();
    if (pi_front != pi && pi_front->alloced == 0) {
        // the first pool is also completely free, so we can throw this particular pool away
        pools.erase(pi->pool_it);
        free(pi);
    } else {
        // push the free pool to the beginning of the list
        pi->free = static_cast<void*>(pi+1);
        pi->alloced = 0;
        pi->freed = 0;
        pools.erase(pi->pool_it);
        pools.push_front(pi);
        pi->pool_it = pools.begin();
    }
}


//      class pool_allocator
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*
 * Also allowing an allocation request greater than 2^poolExp makes things a bit more difficult (this is required, e.g. for large unordered_maps)
 * We cannot simply pass the request to malloc, because when deallocate is called, we have to somehow figure out if this is some pool
 * (In particular, we do not know if the address when masking the low poolExp bits contains a valid PoolInfo struct)
 * Therefore, for *every* request (no matter its size) we have to indicate if the pointer belongs to some pool or not
 * Due to alignment, we can reserve simply sizeof(void*) bytes and store the pointer to the pool beginning (i.e. PoolInfo) struct
 * This actually allows us to make every pool arbitrarily large, without the need for huge alignment issues
 *
 * If this pool is configured to not allow larger requests, we do not need this
 * Instead, we request memory alligned to poolExp bits, and we can simply (by bit-masking) get the start of the pool
 * This saves memory and avoids one possible test / indirection
 */

template <typename T, unsigned poolExp>
void pool_allocator<T, poolExp>::initialize() {
    if (poolHandle != NULL)
        // the pool has already been initialised
        return;

    // always have at least one pool of the minimum size
    // this avoids the test for an existing pool in the allocate method and saves instructions
    makePool(poolExp);
    poolHandle = malloc(1);
    VALGRIND_CREATE_MEMPOOL(poolHandle, 0, false);

    // call base class's init method to register ourselves
    init(std::make_shared<pool_allocator>());
}

template <typename T, unsigned poolExp>
void pool_allocator<T, poolExp>::deinit() {
    if (poolHandle == NULL)
        return;

    VALGRIND_DESTROY_MEMPOOL(poolHandle);

    for (PoolInfo *pi: pools) {
        //assert(pi->alloced == pi->freed && "pool_allocator: memory leak detected during deinitialization");
        free(pi);
    }
    pools.clear();

    free(poolHandle);
    poolHandle = NULL;
}


template <typename T, unsigned poolExp>
allocator::detail::PoolInfo* pool_allocator<T, poolExp>::makePool(unsigned exp) {
    const size_t size = 1<<exp;
    PoolInfo *pi = static_cast<PoolInfo*>(malloc(size));

    pi->free = static_cast<void*>(pi+1);
    pi->alloced = 0;
    pi->freed = 0;
    pools.push_front(pi);
    pi->pool_it = pools.begin();

    (void) VALGRIND_MAKE_MEM_NOACCESS(pi->free, size - sizeof(PoolInfo));

    return pi;
}


template <typename T, unsigned poolExp>
void* pool_allocator<T, poolExp>::try_allocate(PoolInfo *pi, size_t size) {
    // the size must already contain the space reserved for the PoolInfo* back-pointer

    // check if there is enough buffer free (free + size < pool_start + pool_size)
    if (reinterpret_cast<size_t>(pi->free) + size >= reinterpret_cast<size_t>(pi) + (1<<poolExp))
        return NULL;

    pi->alloced++;
    void *alloc = pi->free;
    pi->free = static_cast<void*>(static_cast<char*>(pi->free) + size);

    // alloc is the full allocation
    // it starts with a pointer to the PoolInfo struct
    PoolInfo **piPtr = static_cast<PoolInfo**>(alloc);
    (void) VALGRIND_MAKE_MEM_UNDEFINED(piPtr, sizeof(PoolInfo*));
    *piPtr = pi;
    (void) VALGRIND_MAKE_MEM_NOACCESS(piPtr, sizeof(PoolInfo*));

    // the returned result must be increased by the size of that pointer
    return static_cast<void*>(piPtr + 1);
}


template <typename T, unsigned poolExp>
T* pool_allocator<T, poolExp>::allocate(size_t count, const_pointer hint) {
    (void) hint;

    // if count == 0, we should actually set the size to 1+sizeof(void) to have distinct "objects" returned
    // but this should not happen anyway, so we just hope nobody does allocate(0) (just like malloc(0))

    // the size must contain the space reserved for the PoolInfo* back-pointer
    const size_t size = count * sizeof(T) + sizeof(PoolInfo*);

    // first optimistically try to allocate some space in the first pool
    // here, we initially do not prematurely check the size (if it is larger than the full pool size)
    // in most cases, the pool will be large enough - this saves some cycles

    PoolInfo *pi = pools.front();
    void *result = try_allocate(pi, size);
    if (result != NULL) {
        VALGRIND_MEMPOOL_ALLOC(poolHandle, result, size);
        return static_cast<T*>(result);
    }

    // the first pool is full
    // try to allocate with more complex code
    // having a rare call here increases the chance of inlining the small code of allocate
    return allocate2(size, pi);
}


template <typename T, unsigned poolExp>
T* pool_allocator<T, poolExp>::allocate2(size_t size, PoolInfo *firstPool) {
    PoolInfo *pi = firstPool;

    // if the first pool size is smaller than the requested size, the first pool is always full
    // therefore, do not consider this really as full (and hence push it to the end)
    // instead, simply malloc memory large enough for the request (and enough bytes in front as NULL pointer to PoolInfo to indicate this)
    if (size > (1<<poolExp) - sizeof(PoolInfo)) {
        void *alloc = malloc(size);
        if (alloc == NULL)
            // malloc could not serve our request
            throw std::bad_alloc();

        // alloc is the full allocation
        // it starts with a pointer to the PoolInfo struct
        PoolInfo **piPtr = static_cast<PoolInfo**>(alloc);
        *piPtr = NULL; // NULL indicates no PoolInfo

        // the returned result must be increased by the size of that pointer
        void *result = static_cast<void*>(piPtr + 1);

        VALGRIND_MEMPOOL_ALLOC(poolHandle, result, size);
        return reinterpret_cast<T*>(result);
    }

    // the request has a "usual" size to fit in any pool

    if (pools.size() > 1) {
        // if we have currently more than one pool:
        // * push the full pool to the end, thus avoid trying to allocate in it over and over again
        // * try second pool, this might have free space since two completely freed pools may be at the front of the list

        pools.erase(pools.begin());
        pools.push_back(pi);
        pi->pool_it = pools.end();
        pi->pool_it--;

        pi = pools.front();
        void *result = try_allocate(pi, size);
        if (result != NULL) {
            VALGRIND_MEMPOOL_ALLOC(poolHandle, result, size);
            return static_cast<T*>(result);
        }

        // second pool is also full
        pools.erase(pools.begin());
        pools.push_back(pi);
        pi->pool_it = pools.end();
        pi->pool_it--;
    }

    // we seem to have no free pool, so create a new pool
    pi = makePool(poolExp);
    void *result = try_allocate(pi, size);
    if (result != NULL) {
        VALGRIND_MEMPOOL_ALLOC(poolHandle, result, size);
        return static_cast<T*>(result);
    }

    // even the new pool cannot allocate
    throw std::bad_alloc();
}


template <typename T, unsigned poolExp>
void pool_allocator<T, poolExp>::deallocate(T* p, size_t count) {
    (void) count;

    VALGRIND_MEMPOOL_FREE(poolHandle, p);

    // get pool address: the PoolInfo* right before p
    PoolInfo **piPtr = static_cast<PoolInfo**>(static_cast<void*>(p)) - 1;
    (void) VALGRIND_MAKE_MEM_DEFINED(piPtr, sizeof(PoolInfo*));
    PoolInfo *pi = *piPtr;

    if (pi == NULL) {
        // this address is not part of any pool, but was a distinct malloc call
        free(piPtr);
        return;
    }

    pi->freed++;

    if (pi->freed != pi->alloced)
        return;

    // this pool is free, we can clear it
    PoolInfo *pi_front = pools.front();
    if (pi_front != pi && pi_front->alloced == 0) {
        // the first pool is also completely free, so we can throw this particular pool away
        pools.erase(pi->pool_it);
        free(pi);
    } else {
        // push the free pool to the beginning of the list
        pi->free = static_cast<void*>(pi+1);
        pi->alloced = 0;
        pi->freed = 0;
        pools.erase(pi->pool_it);
        pools.push_front(pi);
        pi->pool_it = pools.begin();
    }
}

#endif // MEMPOOL


//      make_shared
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

namespace util {

template <typename T, class Alloc>
std::shared_ptr<T> make_shared(Alloc &alloc) {
    T* p = alloc.allocate(1);
    new (p) T();
    return std::shared_ptr<T>(p, Deleter<T, Alloc>(alloc), alloc);
}

template <typename T, class Alloc>
std::shared_ptr<T> make_shared(Alloc &alloc, const T& val) {
    T* p = alloc.allocate(1);
    new (p) T(val);
    return std::shared_ptr<T>(p, Deleter<T, Alloc>(alloc), alloc);
}

} // namespace util



//      static variables
// #############################################################################

#if MEMPOOL

template <typename T, unsigned poolExp>
std::list<allocator::detail::PoolInfo*> pool_small_allocator<T, poolExp>::pools;

template <typename T, unsigned poolExp>
size_t pool_small_allocator<T, poolExp>::count = 0;

template <typename T, unsigned poolExp>
void *pool_small_allocator<T, poolExp>::poolHandle = NULL;


template <typename T, unsigned poolExp>
std::list<allocator::detail::PoolInfo*> pool_allocator<T, poolExp>::pools;

template <typename T, unsigned poolExp>
size_t pool_allocator<T, poolExp>::count = 0;

template <typename T, unsigned poolExp>
void *pool_allocator<T, poolExp>::poolHandle = NULL;

#endif // MEMPOOL
