#include "allocator.h"

#include <vector>
#include <memory>

#include <boost/make_unique.hpp>

typedef std::vector<std::shared_ptr<pool_allocator_base>> alloc_list;

// static copies of all pool allocators
alloc_list& get_allocators() {
    // returns a singleton: static variables are initialised only once
    static auto allocators = boost::make_unique<alloc_list>();
    return *allocators;
}

// is static
void pool_allocator_base::init(std::shared_ptr<pool_allocator_base> alloc) {
    get_allocators().push_back(alloc);
}

// is static
void pool_allocator_base::free_all() {
    for (std::shared_ptr<pool_allocator_base> alloc: get_allocators()) {
        alloc->deinit();
    }
    get_allocators().clear();
}
