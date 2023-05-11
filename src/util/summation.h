#pragma once

#include "summation/standard_sum.h"
#include "summation/optimal_sum.h"
#include "summation/kahan_sum.h"


#ifdef KAHAN_SUMMATION
template <typename T>
using Sum = KahanSum<T>;
#elif OPTIMAL_SUMMATION
template <typename T>
using Sum = OptimalSum<T>;
#else
template <typename T>
using Sum = StandardSum<T>;
#endif
