/* This file is part of the COO project.
   Copyright 2019  Micha Nelissen
   It is licensed under BSD-3-clause. */
#ifndef _COO_RTL_H
#define _COO_RTL_H

#include <stdint.h>
#include <stddef.h>

#ifdef _MSC_VER
#define coo_inline __forceinline
#define coo_atomic_t                 long
#define coo_atomic_inc_fetch(x)      InterlockedIncrementNoFence(x)
#define coo_atomic_dec_fetch(x)      InterlockedDecrementNoFence(x)
#define coo_atomic_fetch_inc(x)      InterlockedExchangeAddNoFence(x, 1)
#define coo_atomic_fetch_dec(x)      InterlockedExchangeSubtractNoFence(x, 1)
#define coo_atomic_inc_fetch_acq(x)  InterlockedIncrementAcquire(x)
#define coo_atomic_dec_fetch_acq(x)  InterlockedDecrementAcquire(x)
#define coo_atomic_fetch_inc_acq(x)  InterlockedExchangeAddAcquire(x, 1)
#define coo_atomic_fetch_dec_acq(x)  InterlockedExchangeSubtractAcquire(x, 1)
#define coo_atomic_inc_fetch_rel(x)  InterlockedIncrementRelease(x)
#define coo_atomic_dec_fetch_rel(x)  InterlockedDecrementRelease(x)
#define coo_atomic_fetch_inc_rel(x)  InterlockedExchangeAddRelease(x, 1)
#define coo_atomic_fetch_dec_rel(x)  InterlockedExchangeSubtractRelease(x, 1)
#define coo_atomic_inc_fetch_sync(x) InterlockedIncrement(x)
#define coo_atomic_dec_fetch_sync(x) InterlockedDecrement(x)
#define coo_atomic_fetch_inc_sync(x) InterlockedExchangeAdd(x, 1)
#define coo_atomic_fetch_dec_sync(x) InterlockedExchangeSubtract(x, 1)
#else
#define coo_inline extern inline __attribute__((always_inline)) __attribute__((gnu_inline))
#define coo_atomic_t                 unsigned
#define coo_atomic_inc_fetch(x)      __atomic_add_fetch(x, 1, __ATOMIC_RELAXED)
#define coo_atomic_dec_fetch(x)      __atomic_sub_fetch(x, 1, __ATOMIC_RELAXED)
#define coo_atomic_fetch_inc(x)      __atomic_fetch_add(x, 1, __ATOMIC_RELAXED)
#define coo_atomic_fetch_dec(x)      __atomic_fetch_sub(x, 1, __ATOMIC_RELAXED)
#define coo_atomic_inc_fetch_acq(x)  __atomic_add_fetch(x, 1, __ATOMIC_ACQUIRE)
#define coo_atomic_dec_fetch_acq(x)  __atomic_sub_fetch(x, 1, __ATOMIC_ACQUIRE)
#define coo_atomic_fetch_inc_acq(x)  __atomic_fetch_add(x, 1, __ATOMIC_ACQUIRE)
#define coo_atomic_fetch_dec_acq(x)  __atomic_fetch_sub(x, 1, __ATOMIC_ACQUIRE)
#define coo_atomic_inc_fetch_rel(x)  __atomic_add_fetch(x, 1, __ATOMIC_RELEASE)
#define coo_atomic_dec_fetch_rel(x)  __atomic_sub_fetch(x, 1, __ATOMIC_RELEASE)
#define coo_atomic_fetch_inc_rel(x)  __atomic_fetch_add(x, 1, __ATOMIC_RELEASE)
#define coo_atomic_fetch_dec_rel(x)  __atomic_fetch_sub(x, 1, __ATOMIC_RELEASE)
#define coo_atomic_inc_fetch_sync(x) __atomic_add_fetch(x, 1, __ATOMIC_SEQ_CST)
#define coo_atomic_dec_fetch_sync(x) __atomic_sub_fetch(x, 1, __ATOMIC_SEQ_CST)
#define coo_atomic_fetch_inc_sync(x) __atomic_fetch_add(x, 1, __ATOMIC_SEQ_CST)
#define coo_atomic_fetch_dec_sync(x) __atomic_fetch_sub(x, 1, __ATOMIC_SEQ_CST)
#endif
#ifndef container_of
#define container_of(ptr, type, var) \
        ((type *)((size_t)(ptr)-(size_t)(&((type *)0)->var)))
#endif

/* every vmt looks like: */
struct coo_vmt {
        size_t offset;          /* offset to vmt pointer from instance */
        const void *coo_class;  /* pointer to coo class hierarchy definition */
        /* ...virtual functions... */
};

void *coo_dyn_cast(const void *dest_class, const struct coo_vmt **vmt);

#endif
