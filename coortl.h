/* This file is part of the COO project.
   Copyright 2018  Micha Nelissen
   It is licensed under BSD-3-clause. */
#ifndef _COO_RTL_H
#define _COO_RTL_H

#include <stdint.h>

#ifdef _MSC_VER
#define coo_inline __forceinline
#else
#define coo_inline extern inline __attribute__((always_inline)) __attribute__((gnu_inline))
#endif
#ifndef container_of
#define container_of(ptr, type, node_var) \
  ((type *)((size_t)(ptr)-(size_t)(&((type *)0)->node_var)))
#endif

/* every vmt looks like: */
struct coo_vmt {
        size_t offset;          /* offset to vmt pointer from instance */
        const void *coo_class;  /* pointer to coo class def. that implements vmt */
        /* ...virtual functions... */
};

void *coo_dyn_cast(const struct coo_vmt **vmt, const void *dest_class);

#endif