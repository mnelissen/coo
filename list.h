#ifndef _COO_LIST_H
#define _COO_LIST_H

/* forward list, declare a 'next' variable yourself in the item type */
#define flist(type, flist)   struct { type *first, **last; } flist
#define flist_init(flist)    (flist)->last = &(flist)->first
#define flist_empty(flist)   !(flist)->first
#define flist_first(flist)   (flist)->first
#define flist_add(flist, item, fieldname_next) \
        *(flist)->last = item, (flist)->last = &(item)->fieldname_next
#define flist_foreach(iter, flist, ...) \
        for (iter = (flist)->first; iter; iter = (iter)->__VA_ARGS__)
#define flist_foreach_continue(iter, flist, ...) \
        for (; iter; (iter)->__VA_ARGS__)

/* reverse list (stack), declare a 'prev' variable yourself in the item type */
#define rlist(type, rlist)   struct { type *rlast; } rlist
#define rlist_empty(rlist)   !(rlist)->rlast
#define rlist_add(rlist, item, fieldname_prev) \
        (item)->fieldname_prev = (rlist)->rlast, (rlist)->rlast = item
#define rlist_foreach(iter, rlist, fieldname_prev) \
        for (iter = (rlist)->rlast; iter; iter = (iter)->fieldname_prev)

/* bidirectional list */

#endif