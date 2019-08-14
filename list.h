#ifndef _COO_LIST_H
#define _COO_LIST_H

/* forward list, declare a 'next' variable yourself in the item type */
#define declare_flist(type, flist)   struct { type *first, **last; } flist
#define init_flist(flist)            (flist)->last = &(flist)->first
#define add_flist(flist, item, fieldname_next) \
        *(flist)->last = item, (flist)->last = &(item)->fieldname_next
#define flist_empty(flist)           !(flist)->first
#define foreach_flist(iter, flist, fieldname_next) \
        for (iter = (flist)->first; iter; iter = (iter)->fieldname_next)

/* reverse list (stack), declare a 'prev' variable yourself in the item type */
#define declare_rlist(type, rlist)   struct { type *last; } rlist
#define add_rlist(rlist, item, fieldname_prev) \
        (rlist)->last->fieldname_prev = item, (rlist)->prev = item
#define rlist_empty(rlist)           !(rlist)->last
#define foreach_rlist(iter, rlist, fieldname_prev) \
        for (iter = (rlist)->last; iter; iter = (iter)->fieldname_prev)

/* bidirectional list */

#endif