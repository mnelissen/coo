#ifndef _COO_LIST_H
#define _COO_LIST_H

/* forward list, declare a 'next' variable yourself in the item type */
#define flist(type)          struct { type *first, **last; }
#define flist_clear(flist)   (flist)->first = NULL, flist_init(flist)
#define flist_init(flist)    (flist)->last = &(flist)->first
#define flist_empty(flist)   !(flist)->first
#define flist_first(flist)   (flist)->first
#define flist_last(flist, type, item) ((type*)((char*)(flist)->last - (char*)&((type*)0)->item))
#define flist_add(flist, item, fieldname_next) \
        *(flist)->last = item, (flist)->last = &(item)->fieldname_next
/* iterate forwards, provide item, address of flist, fieldname of item next pointer */
#define flist_foreach(item, flist, ...) \
        for (item = (flist)->first; item; item = (item)->__VA_ARGS__)
#define flist_foreach_continue(item, flist, ...) \
        for (; item; (item)->__VA_ARGS__)

/* backward list (stack), declare a 'prev' variable yourself in the item type */
#define blist(type)          struct { type *rlast; }
#define blist_clear(blist)   (blist)->rlast = NULL
#define blist_empty(blist)   !(blist)->rlast
#define blist_add_section(blist, first, last, fieldname_prev) \
        (last)->fieldname_prev = (blist)->rlast, (blist)->rlast = first
#define blist_add(blist, item, fieldname_prev) \
        blist_add_section(blist, item, item, fieldname_prev)
#define blist_pop_last(blist, pitem, fieldname_prev) \
        (*(pitem) = (blist)->rlast, \
         *(pitem) ? (blist)->rlast = (*(pitem))->fieldname_prev, *(pitem) : NULL)
#define blist_remove_next(pitem, fieldname_prev) \
        *(pitem) = (*(pitem))->fieldname_prev
#define blist_remove_last(blist, fieldname_prev) \
        (blist)->rlast = (blist)->rlast->fieldname_prev
#define blist_remove_until(blist, item) (blist)->rlast = (item)
#define blist_foreach_rev(item, blist, ...) \
        for (item = (blist)->rlast; item; item = item->__VA_ARGS__)
#define blist_foreach_rev_p(pprev, item, blist, ...) \
        for (pprev = &(blist)->rlast; (item = *pprev); pprev = &item->__VA_ARGS__)

/* double linked list, iname is variable name for list item prev/next pointers */
#define dlist(type)                struct { type *dprev, *dnext; }
#define dlist_item(type)           struct { type *iprev, *inext; }
#define dlist_empty(dlist, iname)  ((dlist)->dnext->iname.inext == (dlist)->dnext)
#define dlist_first(dlist)         (dlist)->dnext
#define dlist_last(dlist)          (dlist)->dprev
#define dlist_head(dlist, iname) \
        (void*)((char*)(dlist) - ((char*)(&(dlist)->dnext->iname.iprev) - (char*)(dlist)->dnext))
#define dlist_init(dlist, iname) \
        (dlist)->dnext = NULL, (dlist)->dprev = (dlist)->dnext = dlist_head(dlist, iname)
#define dlist_insert_before(item, newitem, iname) \
        (newitem)->iname.iprev = (item)->iname.iprev, (newitem)->iname.inext = item, \
        (item)->iname.iprev->iname.inext = newitem, (item)->iname.iprev = newitem
#define dlist_add(dlist, newitem, iname) \
        (newitem)->iname.iprev = (dlist)->dprev, (newitem)->iname.inext = \
                (void*)((size_t)(dlist) - ((size_t)&(item)->iname) - (size_t)(item)), \
        (dlist)->dprev->iname.inext = newitem, (dlist)->dprev = newitem
#define dlist_remove_section(first, last, iname) \
        (first)->iname.iprev->iname.inext = (last)->iname.inext, \
        (last)->iname.inext->iname.iprev = (first)->iname.iprev
#define dlist_remove(item, iname) dlist_remove_section(item, item, iname)
#define dlist_remove_and_null(item, iname) \
        dlist_remove(item, iname), (item)->iname.iprev = (item)->iname.inext = NULL
#define dlist_foreach(item, dlist, iname) \
        for (item = (dlist)->dnext; \
                &(item)->iname.inext != &(dlist)->dnext; \
                item = (item)->iname.inext)
#define dlist_foreach_safe(item, next, dlist, iname) \
        for (item = (dlist)->dnext, next = (item)->iname.inext; \
                &(item)->iname.inext != &(dlist)->dnext; \
                item = next, next = (item)->iname.inext)
#define dlist_foreach_continue(item, dlist, iname) \
        for (; &(item)->iname.inext != &(dlist)->dnext; item = (item)->iname.inext)
#define dlist_foreach_continue_safe(item, next, dlist, iname) \
        for (next = (item)->iname.inext; \
                &(item)->iname.inext != &(dlist)->dnext; \
                item = next, next = (item)->iname.inext)
#define dlist_foreach_rev(item, dlist, iname) \
        for (item = (dlist)->dprev; \
                &(item)->iname.iprev != &(dlist)->dprev; \
                item = (item)->iname.iprev)

#endif