#include <stdio.h>
#include <stdint.h>
#include "coortl.h"

#ifdef USE_VARUINT

typedef size_t varuint_t;

/* varuint is an unsigned integer taking a variable amount of space
     every byte codes 7 bits plus a "is_continued" bit (highest bit)
   UNUSED at this time, too hard to let compiler encode it
     need to estimate maximum struct size, but that needs reading MACROs */

static size_t decode_varuint(void **p_data)
{
        uint8_t u8, *data = *(uint8_t**)p_data;
        size_t ret;

        for (ret = 0;; ret <<= 7) {
                u8 = *data;
                ret += u8 & 0x7f;
                if ((u8 & 0x80) == 0)
                        break;
                ret <<= 7;
                data++;
        }

        *p_data = data;
        return ret;
}

static void *skip_varuints(void *p_data, size_t count)
{
        size_t i;

        for (i = 0; i < count; i++)
                decode_varuint(&p_data);
        return p_data;
}

#else

typedef uint32_t varuint_t;

static uint32_t decode_varuint(const void **p_data)
{
        uint32_t ret;

        ret = **(const uint32_t**)p_data;
        *p_data = (const uint32_t*)(*p_data) + 1;
        return ret;
}

#define skip_varuints(x, count) ((uint32_t*)x + count)

#endif

/* class format, if num_parents == 0 then offsets (also) empty
   varuint num_parents;
   varuint offsets[num_parents-1];
   void *parents[num_parents]; */

static void *recurse_dyn_cast(void *this,
                const void *this_class, const void *dest_class)
{
        varuint_t i, num_parents;
        const void *offsets;
        void *new_this;

        num_parents = decode_varuint(&this_class);
        if (num_parents == 0)
                return NULL;

        /* skip offsets */
        offsets = this_class;
        this_class = skip_varuints(this_class, num_parents-1);
        /* align to pointer width */
        this_class = (void*)(((size_t)this_class + (sizeof(void*)-1)) & ~(sizeof(void*)-1));
        for (i = 0;;) {
                if (*(void**)this_class == dest_class)
                        return this;
                new_this = recurse_dyn_cast(this, *(void**)this_class, dest_class);
                if (new_this)
                        return new_this;
                if (++i == num_parents)
                        return NULL;
                this = (char*)this + decode_varuint(&offsets);
                this_class = (char*)this_class + sizeof(void*);
        }
}

void *coo_dyn_cast(const struct coo_vmt **vmt, const void *dest_class)
{
        return recurse_dyn_cast((char*)vmt - (*vmt)->offset,
                (*vmt)->coo_class, dest_class);
}
