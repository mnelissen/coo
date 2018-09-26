#line 1 "inc/bar/bar.hoo"
struct bar {
	int a, b;
	void *vmt;
};

#line 8 "inc/bar/bar.coo.h"
extern struct bar_vmt {
	void (*vbarf)(struct bar *this);
} bar_vmt;

#ifndef coo_inline
#ifdef _MSC_VER
#define coo_inline __forceinline
#else
#define coo_inline extern inline __attribute__((always_inline)) __attribute__((gnu_inline))
#endif
#endif
#ifndef container_of
#define container_of(ptr, type, node_var) \
  ((type *)((size_t)(ptr)-(size_t)(&((type *)0)->node_var)))
#endif

struct bar *new_bar(void);
int *bar_barf(struct bar *this, char a, int b);
void *bar_get_bar(struct bar *this, int a);
void bar_vbarf(struct bar *this);
void bar_bar_root(struct bar *this);
coo_inline void bar_vmt_vbarf(struct bar *this)
{
	((struct bar_vmt*)this->vmt)->vbarf(this);
}

#line 7 "inc/bar/bar.hoo"
