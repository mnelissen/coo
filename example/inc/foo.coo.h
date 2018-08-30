#line 1 "inc/foo.hoo"
#include "bar/bar.coo.h"
#include "baz.h"

struct foo {
	int x;
	void *vmt;
	int *(*intfuncptr)(char c);
	int *y;
	unsigned int *z;
};

extern struct foo_vmt {
	void (*vfunc)(struct foo *foo, int arg1, float *arg2);
} foo_vmt;

#ifndef coo_inline
#ifdef _MSC_VER
#define coo_inline __forceinline
#else
#define coo_inline extern inline __attribute__((always_inline)) __attribute__((gnu_inline))
#endif
#endif

char foo_func(struct foo *this);
float foo_func1(struct foo *this, void *p);
int *foo_func2(struct foo *this, char a, int b);
void *foo_get_foo(struct foo *this, int a);
void foo_vfunc(struct foo *this, int arg1, float *arg2);
void foo_foo_root(struct foo *this);
coo_inline void foo_vmt_vfunc(struct foo *this, int arg1, float *arg2)
{
	((struct foo_vmt*)this->vmt)->vfunc(this, arg1, arg2);
}


#line 17
