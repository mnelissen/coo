#line 1 "inc/foo.hoo"
#include "bar/bar.coo.h"
#include "baz.h"

struct foo {
	int x;
	struct coo_vmt *vmt;
	int *(*intfuncptr)(char c);
	int *y;
	unsigned int *z;
};

#line 14 "inc/foo.coo.h"
#include <coortl.h>
extern struct foo_vmt {
	struct coo_vmt vmt_base;
	void (*vfunc)(struct foo *this, int arg1, float *arg2);
} foo_vmt;

extern const struct foo_coo_class foo_coo_class;

struct foo *new_foo(void);
char foo_func(struct foo *this);
float foo_func1(struct foo *this, void *p);
int *foo_func2(struct foo *this, char a, int b);
void *foo_get_foo(struct foo *this, int a);
void foo_vfunc(struct foo *this, int arg1, float *arg2);
struct foo *foo_foo_root(struct foo *this);
coo_inline void foo_vmt_vfunc(struct foo *this, int arg1, float *arg2)
{
	((struct foo_vmt*)this->vmt)->vfunc(this, arg1, arg2);
}


#line 17 "inc/foo.hoo"
