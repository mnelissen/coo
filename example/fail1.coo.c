#line 1 "fail1.coo"
#include <stdio.h>

struct A {
	int a1;
	void *vmt;
} A;

#line 10 "fail1.coo.c"
extern struct A_vmt {
	void (*vfunc_a)(struct A *this, int a2);
} A_vmt;

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

struct A *new_A();
void A_vfunc_a(struct A *this, int a2);
void A_A(struct A *this);
void A_A_root(struct A *this);
coo_inline void A_vmt_vfunc_a(struct A *this, int a2)
{
	((struct A_vmt*)this->vmt)->vfunc_a(this, a2);
}

#line 9 "fail1.coo"
struct B {
	struct A A;
	int b1;
	struct A a_local;
};

#line 43 "fail1.coo.c"
extern struct B_vmt {
	void (*vfunc_a)(struct B *this, int a2);
	void (*vfunc_b)(struct B *this, int b2);
} B_vmt;

struct B *new_B();
void B_vfunc_a(struct B *this, int a2);
void B_vfunc_b(struct B *this, int b2);
void B_B(struct B *this);
void B_B_root(struct B *this);
coo_inline void B_vmt_vfunc_b(struct B *this, int b2)
{
	((struct B_vmt*)this->A.vmt)->vfunc_b(this, b2);
}

#line 17 "fail1.coo"
void A_A(struct A *this)
{
	this->a1 = -1;
}

void A_vfunc_a(struct A *this, int arg)
{
	printf("A::vfunc(%d), a1=%d\n", arg, this->a1);
}

void B_B(struct B *this)
{
	A_A_root(&this->a_local);
}

#line 75 "fail1.coo.c"
struct B *new_B()
{
	struct B *this = malloc(sizeof(*this));
	if (this == NULL) return NULL;
	if (B_B_root(this, ) == NULL) {
		free(this);
		return NULL;
	}
	return this;
}

struct B_vmt B_vmt = {
	B_vfunc_a,
	B_vfunc_b,
};

void B_B_root(struct B *this)
{
	this->A.vmt = &B_vmt;
	B_B(this);
}

struct A *new_A()
{
	struct A *this = malloc(sizeof(*this));
	if (this == NULL) return NULL;
	if (A_A_root(this, ) == NULL) {
		free(this);
		return NULL;
	}
	return this;
}

struct A_vmt A_vmt = {
	A_vfunc_a,
};

void A_A_root(struct A *this)
{
	this->vmt = &A_vmt;
	A_A(this);
}
