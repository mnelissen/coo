#line 1 "example.coo"
#include <stdio.h>
#include <foo.coo.h>

typedef struct A {
	int a1;
	void *vmt;
} A;

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

void A_vfunc_a(struct A *this, int a2);
void A_A(struct A *this);
void A_A_root(struct A *this);
coo_inline void A_vmt_vfunc_a(struct A *this, int a2)
{
	((struct A_vmt*)this->vmt)->vfunc_a(this, a2);
}

#line 10
struct B {
	struct A A;
	int b1;
	struct A a_local;
};

extern struct B_vmt {
	void (*vfunc_a)(struct B *this, int a2);
	void (*vfunc_b)(struct B *this, int b2);
} B_vmt;

void B_vfunc_a(struct B *this, int a2);
void B_vfunc_b(struct B *this, int b2);
void B_B(struct B *this);
void B_B_root(struct B *this);
coo_inline void B_vmt_vfunc_b(struct B *this, int b2)
{
	((struct B_vmt*)this->A.vmt)->vfunc_b(this, b2);
}

#line 18
struct C {
	struct A *A;
	int c1;
	void *vmt;
};

struct C_root {
	struct C C;
	struct A A;
};

extern struct C_A_vmt {
	void (*vfunc_a)(struct A *this, int a2);
} C_A_vmt;

extern struct C_vmt {
	void (*vfunc_c)(struct C *this, int c2);
} C_vmt;

void C_vfunc_a(struct C *this, int a2);
void C_vfunc_c(struct C *this, int c2);
void C_C_root(struct C_root *this);
coo_inline void C_vmt_vfunc_c(struct C *this, int c2)
{
	((struct C_vmt*)this->vmt)->vfunc_c(this, c2);
}

#line 24
struct D {
	struct B B;
	struct C C;
	int d1;
};

extern struct D_vmt {
	void (*vfunc_a)(struct D *this, int a2);
	void (*vfunc_b)(struct D *this, int b2);
	void (*vfunc_d)(struct D *this, struct A* a, struct B *b);
} D_vmt;

extern struct D_C_vmt {
	void (*vfunc_c)(struct C *this, int c2);
} D_C_vmt;

void D_vfunc_a(struct D *this, int a2);
void D_vfunc_b(struct D *this, int b2);
void D_vfunc_c(struct C *this, int c2);
void D_D(struct D *this, struct A *a);
void D_vfunc_d(struct D *this, struct A* a, struct B *b);
void D_D_root(struct D *this, struct A *a);
coo_inline void D_vmt_vfunc_d(struct D *this, struct A* a, struct B *b)
{
	((struct D_vmt*)this->B.A.vmt)->vfunc_d(this, a, b);
}

#line 33
struct E {
	struct D D;
	int e1;
};

extern struct E_vmt {
	void (*vfunc_a)(struct D *this, int a2);
	void (*vfunc_b)(struct E *this, int b2);
	void (*vfunc_d)(struct E *this, struct A* a, struct B *b);
} E_vmt;

void E_vfunc_b(struct E *this, int b2);
void E_vfunc_d(struct E *this, struct A* a, struct B *b);
void E_E_root(struct E *this);
#line 39
static void C_test_c(struct C *this)
{
	printf("%d %d\n", this->A->a1, this->c1);
}

static void D_test(struct D *this)
{
	C_test_c(&this->C);
	printf("%d %d %d %d\n", this->B.A.a1, this->B.b1, this->C.c1, this->d1);
}

char foo_func(struct foo *this)
{
	printf("%d\n", this->x + *this->y);
	this->x = *this->y;
	*this->y = this->x;
	this->y = &this->x;
	return 0;
}

static double foo_func_new(struct foo *this, int z)
{
	struct foo *x = foo_get_foo(x, 2), *x2;
	printf("%d\n", *this->y + *this->z);
	foo_func1(this, this->z);
	foo_func(x);
	foo_func1(x2, this->z);
	foo_func(((struct foo*)(3 + foo_get_foo(x, foo_func1(x2, this->z))) + 5));
	return 1.0;
}

float foo_func1(struct foo *this, void *p)
{
	return 1.0;
}

void foo_vfunc(struct foo *this, int arg1, float *arg2)
{
	*arg2 = foo_func1(this, &arg1);
}

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
	A_A(&this->A);
	A_A_root(&this->a_local);
}

void B_vfunc_a(struct B *this, int arg)
{
	printf("B::vfunc_a(%d), a1=%d, b1=%d\n", arg, this->A.a1, this->b1);
}

void B_vfunc_b(struct B *this, int arg)
{
	printf("B::vfunc_b(%d), a1=%d, b1=%d\n", arg, this->A.a1, this->b1);
}

void C_vfunc_a(struct C *this, int arg)
{
	printf("C::vfunc_a(%d), a1=%d, c1=%d\n", arg, this->A->a1, this->c1);
}

void C_vfunc_c(struct C *this, int arg)
{
	A_vfunc_a(this->A, arg);
	printf("C::vfunc_c(%d), a1=%d, c1=%d\n", arg, this->A->a1, this->c1);
}

void D_vfunc_a(struct D *this, int arg)
{
	printf("D::vfunc_a(%d), a1=%d, b1=%d c1=%d d1=%d\n", arg, this->B.A.a1, this->B.b1, this->C.c1, this->d1);
}

void D_vfunc_b(struct D *this, int arg)
{
	printf("D::vfunc_b(%d), a1=%d, b1=%d, d1=%d\n", arg, this->B.A.a1, this->B.b1, this->d1);
}

void D_vfunc_c(struct C *__this, int arg)
{	struct D *this = container_of(__this, struct D, C);

	printf("D::vfunc_c(%d), a1=%d, b1=%d c1=%d d1=%d\n", arg, this->B.A.a1, this->B.b1, this->C.c1, this->d1);
}

void D_vfunc_d(struct D *this, struct A *a, struct B *b)
{
	printf("D::vfunc_d(a, b), a1=%d, b1=%d c1=%d d1=%d a->a1=%d b->b1=%d\n", this->B.A.a1, this->B.b1, this->C.c1, this->d1, a->a1, b->b1);
}

void D_D(struct D *this, struct A *a)
{
	this->B.b1 = -5;
	B_B(&this->B);
}

static void E_E(struct E *this)
{
	this->D.B.A.a1 = -6;
	D_D(&this->D, &this->D.B.A);
}

void E_vfunc_b(struct E *this, int arg)
{
	printf("E::vfunc_b(%d), a1=%d, b1=%d d1=%d e1=%d\n", arg, this->D.B.A.a1, this->D.B.b1, this->D.d1, this->e1);
}

void E_vfunc_d(struct E *this, struct A *a, struct B *b)
{
	D_vfunc_d(&this->D, a, b);
	printf("E::vfunc_d(a, b), a1=%d, b1=%d c1=%d d1=%d e1=%d a->a1=%d b->b1=%d\n",
		this->D.B.A.a1, this->D.B.b1, this->D.C.c1, this->D.d1, this->e1, a->a1, b->b1);
}

int main(int argc, char **argv)
{
	struct A a, a_a;
	int aa1;
	struct B b;
	struct C_root c;
	struct D d; int db1;
	struct E e;

#line 163
	A_A_root(&a); A_A_root(&a_a);
	aa1 = a.a1;
	B_B_root(&b);
	C_C_root(&c);
	D_D_root(&d, &(&c)->A); db1 = d.B.b1;
	E_E_root(&e);
#line 170
	a.a1 = 10;
	A_vmt_vfunc_a(&a, 11);

	b.A.a1 = 20;
	b.b1 = 21;
	A_vmt_vfunc_a(&b.A, 22);
	B_vmt_vfunc_b(&b, 23);

	c.A.a1 = 30;
	c.C.c1 = 31;
	A_vmt_vfunc_a(&c.A, 32);
	C_vmt_vfunc_c(&c.C, 33);

	d.B.A.a1 = 40;
	d.B.b1 = 41;
	d.C.c1 = 42;
	d.d1 = 43;
	A_vmt_vfunc_a(&d.B.A, 44);
	B_vmt_vfunc_b(&d.B, 45);
	C_vmt_vfunc_c(&d.C, 46);
	D_vmt_vfunc_d(&d, &(&e)->D.B.A, &(&e)->D.B);

	e.D.B.A.a1 = 50;
	e.D.B.b1 = 51;
	e.D.C.c1 = 52;
	e.D.d1 = 53;
	e.e1 = 54;
	A_vmt_vfunc_a(&e.D.B.A, 55);
	B_vmt_vfunc_b(&e.D.B, 56);
	C_vmt_vfunc_c(&e.D.C, 57);
	D_vmt_vfunc_d(&e.D, &(&e)->D.B.A, &(&e)->D.B);

	/* silence warnings */
	printf("%d %d\n", aa1, db1);

	return 0;
}

struct foo_vmt foo_vmt = {
	foo_vfunc,
};

void foo_foo_root(struct foo *this)
{
	this->vmt = &foo_vmt;
}

void C_root_vfunc_a(struct A *__this, int a2)
{	struct C_root *this = container_of(__this, struct C_root, A);
	C_vfunc_a(&this->C, a2);
}

struct C_A_vmt C_A_vmt = {
	C_root_vfunc_a,
};

struct C_vmt C_vmt = {
	C_vfunc_c,
};

void C_C_root(struct C_root *this)
{
	this->C.A = &this->A;
	this->A.vmt = &C_A_vmt;
	this->C.vmt = &C_vmt;
	A_A(&this->A);
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

struct A_vmt A_vmt = {
	A_vfunc_a,
};

void A_A_root(struct A *this)
{
	this->vmt = &A_vmt;
	A_A(this);
}

struct E_vmt E_vmt = {
	D_vfunc_a,
	E_vfunc_b,
	E_vfunc_d,
};

void E_E_root(struct E *this)
{
	this->D.B.A.vmt = &E_vmt;
	this->D.C.vmt = &D_C_vmt;
	E_E(this);
}

struct D_vmt D_vmt = {
	D_vfunc_a,
	D_vfunc_b,
	D_vfunc_d,
};

struct D_C_vmt D_C_vmt = {
	D_vfunc_c,
};

void D_D_root(struct D *this, struct A *a)
{
	this->C.A = &this->B.A;
	this->B.A.vmt = &D_vmt;
	this->C.vmt = &D_C_vmt;
	D_D(this, a);
}
