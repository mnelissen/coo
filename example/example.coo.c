#line 1 "example.coo"
#include <stdio.h>
#include <foo.coo.h>

typedef struct A {
	int a1;
	void *vmt;
} A;

extern struct A_vmt {
	void (*vfunc_a)(struct A *A, int a2);
} A_vmt;

#ifndef coo_inline
#ifdef _MSC_VER
#define coo_inline __forceinline
#else
#define coo_inline extern inline __attribute__((always_inline)) __attribute__((gnu_inline))
#endif
#endif

void A_vfunc_a(struct A *this, int a2);
void A_A(struct A *this);
coo_inline void A_vmt_vfunc_a(struct A *this, int a2)
{
	((struct A_vmt*)this->vmt)->vfunc_a(this, a2);
}

#line 9
struct B {
	struct A A;
	int b1;
};

extern struct B_vmt {
	void (*vfunc_a)(struct B *B, int a2);
	void (*vfunc_b)(struct B *B, int b2);
} B_vmt;

void B_vfunc_a(struct B *this, int a2);
void B_vfunc_b(struct B *this, int b2);
void B_B(struct B *this);
coo_inline void B_vmt_vfunc_b(struct B *this, int b2)
{
	((struct B_vmt*)this->A.vmt)->vfunc_b(this, b2);
}

#line 15
struct C {
	struct A *A;
	int c1;
	void *vmt;
};

extern struct C_A_vmt {
	void (*vfunc_a)(struct C *C, int a2);
} C_A_vmt;

extern struct C_vmt {
	void (*vfunc_c)(struct C *C, int c2);
} C_vmt;

void C_vfunc_a(struct C *this, int a2);
void C_vfunc_c(struct C *this, int c2);
void C_C(struct C *this);
coo_inline void C_vmt_vfunc_c(struct C *this, int c2)
{
	((struct C_vmt*)this->vmt)->vfunc_c(this, c2);
}

#line 21
struct D {
	struct B B;
	struct C C;
	int d1;
};

extern struct D_vmt {
	void (*vfunc_a)(struct D *D, int a2);
	void (*vfunc_b)(struct D *D, int b2);
	void (*vfunc_d)(struct D *D, struct A* a, struct B *b);
} D_vmt;

extern struct D_C_vmt {
	void (*vfunc_c)(struct D *D, int c2);
} D_C_vmt;

void D_vfunc_a(struct D *this, int a2);
void D_vfunc_b(struct D *this, int b2);
void D_vfunc_c(struct D *this, int c2);
void D_D(struct D *this, struct A *a);
void D_vfunc_d(struct D *this, struct A* a, struct B *b);
coo_inline void D_vmt_vfunc_d(struct D *this, struct A* a, struct B *b)
{
	((struct D_vmt*)this->B.A.vmt)->vfunc_d(this, a, b);
}

#line 30
struct E {
	struct D D;
	int e1;
};

extern struct E_vmt {
	void (*vfunc_a)(struct D *D, int a2);
	void (*vfunc_b)(struct E *E, int b2);
	void (*vfunc_d)(struct E *E, struct A* a, struct B *b);
} E_vmt;

void E_vfunc_b(struct E *this, int b2);
void E_vfunc_d(struct E *this, struct A* a, struct B *b);
void E_E(struct E *this);
#line 36
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

void foo_vfunc(struct foo *this, int arg1, float *arg2)
{
	*arg2 = foo_func1(this, &arg1);
}

void A_vfunc_a(struct A *this, int a2)
{
	printf("A::vfunc(%d)\n", a2);
}

void B_vfunc_a(struct B *this, int a2)
{
	printf("B::vfunc_a(), a2=%d, b1=%d\n", a2, this->b1);
}

void B_vfunc_b(struct B *this, int b2)
{
	printf("B::vfunc_b(), b1=%d, b2=%d\n", this->b1, b2);
}

void C_vfunc_a(struct C *this, int a2)
{
	printf("C::vfunc_a(), a1=%d a2=%d c1=%d\n", this->A->a1, a2, this->c1);
}

void C_vfunc_c(struct C *this, int c2)
{
	printf("C::vfunc_c(), c1=%d c2=%d\n", this->c1, c2);
}

void D_vfunc_a(struct D *this, int a2)
{
	printf("D::vfunc_a(), a2=%d b1=%d c1=%d d1=%d\n", a2, this->B.b1, this->C.c1, this->d1);
}

void D_vfunc_b(struct D *this, int b2)
{
	printf("D::vfunc_b(), b1=%d, b2=%d, d1=%d\n", this->B.b1, b2, this->d1);
}

void D_vfunc_c(struct D *this, int c2)
{
	printf("D::vfunc_c(), b1=%d c1=%d c2=%d d1=%d\n", this->B.b1, this->C.c1, c2, this->d1);
}

void D_vfunc_d(struct D *this, struct A *a, struct B *b)
{
	printf("D::vfunc_d(), b1=%d c1=%d d1=%d a.a1=%d b.b1=%d\n", this->B.b1, this->C.c1, this->d1, a->a1, b->b1);
}

void D_D(struct D *this, struct A *a)
{
	this->B.A.a1 = -5;
	B_B(&this->B);
	C_C(&this->C);
	this->B.A.vmt = &D_vmt;
	this->C.vmt = &D_C_vmt;
#line 122
}

void E_E(struct E *this)
{
	this->D.B.A.a1 = -6;
	D_D(&this->D, &this->D.B.A);
	this->D.B.A.vmt = &E_vmt;
	this->D.C.vmt = &D_C_vmt;
#line 128
}

void E_vfunc_b(struct E *this, int b2)
{
	printf("E::vfunc_b(), b1=%d b2=%d d1=%d e1=%d\n", this->D.B.b1, b2, this->D.d1, this->e1);
}

void E_vfunc_d(struct E *this, struct A *a, struct B *b)
{
	printf("E::vfunc_d(), b1=%d c1=%d d1=%d e1=%d a.a1=%d b.b1=%d\n", this->D.B.b1, this->D.C.c1, this->D.d1, this->e1, a->a1, b->b1);
}

int main(int argc, char **argv)
{
	struct A a, a_a;
	int aa1;
	struct B b;
	int ba1;
	struct C c;
	struct D d; int db1;
	struct E e;

#line 142
	A_A(&a); A_A(&a_a);
	aa1 = a.a1;
	B_B(&b);
	ba1 = b.A.a1;
	C_C(&c);
	D_D(&d, (&c)->A); db1 = d.B.b1;
	E_E(&e);
#line 150
	a.a1 = 10;
	A_vmt_vfunc_a(&a, 11);

	b.A.a1 = 20;
	A_vmt_vfunc_a(&b.A, 21);
	B_vmt_vfunc_b(&b, 22);

	c.A->a1 = 30;
	c.c1 = 31;
	A_vmt_vfunc_a(c.A, 32);
	C_vmt_vfunc_c(&c, 33);

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
	printf("%d %d %d\n", aa1, ba1, db1);

	return 0;
}
struct foo_vmt foo_vmt = {
	foo_vfunc,
};

struct C_A_vmt C_A_vmt = {
	C_vfunc_a,
};

struct C_vmt C_vmt = {
	C_vfunc_c,
};

struct B_vmt B_vmt = {
	B_vfunc_a,
	B_vfunc_b,
};

struct A_vmt A_vmt = {
	A_vfunc_a,
};

struct E_vmt E_vmt = {
	D_vfunc_a,
	E_vfunc_b,
	E_vfunc_d,
};

struct D_vmt D_vmt = {
	D_vfunc_a,
	D_vfunc_b,
	D_vfunc_d,
};

struct D_C_vmt D_C_vmt = {
	D_vfunc_c,
};
