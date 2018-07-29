#include <stdio.h>
#include <foo.coo.h>

struct A {
	int a1;
	void *vmt;
};

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
coo_inline void A_vmt_vfunc_a(struct A *this, int a2)
{
	((struct A_vmt*)this->vmt)->vfunc_a(this, a2);
}

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
coo_inline void B_vmt_vfunc_b(struct B *this, int b2)
{
	((struct B_vmt*)this->A.vmt)->vfunc_b(this, b2);
}

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
coo_inline void C_vmt_vfunc_c(struct C *this, int c2)
{
	((struct C_vmt*)this->vmt)->vfunc_c(this, c2);
}

struct D {
	struct B B;
	struct C C;
	int d1;
};

extern struct D_vmt {
	void (*vfunc_a)(struct D *D, int a2);
	void (*vfunc_b)(struct D *D, int b2);
	void (*vfunc_d)(struct D *D, A* a, B *b);
} D_vmt;

extern struct D_C_vmt {
	void (*vfunc_c)(struct D *D, int c2);
} D_C_vmt;

void D_vfunc_a(struct D *this, int a2);
void D_vfunc_b(struct D *this, int b2);
void D_vfunc_c(struct D *this, int c2);
void D_vfunc_d(struct D *this, A* a, B *b);
coo_inline void D_vmt_vfunc_d(struct D *this, A* a, B *b)
{
	((struct D_vmt*)this->B.A.vmt)->vfunc_d(this, a, b);
}

struct E {
	struct D D;
	int e1;
};

extern struct E_vmt {
	void (*vfunc_a)(struct D *D, int a2);
	void (*vfunc_b)(struct E *E, int b2);
	void (*vfunc_d)(struct E *E, A* a, B *b);
} E_vmt;

void E_vfunc_b(struct E *this, int b2);
void E_vfunc_d(struct E *this, A* a, B *b);
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
}

static double foo_func_new(struct foo *this, int z)
{
	struct foo *x = get_x(), *x2;
	printf("%d\n", *this->y + z);
	foo_func1(this, z);
	foo_func(x);
	foo_func1(x2, z);
	((struct foo*)(3 + foo_get_foo(x, foo_func1(x2, z))) + 5)->func();
}

void foo_vfunc(struct foo *this, int arg1, int arg2)
{
	int b = foo_func1(this, arg1);
}

void A_vfunc_a(struct A *this, int a2)
{
	printf("A::vfunc(%d)\n", a2);
}

void B_vfunc_a(struct B *this)
{
	printf("B::vfunc_a(), b1=%d\n", this->b1);
}

void B_vfunc_b(struct B *this, int b2)
{
	printf("B::vfunc_b(), b1=%d, b2=%d\n", this->b1, b2);
}

void C_vfunc_a(struct C *this, int a2)
{
	printf("C::vfunc_a(), a1=%d a2=%d b1=%d c1=%d\n", this->A->a1, a2, b1, this->c1);
}

void C_vfunc_c(struct C *this, int c2)
{
	printf("C::vfunc_c(), b1=%d c1=%d c2=%d\n", b1, this->c1, c2);
}

void D_vfunc_a(struct D *this, int a2)
{
	printf("D::vfunc_a(), a2=%d b1=%d c1=%d d1=%d\n", a2, this->B.b1, this->C.c1, this->d1);
}

void D_vfunc_b(struct D *this, int b2)
{
	printf("D::vfunc_b(), b1=%d, b2=%d, d1=%\n", this->B.b1, b2, this->d1);
}

void D_vfunc_c(struct D *this, int c2)
{
	printf("D::vfunc_c(), b1=%d c1=%d c2=%d d1=%d\n", this->B.b1, this->C.c1, c2, this->d1);
}

void D_vfunc_d(struct D *this, int d2, int d3)
{
	printf("D::vfunc_d(), b1=%d c1=%d d1=%d d2=%d d3=%d\n", this->B.b1, this->C.c1, this->d1, d2, d3);
}

void E_vfunc_b(struct E *this, int b2)
{
	printf("E::vfunc_b(), b1=%d b2=%d d1=% e1=%d\n", this->D.B.b1, b2, this->D.d1, this->e1);
}

void E_vfunc_d(struct E *this, int d2, int d3)
{
	printf("E::vfunc_d(), b1=%d c1=%d d1=%d d2=%d d3=%d e1=%d\n", this->D.B.b1, this->D.C.c1, this->D.d1, d2, d3, this->e1);
}

int main(int argc, char **argv)
{
	A a;
	B b;
	C c;
	D d;
	E e;

	a.a1 = 10;
	a.vfunc_a(11);

	&(b.a1 ).A= 20;
	b.vfunc_a(21);
	b.vfunc_b(22);

	c.a1 = 30;
	c.c1 = 31;
	c.vfunc_a(32);
	c.vfunc_c(33);

	&(d.a1 ).C= 40;
	d.b1 = 41;
	d.c1 = 42;
	d.d1 = 43;
	d.vfunc_a(44);
	d.vfunc_b(45);
	d.vfunc_c(46);
	&(d.vfunc_d(&e, &e)).D;

	&(e.a1 ).D= 50;
	e.b1 = 51;
	e.c1 = 52;
	e.d1 = 53;
	e.e1 = 54;
	e.vfunc_a(55);
	e.vfunc_b(56);
	e.vfunc_c(57);
	e.vfunc_d(&e, &e);

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
