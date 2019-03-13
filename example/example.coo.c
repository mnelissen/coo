#line 1 "example.coo"
#include <stdio.h>
#include <stdlib.h>
#include <foo.coo.h>

typedef struct A {
	int a1;
	const struct coo_vmt *vmt;
} A;

#line 12 "example.coo.c"
#include <coortl.h>
extern const struct A_vmt {
	struct coo_vmt vmt_base;
	void (*vfunc_a)(struct A *this, int a2);
} A_vmt;

extern const struct A_coo_class A_coo_class;

struct A *new_A(void);
void A_vfunc_a(struct A *this, int a2);
struct A *A_A(struct A *this);
struct A *A_A_root(struct A *this);
coo_inline void A_vmt_vfunc_a(struct A *this, int a2)
{
	((struct A_vmt*)this->vmt)->vfunc_a(this, a2);
}

#line 11 "example.coo"
struct B {
	struct A A;
	int b1;
	struct A a_local;
};

#line 37 "example.coo.c"
extern const struct B_vmt {
	struct coo_vmt vmt_base;
	void (*vfunc_a)(struct B *this, int a2);
	void (*vfunc_b)(struct B *this, int b2);
} B_vmt;

extern const struct B_coo_class B_coo_class;

struct B *new_B(void);
void B_vfunc_a(struct B *this, int a2);
void B_vfunc_b(struct B *this, int b2);
struct B *B_B(struct B *this);
struct B *B_B_root(struct B *this);
coo_inline void B_vmt_vfunc_b(struct B *this, int b2)
{
	((struct B_vmt*)this->A.vmt)->vfunc_b(this, b2);
}

#line 19 "example.coo"
struct C {
	struct A *A;
	int c1;
	const struct coo_vmt *vmt;
};

#line 63 "example.coo.c"
struct C_root {
	struct C C;
	struct A A;
};

extern const struct C_A_vmt {
	struct coo_vmt vmt_base;
	void (*vfunc_a)(struct A *this, int a2);
} C_A_vmt;

extern const struct C_vmt {
	struct coo_vmt vmt_base;
	void (*vfunc_c)(struct C *this, int c2);
	void (*vfunc_c3)(struct C *this, int c3);
} C_vmt;

extern const struct C_coo_class C_coo_class;

struct C *new_C(void);
void C_vfunc_a(struct C *this, int a2);
void C_vfunc_c(struct C *this, int c2);
void C_vfunc_c3(struct C *this, int c3);
struct C *C_C_root(struct C_root *this);
coo_inline void C_vmt_vfunc_c(struct C *this, int c2)
{
	((struct C_vmt*)this->vmt)->vfunc_c(this, c2);
}

coo_inline void C_vmt_vfunc_c3(struct C *this, int c3)
{
	((struct C_vmt*)this->vmt)->vfunc_c3(this, c3);
}

#line 26 "example.coo"
struct D {
	struct B B;
	struct C C;
	int d1;
};

#line 104 "example.coo.c"
extern const struct D_vmt {
	struct coo_vmt vmt_base;
	void (*vfunc_a)(struct D *this, int a2);
	void (*vfunc_b)(struct D *this, int b2);
	void (*vfunc_d)(struct D *this, struct A* a, struct B *b);
} D_vmt;

extern const struct D_C_vmt {
	struct coo_vmt vmt_base;
	void (*vfunc_c)(struct C *this, int c2);
	void (*vfunc_c3)(struct C *this, int c3);
} D_C_vmt;

extern const struct D_coo_class D_coo_class;

struct D *new_D(struct A *a);
void D_vfunc_a(struct D *this, int a2);
void D_vfunc_b(struct D *this, int b2);
void D_vfunc_c3(struct C *this, int c3);
struct D *D_D(struct D *this, struct A *a);
void D_vfunc_d(struct D *this, struct A* a, struct B *b);
struct D *D_D_root(struct D *this, struct A *a);
coo_inline void D_vmt_vfunc_d(struct D *this, struct A* a, struct B *b)
{
	((struct D_vmt*)this->B.A.vmt)->vfunc_d(this, a, b);
}

#line 35 "example.coo"
struct E {
	struct D D;
	int e1;
};

#line 138 "example.coo.c"
extern const struct E_vmt {
	struct coo_vmt vmt_base;
	void (*vfunc_a)(struct D *this, int a2);
	void (*vfunc_b)(struct E *this, int b2);
	void (*vfunc_d)(struct E *this, struct A* a, struct B *b);
} E_vmt;

extern const struct E_coo_class E_coo_class;

struct E *new_E(void);
void free_E(struct E *this);
void E_vfunc_b(struct E *this, int b2);
void E_vfunc_d(struct E *this, struct A* a, struct B *b);
struct E *E_E(struct E *this);
void E_d_E(struct E *this);
struct E *E_E_root(struct E *this);
#line 43 "example.coo"
static void C_test_c(struct C *this)
{
	printf("%d %d\n", this->A->a1, this->c1);
}

static void D_test(struct D *this)
{
	C_test_c(&this->C);
	printf("%d %d %d %d\n", this->B.A.a1, this->B.b1, this->C.c1, this->d1);
}

void * foo_get_foo(struct foo *this, int a)
{
	printf("get_foo a=%d\n", a);
	return this;
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

struct A *A_A(struct A *this)
{
	this->a1 = -1;
#line 206 "example.coo.c"
	return this;
#line 93 "example.coo"
}

void A_vfunc_a(struct A *this, int arg)
{
	printf("A::vfunc(%d), a1=%d\n", arg, this->a1);
}

struct B *B_B(struct B *this)
{
	A_A(&this->A);
	A_A_root(&this->a_local);
#line 220 "example.coo.c"
	return this;
#line 104 "example.coo"
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

void C_vfunc_c3(struct C *this, int arg)
{
	A_vfunc_a(this->A, arg);
	printf("C::vfunc_c3(%d), a1=%d, c1=%d\n", arg, this->A->a1, this->c1);
}

void D_vfunc_a(struct D *this, int arg)
{
	printf("D::vfunc_a(%d), a1=%d, b1=%d c1=%d d1=%d\n", arg, this->B.A.a1, this->B.b1, this->C.c1, this->d1);
}

void D_vfunc_b(struct D *this, int arg)
{
	printf("D::vfunc_b(%d), a1=%d, b1=%d, d1=%d\n", arg, this->B.A.a1, this->B.b1, this->d1);
}

void D_vfunc_c3(struct C *__this, int arg)
{	struct D *this = container_of(__this, struct D, C);
	printf("D::vfunc_c3(%d), a1=%d, b1=%d c1=%d d1=%d\n", arg, this->B.A.a1, this->B.b1, this->C.c1, this->d1);
}

void D_vfunc_d(struct D *this, struct A *a, struct B *b)
{
	printf("D::vfunc_d(a, b), a1=%d, b1=%d c1=%d d1=%d a->a1=%d b->b1=%d\n", this->B.A.a1, this->B.b1, this->C.c1, this->d1, a->a1, b->b1);
}

struct D *D_D(struct D *this, struct A *a)
{
	this->B.b1 = -5;
	B_B(&this->B);
#line 276 "example.coo.c"
	return this;
#line 157 "example.coo"
}

struct E *E_E(struct E *this)
{
	this->D.B.A.a1 = -6;
	D_D(&this->D, &this->D.B.A);
#line 285 "example.coo.c"
	return this;
#line 163 "example.coo"
}

void E_d_E(struct E *this)
{
	this->D.B.A.a1 = -7;
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
	struct E e, *p_e;

	int __coo_ret;
#line 184
	A_A_root(&a); A_A_root(&a_a);
	aa1 = a.a1;
	B_B_root(&b);
	C_C_root(&c);
	D_D_root(&d, &(&c)->A); db1 = d.B.b1;
	E_E_root(&e);
#line 191
	a.a1 = 10;
	A_vmt_vfunc_a(&a, 11);

	b.A.a1 = 20;
	b.b1 = 21;
	A_vmt_vfunc_a(&b.A, 22);
	B_vmt_vfunc_b(&b, 23);
	if (b.A.a1 < 10)
		{ __coo_ret = -1; goto __coo_out0; }

	struct E e2;
#line 201
	E_E_root(&e2);
#line 202
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
	D_vfunc_a(&e.D, 55);
	E_vfunc_b(&e, 56);
	C_vfunc_c(&e.D.C, 57);
	D_vfunc_c3(&e.D.C, 58);
	E_vfunc_d(&e, &(&e)->D.B.A, &(&e)->D.B);
	if (e.D.C.c1 < 33)
		{ __coo_ret = -2; goto __coo_out1; }

	D_vfunc_a(&(p_e = new_E())->D, -1);
	free_E(p_e);
	p_e = &e2;
	p_e->D.B.A.a1 = 60;
	p_e->D.B.b1 = 61;
	p_e->D.C.c1 = 62;
	p_e->D.d1 = 63;
	p_e->e1 = 64;
	D_vfunc_a(&p_e->D, 65);
	E_vfunc_b(p_e, 66);
	C_vfunc_c(&p_e->D.C, 67);
	E_vfunc_d(p_e, &(&e)->D.B.A, &(&e)->D.B);

	/* silence warnings */
	printf("%d %d\n", aa1, db1);

	__coo_ret = 0;
#line 384 "example.coo.c"
__coo_out1:
	E_d_E(&e2);
__coo_out0:
	E_d_E(&e);
	return __coo_ret;
#line 246 "example.coo"
}

#line 392 "example.coo.c"
#include <stddef.h>
#include <stdint.h>
#pragma pack(8)
const struct foo_coo_class {
	uint32_t num_parents;
} foo_coo_class = {
	0,
};

const struct foo_vmt foo_vmt = {
	{ offsetof(struct foo, vmt),
	  &foo_coo_class },
	foo_vfunc,
};

struct foo *new_foo(void)
{
	struct foo *this = malloc(sizeof(*this));
	if (this == NULL) return NULL;
	return foo_foo_root(this);
}

struct foo *foo_foo_root(struct foo *this)
{
	this->vmt = &foo_vmt.vmt_base;
	return this;
}

const struct C_coo_class {
	uint32_t num_parents;
	const void *parents[1];
} C_coo_class = {
	1,
	{ &A_coo_class }
};

void C_root_vfunc_a(struct A *__this, int a2)
{	struct C_root *this = container_of(__this, struct C_root, A);
	C_vfunc_a(&this->C, a2);
}

const struct C_A_vmt C_A_vmt = {
	{ offsetof(struct C_root, A.vmt),
	  &C_coo_class },
	C_root_vfunc_a,
};

const struct C_vmt C_vmt = {
	{ offsetof(struct C, vmt),
	  &C_coo_class },
	C_vfunc_c,
	C_vfunc_c3,
};

struct C *new_C(void)
{
	struct C_root *this = malloc(sizeof(*this));
	if (this == NULL) return NULL;
	C_C_root(this);
	return &this->C;
}

struct C *C_C_root(struct C_root *this)
{
	this->C.A = &this->A;
	this->A.vmt = &C_A_vmt.vmt_base;
	this->C.vmt = &C_vmt.vmt_base;
	A_A(this->C.A);
	return &this->C;
}

const struct B_coo_class {
	uint32_t num_parents;
	const void *parents[1];
} B_coo_class = {
	1,
	{ &A_coo_class }
};

const struct B_vmt B_vmt = {
	{ offsetof(struct B, A.vmt),
	  &B_coo_class },
	B_vfunc_a,
	B_vfunc_b,
};

struct B *new_B(void)
{
	struct B *this = malloc(sizeof(*this));
	if (this == NULL) return NULL;
	return B_B_root(this);
}

struct B *B_B_root(struct B *this)
{
	this->A.vmt = &B_vmt.vmt_base;
	return B_B(this);
}

const struct A_coo_class {
	uint32_t num_parents;
} A_coo_class = {
	0,
};

const struct A_vmt A_vmt = {
	{ offsetof(struct A, vmt),
	  &A_coo_class },
	A_vfunc_a,
};

struct A *new_A(void)
{
	struct A *this = malloc(sizeof(*this));
	if (this == NULL) return NULL;
	return A_A_root(this);
}

struct A *A_A_root(struct A *this)
{
	this->vmt = &A_vmt.vmt_base;
	return A_A(this);
}

const struct E_coo_class {
	uint32_t num_parents;
	const void *parents[1];
} E_coo_class = {
	1,
	{ &D_coo_class }
};

const struct E_vmt E_vmt = {
	{ offsetof(struct E, D.B.A.vmt),
	  &E_coo_class },
	D_vfunc_a,
	E_vfunc_b,
	E_vfunc_d,
};

struct E *new_E(void)
{
	struct E *this = malloc(sizeof(*this));
	if (this == NULL) return NULL;
	return E_E_root(this);
}

struct E *E_E_root(struct E *this)
{
	this->D.C.A = &this->D.B.A;
	this->D.B.A.vmt = &E_vmt.vmt_base;
	this->D.C.vmt = &D_C_vmt.vmt_base;
	return E_E(this);
}

void free_E(struct E *this)
{
	E_d_E(this);
	free(this);
}

const struct D_coo_class {
	uint32_t num_parents;
	uint32_t offsets[1];
	const void *parents[2];
} D_coo_class = {
	2,
	{ offsetof(struct D, C) },
	{ &B_coo_class,
	  &C_coo_class }
};

const struct D_vmt D_vmt = {
	{ offsetof(struct D, B.A.vmt),
	  &D_coo_class },
	D_vfunc_a,
	D_vfunc_b,
	D_vfunc_d,
};

const struct D_C_vmt D_C_vmt = {
	{ offsetof(struct D, C.vmt),
	  &D_coo_class },
	C_vfunc_c,
	D_vfunc_c3,
};

struct D *new_D(struct A *a)
{
	struct D *this = malloc(sizeof(*this));
	if (this == NULL) return NULL;
	return D_D_root(this, a);
}

struct D *D_D_root(struct D *this, struct A *a)
{
	this->C.A = &this->B.A;
	this->B.A.vmt = &D_vmt.vmt_base;
	this->C.vmt = &D_C_vmt.vmt_base;
	return D_D(this, a);
}
