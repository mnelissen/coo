#line 1 "example.coo"
#include <stdio.h>
#include <stdlib.h>
#include <foo.coo.h>

struct A {
	int a1;
	const struct coo_vmt *vmt;
};

#line 12 "example.coo.c"
#include <coortl.h>
extern const struct A_vmt {
	struct coo_vmt vmt_base;
	void (*vfunc_a)(struct A *this, int a2);
	void (*d_A)(struct A *this);
} A_vmt;

extern const struct A_coo_class A_coo_class;

struct A *new_A(void);
void A_A_root_zi(struct A *this);
void free_A(struct A *this);
void A_vfunc_a(struct A *this, int a2);
struct A *A_A(struct A *this);
void A_d_A(struct A *this);
struct A *A_A_root(struct A *this);
#line 12 "example.coo"
struct B {
	struct A A;
#line 15 "example.coo"
	int b1;
	struct A a_local;
};

#line 37 "example.coo.c"
extern const struct B_vmt {
	struct coo_vmt vmt_base;
	void (*vfunc_a)(struct B *this, int a2);
	void (*d_B)(struct B *this);
	void (*vfunc_b)(struct B *this, int b2);
} B_vmt;

extern const struct B_coo_class B_coo_class;

struct B *new_B(void);
void B_B_root_zi(struct B *this);
#define free_B(this) free_A(&(this)->A)
void B_vfunc_a(struct B *this, int a2);
void B_d_B(struct B *this);
void B_vfunc_b(struct B *this, int b2);
struct B *B_B(struct B *this);
struct B *B_B_root(struct B *this);
#line 20 "example.coo"
struct C {
	struct A *A;
	int c1;
	const struct coo_vmt *vmt;
};

#line 62 "example.coo.c"
struct C_root {
	struct C C;
	struct A A;
};
extern const struct C_A_vmt {
	struct coo_vmt vmt_base;
	void (*vfunc_a)(struct A *this, int a2);
	void (*d_A)(struct A *this);
} C_A_vmt;

extern const struct C_vmt {
	struct coo_vmt vmt_base;
	void (*vfunc_c)(struct C *this, int c2);
	void (*vfunc_c3)(struct C *this, int c3);
} C_vmt;

extern const struct C_coo_class C_coo_class;

struct C *new_C(void);
void C_C_root_zi(struct C_root *this);
#define free_C(this) free_A((this)->A)
void C_vfunc_a(struct C *this, int a2);
void C_vfunc_c(struct C *this, int c2);
void C_vfunc_c3(struct C *this, int c3);
struct C *C_C_root(struct C_root *this);
void C_d_C_root(struct C *this);
#line 27 "example.coo"
struct D {
	struct B B;
	struct C C;
#line 29 "example.coo"
	int d1;
};

#line 97 "example.coo.c"
extern const struct D_vmt {
	struct coo_vmt vmt_base;
	void (*vfunc_a)(struct D *this, int a2);
	void (*d_B)(struct B *this);
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
void D_D_root_zi(struct D *this, struct A *a);
#define free_D(this) free_A(&(this)->B.A)
void D_vfunc_a(struct D *this, int a2);
void D_vfunc_b(struct D *this, int b2);
void D_vfunc_c3(struct C *this, int c3);
struct D *D_D(struct D *this, struct A *a);
void D_vfunc_d(struct D *this, struct A* a, struct B *b);
struct D *D_D_root(struct D *this, struct A *a);
void D_d_D_root(struct D *this);
#line 36 "example.coo"
struct E {
	struct D D;
	int e1;
};

#line 130 "example.coo.c"
extern const struct E_vmt {
	struct coo_vmt vmt_base;
	void (*vfunc_a)(struct D *this, int a2);
	void (*d_E)(struct E *this);
	void (*vfunc_b)(struct E *this, int b2);
	void (*vfunc_d)(struct E *this, struct A* a, struct B *b);
} E_vmt;

extern const struct E_coo_class E_coo_class;

struct E *new_E(void);
void E_E_root_zi(struct E *this);
#define free_E(this) free_A(&(this)->D.B.A)
void E_d_E(struct E *this);
void E_vfunc_b(struct E *this, int b2);
void E_vfunc_d(struct E *this, struct A* a, struct B *b);
struct E *E_E(struct E *this);
struct E *E_E_root(struct E *this);
void E_d_E_root(struct E *this);
#line 44 "example.coo"
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
	char ch_a = 'a';
	printf("get_foo %c=%d\n", ch_a, a);
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

static double foo_func_new(struct foo *this, struct foo *in, int z1)
{
	struct foo *x = foo_get_foo(in, 2), *x2 = x;
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
#line 202 "example.coo.c"
	return this;
#line 95 "example.coo"
}

void A_d_A(struct A *this)
{
	printf("A::-A() a1=%d\n", this->a1);
}

void A_vfunc_a(struct A *this, int arg)
{
	printf("A::vfunc(%d), a1=%d\n", arg, this->a1);
}

struct B *B_B(struct B *this)
{
	A_A(&this->A);
	A_A_root(&this->a_local);
#line 221 "example.coo.c"
	return this;
#line 111 "example.coo"
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
#line 277 "example.coo.c"
	return this;
#line 164 "example.coo"
}

struct E *E_E(struct E *this)
{
	this->D.B.A.a1 = -6;
	D_D(&this->D, &this->D.B.A);
#line 286 "example.coo.c"
	return this;
#line 170 "example.coo"
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
	struct foo foo;

	int __coo_ret;
#line 191
	A_A_root_zi(&a), A_A_root_zi(&a_a);
	aa1 = a.a1;
	B_B_root_zi(&b);
	C_C_root(&c);
	D_D_root_zi(&d, &(&c)->A); db1 = d.B.b1;
	E_E_root_zi(&e),
	foo_foo_root_zi(&foo);
#line 199
	a.a1 = 10;
	((struct A_vmt*)a.vmt)->vfunc_a(&a, 11);

	b.A.a1 = 20;
	b.b1 = 21;
	((struct A_vmt*)b.A.vmt)->vfunc_a(&b.A, 22);
	((struct B_vmt*)b.A.vmt)->vfunc_b(&b, 23);
	if (b.A.a1 < 10)
		{ __coo_ret = -1; goto __coo_out0; }

	struct E e2;
#line 209
	E_E_root_zi(&e2);
#line 210
	c.A.a1 = 30;
	c.C.c1 = 31;
	((struct A_vmt*)c.A.vmt)->vfunc_a(&c.A, 32);
	((struct C_vmt*)c.C.vmt)->vfunc_c(&c.C, 33);

	d.B.A.a1 = 40;
	d.B.b1 = 41;
	d.C.c1 = 42;
	d.d1 = 43;
	((struct A_vmt*)d.B.A.vmt)->vfunc_a(&d.B.A, 44);
	((struct B_vmt*)d.B.A.vmt)->vfunc_b(&d.B, 45);
	((struct C_vmt*)d.C.vmt)->vfunc_c(&d.C, 46);
	((struct D_vmt*)d.B.A.vmt)->vfunc_d(&d, &(&e)->D.B.A, &(&e)->D.B);
	D_test(&d);

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

	foo_func_new(&foo, &foo, 3);

	/* silence warnings */
	printf("%d %d\n", aa1, db1);

	__coo_ret = 0;
#line 389 "example.coo.c"
__coo_out1:
	E_d_E(&e2);
__coo_out0:
	E_d_E(&e);
	B_d_B(&d.B);
	A_d_A(&c.A);
	B_d_B(&b);
	A_d_A(&a_a);
	A_d_A(&a);
	return __coo_ret;
#line 257 "example.coo"
}

#line 403 "example.coo.c"
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
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
	struct foo *this = calloc(1, sizeof(*this));
	if (this == NULL) return NULL;
	if (!foo_foo_root(this)) return free(this), NULL;
	return this;
}

void foo_foo_root_zi(struct foo *this)
{
	memset(this, 0, sizeof(*this));
	foo_foo_root(this);
}

struct foo *foo_foo_root(struct foo *this)
{
	this->vmt = &foo_vmt.vmt_base;
	return this;
}

const struct A_coo_class {
	uint32_t num_parents;
} A_coo_class = {
	0,
};

static void destroy_A(struct A *this)
{
	A_d_A(this);
	free(this);
}

const struct A_vmt A_vmt = {
	{ offsetof(struct A, vmt),
	  &A_coo_class },
	A_vfunc_a,
	destroy_A,
};

struct A *new_A(void)
{
	struct A *this = calloc(1, sizeof(*this));
	if (this == NULL) return NULL;
	if (!A_A_root(this)) return free(this), NULL;
	return this;
}

void A_A_root_zi(struct A *this)
{
	memset(this, 0, sizeof(*this));
	A_A_root(this);
}

struct A *A_A_root(struct A *this)
{
	this->vmt = &A_vmt.vmt_base;
	return A_A(this);
}

void free_A(struct A *this)
{
	if (this == NULL)
		return;
	((struct A_vmt*)this->vmt)->d_A(this);
}

const struct B_coo_class {
	uint32_t num_parents;
	const void *parents[1];
} B_coo_class = {
	1,
	{ &A_coo_class }
};

static void destroy_B(struct B *this)
{
	B_d_B(this);
	free(this);
}

const struct B_vmt B_vmt = {
	{ offsetof(struct B, A.vmt),
	  &B_coo_class },
	B_vfunc_a,
	destroy_B,
	B_vfunc_b,
};

struct B *new_B(void)
{
	struct B *this = calloc(1, sizeof(*this));
	if (this == NULL) return NULL;
	if (!B_B_root(this)) return free(this), NULL;
	return this;
}

void B_B_root_zi(struct B *this)
{
	memset(this, 0, sizeof(*this));
	B_B_root(this);
}

struct B *B_B_root(struct B *this)
{
	this->A.vmt = &B_vmt.vmt_base;
	return B_B(this);
}

void B_d_B(struct B *this)
{
	A_d_A(&this->a_local);
	A_d_A(&this->A);
}

const struct C_coo_class {
	uint32_t num_parents;
	const void *parents[1];
} C_coo_class = {
	1,
	{ &A_coo_class }
};

static void C_root_vfunc_a(struct A *__this, int a2)
{	struct C_root *this = container_of(__this, struct C_root, A);
	C_vfunc_a(&this->C, a2);
}

static void destroy_A_C(struct A *__this)
{	struct C_root *this = container_of(__this, struct C_root, A);
	destroy_A(&this->A);
}

const struct C_A_vmt C_A_vmt = {
	{ offsetof(struct C_root, A.vmt),
	  &C_coo_class },
	C_root_vfunc_a,
	destroy_A_C,
};

const struct C_vmt C_vmt = {
	{ offsetof(struct C_root, C.vmt),
	  &C_coo_class },
	C_vfunc_c,
	C_vfunc_c3,
};

struct C *new_C(void)
{
	struct C_root *this = calloc(1, sizeof(*this));
	if (this == NULL) return NULL;
	if (!C_C_root(this)) return free(this), NULL;
	return &this->C;
}

void C_C_root_zi(struct C_root *this)
{
	memset(this, 0, sizeof(*this));
	C_C_root(this);
}

struct C *C_C_root(struct C_root *this)
{
	this->C.A = &this->A;
	this->A.vmt = &A_vmt.vmt_base;
	this->C.vmt = &C_vmt.vmt_base;
	if (!A_A(this->C.A))
		return NULL;
	return &this->C;
}

void C_d_C_root(struct C *__this)
{
	struct C_root *this = container_of(__this, struct C_root, C);
	A_d_A(&this->A);
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
	destroy_B,
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
	struct D *this = calloc(1, sizeof(*this));
	if (this == NULL) return NULL;
	if (!D_D_root(this, a)) return free(this), NULL;
	return this;
}

void D_D_root_zi(struct D *this, struct A *a)
{
	memset(this, 0, sizeof(*this));
	D_D_root(this, a);
}

struct D *D_D_root(struct D *this, struct A *a)
{
	this->C.A = &this->B.A;
	this->B.A.vmt = &D_vmt.vmt_base;
	this->C.vmt = &D_C_vmt.vmt_base;
	return D_D(this, a);
}

void D_d_D_root(struct D *this)
{
	A_d_A(&this->B.A);
}

const struct E_coo_class {
	uint32_t num_parents;
	const void *parents[1];
} E_coo_class = {
	1,
	{ &D_coo_class }
};

static void destroy_E(struct E *this)
{
	E_d_E(this);
	free(this);
}

const struct E_vmt E_vmt = {
	{ offsetof(struct E, D.B.A.vmt),
	  &E_coo_class },
	D_vfunc_a,
	destroy_E,
	E_vfunc_b,
	E_vfunc_d,
};

struct E *new_E(void)
{
	struct E *this = calloc(1, sizeof(*this));
	if (this == NULL) return NULL;
	if (!E_E_root(this)) return free(this), NULL;
	return this;
}

void E_E_root_zi(struct E *this)
{
	memset(this, 0, sizeof(*this));
	E_E_root(this);
}

struct E *E_E_root(struct E *this)
{
	this->D.C.A = &this->D.B.A;
	this->D.B.A.vmt = &E_vmt.vmt_base;
	this->D.C.vmt = &D_C_vmt.vmt_base;
	return E_E(this);
}

void E_d_E_root(struct E *this)
{
	A_d_A(&this->D.B.A);
}
