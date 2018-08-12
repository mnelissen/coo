struct bar {
	int a, b;
	void *vmt;
};

extern struct bar_vmt {
	void (*vbarf)(struct bar *bar);
} bar_vmt;

#ifndef coo_inline
#ifdef _MSC_VER
#define coo_inline __forceinline
#else
#define coo_inline extern inline __attribute__((always_inline)) __attribute__((gnu_inline))
#endif
#endif

int *bar_barf(struct bar *this, char a, int b);
void *bar_get_bar(struct bar *this, int a);
void bar_vbarf(struct bar *this);
coo_inline void bar_vmt_vbarf(struct bar *this)
{
	((struct bar_vmt*)this->vmt)->vbarf(this);
}
