#line 1 "inc/bar/bar.hoo"
struct bar {
	int a, b;
	struct coo_vmt *vmt;
};

#line 8 "inc/bar/bar.coo.h"
#include <coortl.h>
extern struct bar_vmt {
	struct coo_vmt vmt_base;
	void (*vbarf)(struct bar *this);
} bar_vmt;

extern const struct bar_coo_class bar_coo_class;

struct bar *new_bar(void);
int *bar_barf(struct bar *this, char a, int b);
void *bar_get_bar(struct bar *this, int a);
void bar_vbarf(struct bar *this);
struct bar *bar_bar_root(struct bar *this);
coo_inline void bar_vmt_vbarf(struct bar *this)
{
	((struct bar_vmt*)this->vmt)->vbarf(this);
}

#line 7 "inc/bar/bar.hoo"
