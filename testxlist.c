#include <stdio.h>
#include <stdlib.h>
#include "list.h"

struct item {
	xlist_item_t link;
	int x;
};

typedef xlist(struct item) item_list_t;

static void print_list(item_list_t *list)
{
	struct item *prev, *item, *temp;

	printf("fwd:");
	xlist_foreach(item, prev, temp, list, link)
		printf(" %d", item->x);
	printf("  rev:");
	xlist_foreach_rev(item, prev, temp, list, link)
		printf(" %d", item->x);
	printf("\n");
}

static struct item *create_item(int x)
{
	struct item *temp = malloc(sizeof *temp);
	return temp->x = x, temp;
}

int main(void)
{
	struct item *prev, *item, *temp, *new;
	item_list_t list;
	int i;

	xlist_init(&list, link);
	print_list(&list);
	new = create_item(2);
	xlist_add_head(&list, new, link);
	print_list(&list);
	new = create_item(4);
	xlist_add_tail(&list, new, link);
	print_list(&list);
	xlist_foreach(item, prev, temp, &list, link) {
		new = create_item(1);
		xlist_insert_before(&list, prev, item, new, link);
		break;
	}
	print_list(&list);
	i = 0;
	xlist_foreach(item, prev, temp, &list, link) {
		if (i++ == 2) {
			new = create_item(3);
			xlist_insert_before(&list, prev, item, new, link);
			break;
		}
	}
	print_list(&list);
	i = 0;
	xlist_foreach(item, prev, temp, &list, link) {
		if (i++ == 3) {
			new = create_item(5);
			xlist_insert_after(&list, prev, item, temp, new, link);
			break;
		}
	}
	print_list(&list);
}
