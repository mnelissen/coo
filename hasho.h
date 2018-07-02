#ifndef _HASHO_H
#define _HASHO_H
#include <stddef.h>
#include <stdint.h>

struct hasho_entry {
	void *key;
	void *value;
};

typedef struct hasho {
	struct hasho_entry *entries;
	unsigned mask;
	int num_entries;
} hasho_t;

size_t ptrhash(void *ptr);

int hasho_init(hasho_t *table, unsigned size);
void hasho_clear(struct hasho *table);
void *hasho_find(struct hasho *table, void *key);
int hasho_insert(struct hasho *table, void *key, void *value);
struct hasho_entry *hasho_next(struct hasho *table, struct hasho_entry *entry);
int hasho_remove(struct hasho *table, void *value);
int hasho_destroy(struct hasho *table);

#define hasho_foreach(e, h) \
  for (e = NULL; (e = hasho_next(h, e)) != NULL;)

#endif
