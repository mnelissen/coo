/* open addressing hash table implementation */
#ifndef _HASHO_H
#define _HASHO_H
#include <stddef.h>
#include <stdint.h>

#define hasho_insert_exists(tbl, key, val, resvar) \
        (resvar = hasho_insert(tbl, key, val)) != NULL && (int)(size_t)resvar != -1

struct hasho_entry {
	void *key;
	void *value;
};

typedef struct hasho {
	struct hasho_entry *entries;
	unsigned mask;
	unsigned num_entries;
} hasho_t;

size_t ptrhash(void *ptr);

int hasho_init(hasho_t *table, unsigned size);
void hasho_clear(struct hasho *table);
void *hasho_find(struct hasho *table, void *key);
struct hasho_entry *hasho_insert(struct hasho *table, void *key, void *value);
struct hasho_entry *hasho_next(struct hasho *table, struct hasho_entry *entry);
int hasho_remove(struct hasho *table, void *value);
int hasho_destroy(struct hasho *table);

#define hasho_foreach(e, h) \
  for (e = NULL; (e = hasho_next(h, e)) != NULL;)

#endif
