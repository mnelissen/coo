#ifndef _HASH_H
#define _HASH_H
#include <stddef.h>
#include <stdint.h>

typedef struct hash_entry {
	struct hash_entry *next;
        size_t hash;    /* cache to speed up compare and resize */
} hash_entry_t;

/* given 2 pointers to user structures, return 0 = equal, !0 = not equal */
typedef int (*hash_cmp_cb)(void *a, void *b);

typedef struct hash {
	struct hash_entry **entries;
        hash_cmp_cb compare;
	unsigned mask;
	int num_entries;
        int offset;       /* offset from userdata to struct hash_entry */
} hash_t;

uint32_t uint32hash(uint32_t key);
size_t uint64hash(uint64_t key);
size_t ptrhash(void *ptr);
size_t strhash(const char *str);
size_t memhash(const void *mem, size_t size);

int hash_init(hash_t *table, hash_cmp_cb compare, unsigned size, int offset);
void hash_clear(struct hash *table);
void *hash_find(struct hash *table, size_t hash, struct hash_entry *key);
void *hash_find_custom(struct hash *table, size_t hash, hash_cmp_cb compare, void *key);
int hash_insert(struct hash *table, struct hash_entry *entry, size_t hash);
void *hash_next(struct hash *table, void *user);
int hash_remove(struct hash *table, struct hash_entry *entry);
int hash_destroy(struct hash *table);

#define hash_foreach(u, h) \
  for (u = NULL; (u = hash_next(h, u)) != NULL;)

#endif
