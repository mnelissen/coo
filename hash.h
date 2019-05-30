/* bucket hash table implementation */
#ifndef _HASH_H
#define _HASH_H
#include <stddef.h>
#include <stdint.h>

#define hash_insert_exists(tbl, new_entry, h, resvar) \
        (resvar = hash_insert(tbl, new_entry, h)) != NULL && (int)(size_t)resvar != -1
#define hash_insert_nomem(entry) ((size_t)entry == (size_t)-1)

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
	unsigned num_entries;
        int cmp_offset;   /* offset from struct hash_entry to compare data */
        int user_offset;  /* offset from userdata to struct hash_entry */
} hash_t;

uint32_t uint32hash(uint32_t key);
uint64_t uint64hash(uint64_t key);
size_t strhash(const char *str);
size_t memhash(const void *mem, size_t size);

/* pass offset from userdata to compare data in user_cmp_offset */
int hash_init(hash_t *table, hash_cmp_cb compare, unsigned size,
        int user_entry_offset, int user_cmp_offset);
void hash_clear(struct hash *table);
/* find user entry with provided address of compare data as key, return NULL if not found */
void *hash_find(struct hash *table, size_t hash, void *key);
/* custom compare callback called like compare(user, key) */
void *hash_find_custom(struct hash *table, size_t hash, hash_cmp_cb compare, void *key);
struct hash_entry *hash_insert(struct hash *table, struct hash_entry *entry, size_t hash);
void *hash_next(struct hash *table, void *user);
int hash_remove(struct hash *table, struct hash_entry *entry);
int hash_destroy(struct hash *table);

#define hash_foreach(u, h) \
  for (u = NULL; (u = hash_next(h, u)) != NULL;)

#endif
