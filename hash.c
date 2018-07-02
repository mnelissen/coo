#include "hash.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_HASH_DEPTH   4

/* Thomas Wang */
uint32_t uint32hash(uint32_t key)
{
  key = ~key + (key << 15); // key = (key << 15) - key - 1;
  key = key ^ (key >> 12);
  key = key + (key << 2);
  key = key ^ (key >> 4);
  key = key * 2057; // key = (key + (key << 3)) + (key << 11);
  key = key ^ (key >> 16);
  return key;
}

#if defined(_WIN64) || defined(__x86_64__) || defined(__ppc64__)

/* Thomas Wang */
uint64_t uint64hash(uint64_t key)
{
  key = (~key) + (key << 21); // key = (key << 21) - key - 1;
  key = key ^ (key >> 24);
  key = (key + (key << 3)) + (key << 8); // key * 265
  key = key ^ (key >> 14);
  key = (key + (key << 2)) + (key << 4); // key * 21
  key = key ^ (key >> 28);
  key = key + (key << 31);
  return key;
}

#else

/* Thomas Wang, 64bit to 32bit */
uint32_t uint64hash(uint64_t key)
{
  key = (~key) + (key << 18); // key = (key << 18) - key - 1;
  key = key ^ (key >> 31);
  key = key * 21; // key = (key + (key << 2)) + (key << 4);
  key = key ^ (key >> 11);
  key = key + (key << 6);
  key = key ^ (key >> 22);
  return (uint32_t)key;
}

#endif

size_t memhash(const void *ptr, size_t size)
{
	size_t i, hash = 5381;
	const uint8_t *data = ptr;

	for (i = 0; i < size; i++, data++)
		hash = (hash * 65600 - hash) ^ *data;
	return hash;
}

size_t strhash(const char *str)
{
	size_t hash = 5381;
	char c;

	while ((c = *str++) != 0)
		hash = (hash * 65600 - hash) ^ (unsigned)c;
	return hash;
}

static struct hash_entry *to_entry(struct hash *table, void *user)
{
	return (struct hash_entry *)((char*)user + table->user_offset);
}

static void *to_cmp(struct hash *table, struct hash_entry *entry)
{
	return (char*)entry + table->cmp_offset;
}

static void *to_user(struct hash *table, struct hash_entry *entry)
{
	return (char*)entry - table->user_offset;
}

int hash_init(hash_t *table, hash_cmp_cb compare, unsigned size,
		int user_entry_offset, int user_cmp_offset)
{
	if (size <= 1 || (size & (size - 1)) != 0)
		return -1;

	table->entries = calloc(sizeof(*table->entries), size);
	if (table->entries == NULL)
		return -1;

	table->compare = compare;
	table->num_entries = 0;
	table->mask = size - 1;
	/* from entry to compare: entry back to user, forward to compare */
	table->cmp_offset = user_cmp_offset - user_entry_offset;
	table->user_offset = user_entry_offset;
	return 0;
}

void hash_clear(struct hash *table)
{
	memset(table->entries, 0, sizeof(*table->entries) * (table->mask + 1));
	table->num_entries = 0;
}

static void insert(struct hash_entry **insertp, struct hash_entry *new)
{
	new->next = *insertp;
	*insertp = new;
}

static int grow(struct hash *table)
{
	struct hash_entry **new_entries, **new_iter, **new_list[2], *entry;
	unsigned i, which, oldsize = table->mask + 1, size = oldsize * 2;

	new_entries = realloc(table->entries, size);
	if (new_entries == NULL)
		return -1;
	/* use fact that new table is exactly twice as large, and effectively we
	   use a single new bit in hash_value to determine new bucket
	   [0] goes to [0..1], [1] goes to [2..3], etc. */
	for (i = 0, new_iter = new_entries; i < oldsize; i++) {
		new_list[0] = new_iter++, new_list[1] = new_iter++;
		for (entry = table->entries[i]; entry; entry = entry->next) {
			which = (entry->hash & oldsize) != 0;
			*new_list[which] = entry;
			new_list[which] = &entry->next;
		}

		*new_list[0] = *new_list[1] = NULL;
	}

	free(table->entries);
	table->entries = new_entries;
	table->mask = size - 1;
	return 0;
}

void *hash_find_custom(struct hash *table, size_t hash, hash_cmp_cb compare, void *key)
{
	struct hash_entry *entry;
	size_t table_index;

	table_index = hash & table->mask;
	entry = table->entries[table_index];
	for (; entry; entry = entry->next) {
		/* hash is equal, check if really equal, that is a fail */
		if (entry->hash == hash && compare(to_cmp(table, entry), key) == 0)
			return to_user(table, entry);
	}

	return NULL;
}

void *hash_find(struct hash *table, size_t hash, void *key)
{
	return hash_find_custom(table, hash, table->compare, key);
}

int hash_insert(struct hash *table, struct hash_entry *new, size_t hash)
{
	struct hash_entry *entry;
	size_t table_index, depth;
	void *new_cmp = to_cmp(table, new);

	/* check for duplicates */
	new->hash = hash;
retry:
	table_index = hash & table->mask;
	entry = table->entries[table_index];
	for (depth = 0; entry; entry = entry->next) {
		/* hash is equal, check if really equal, that is a fail */
		if (entry->hash == hash && table->compare(to_cmp(table, entry), new_cmp) == 0)
			return -1;
		if (++depth == MAX_HASH_DEPTH) {
			if (grow(table) < 0)
				return -1;
			/* could be more optimal by checking "other" hash is
			   not empty, but this is simpler, and seldom grow */
			goto retry;
		}
	}

	insert(&table->entries[table_index], new);
	return 0;
}

void *hash_next(struct hash *table, void *user)
{
	struct hash_entry *entry;
	size_t index;

	if (user != NULL) {
		entry = to_entry(table, user);
		index = (entry->hash & table->mask) + 1;
		entry = entry->next;
		if (entry)
			goto ret;
	} else
		index = 0;

	/* find next used hash index entry */
	for (; index <= table->mask; index++)
		if ((entry = table->entries[index]) != NULL)
			goto ret;

	/* empty, end of table reached */
	return NULL;
ret:
	return to_user(table, entry);
}

int hash_remove(struct hash *table, struct hash_entry *delete)
{
	struct hash_entry **nextp, **entryp = &table->entries[delete->hash & table->mask];

	/* find the previous entry within the linked list */
	for (; *entryp; entryp = nextp) {
		nextp = &(*entryp)->next;
		if (*nextp == delete) {
			*entryp = delete->next;
			return 0;
		}
	}

	/* not found */
	return -1;
}

int hash_destroy(struct hash *table)
{
	free(table->entries);
	table->entries = NULL;
	return 0;
}
