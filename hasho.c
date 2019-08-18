#include "hasho.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hash.h"

size_t ptrhash(void *ptr)
{
	/* combination of Knuth and some extra to try mingle lower bits more */
	return ((size_t)ptr * 2654435761 >> 20) ^ ((size_t)ptr * 2154435697);
}

int hasho_init(hasho_t *table, unsigned size)
{
	if (size <= 1 || (size & (size - 1)) != 0)
		return -1;

	table->entries = calloc(sizeof(*table->entries), size);
	if (table->entries == NULL)
		return -1;

	table->num_entries = 0;
	table->mask = size - 1;
	return 0;
}

void hasho_clear(struct hasho *table)
{
	memset(table->entries, 0, sizeof(*table->entries) * (table->mask + 1));
	table->num_entries = 0;
}

static int grow(struct hasho *table)
{
	unsigned i, oldsize = table->mask + 1, size = oldsize * 2;
	struct hasho_entry *old_entries = table->entries, *new_entries;

	new_entries = calloc(size, sizeof(*new_entries));
	if (new_entries == NULL)
		return -1;
	table->entries = new_entries;
	table->mask = size - 1;
	table->num_entries = 0;
	for (i = 0; i < oldsize; i++)
		if (old_entries[i].key != NULL)
			hasho_insert(table, old_entries[i].key, old_entries[i].value);

	free(old_entries);
	return 0;
}

void *hasho_find(struct hasho *table, void *key)
{
	unsigned i, start;

	start = i = ptrhash(key) & table->mask;
	for (;;) {
		if (table->entries[i].key == key)
			return table->entries[i].value;
		i = (i + 1) & table->mask;
		if (i == start)
			return NULL;
	}
}

struct hasho_entry *hasho_insert(struct hasho *table, void *key, void *value)
{
	unsigned i;

	if (table->num_entries * 2 > table->mask)
		if (grow(table) < 0)
			return (void*)(size_t)-1;

	i = ptrhash(key) & table->mask;
	for (;;) {
		/* hash is equal, check if really equal, that is a fail */
		if (table->entries[i].key == key)
			return &table->entries[i];
		if (table->entries[i].key == NULL) {
			table->entries[i].key = key;
			table->entries[i].value = value;
			table->num_entries++;
			return 0;
		}
		i = (i + 1) & table->mask;
	}

	return 0;
}

struct hasho_entry *hasho_next(struct hasho *table, struct hasho_entry *entry)
{
	if (entry == NULL)
		entry = &table->entries[0];
	else
		entry++;

	for (; entry <= &table->entries[table->mask]; entry++)
		if (entry->key != NULL)
			return entry;

	/* empty, end of table reached */
	return NULL;
}

int hasho_remove(struct hasho *table, void *key)
{
	unsigned i, start;

	start = i = ptrhash(key) & table->mask;
	for (;;) {
		if (table->entries[i].key == key) {
			table->entries[i].key = NULL;
			table->num_entries--;
			return 0;
		}
		i = (i + 1) & table->mask;
		if (i == start)
			return -1;
	}
}

int hasho_deinit(struct hasho *table)
{
	free(table->entries);
	table->entries = NULL;
	table->num_entries = 0;
	return 0;
}
