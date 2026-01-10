/*
 * util.h
 * Utility functions and data structures
 * Copyright (C) 2016 Chris Burdess <dog@gnu.org>
 * 
 * This file is part of genesis.
 * 
 * genesis is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * genesis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <https://www.gnu.org/licenses/>.
 */

#ifndef UTIL_H
#define UTIL_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdarg.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

/* ========================================================================
 * Platform-specific constants
 * ======================================================================== */

#ifdef _WIN32
    #define DIR_SEPARATOR '\\'
    #define DIR_SEPARATOR_S "\\"
    #define PATH_SEPARATOR ';'
    #define PATH_SEPARATOR_S ";"
#else
    #define DIR_SEPARATOR '/'
    #define DIR_SEPARATOR_S "/"
    #define PATH_SEPARATOR ':'
    #define PATH_SEPARATOR_S ":"
#endif

/* ========================================================================
 * Singly-linked list
 * ======================================================================== */

typedef struct slist
{
    void *data;
    struct slist *next;
} slist_t;

slist_t *slist_new(void *data);
slist_t *slist_append(slist_t *list, void *data);
slist_t *slist_prepend(slist_t *list, void *data);
slist_t *slist_last(slist_t *list);
slist_t *slist_reverse(slist_t *list);
#define slist_next(list) ((list) ? (list)->next : NULL)
size_t slist_length(slist_t *list);
void slist_free(slist_t *list);
void slist_free_full(slist_t *list, void (*free_func)(void *));

/**
 * Append data to a list, handling NULL list case.
 * This is a convenience function that handles the common pattern:
 *   if (list == NULL) list = slist_new(data); else slist_append(list, data);
 * 
 * @param list_ptr  Pointer to list pointer (will be updated if list was NULL)
 * @param data      Data to append
 * @return The list (same as *list_ptr after call)
 */
slist_t *slist_push(slist_t **list_ptr, void *data);

/* ========================================================================
 * Hash table
 * ======================================================================== */

#define HASHTABLE_INITIAL_SIZE 64
#define HASHTABLE_LOAD_FACTOR 0.75

typedef struct hashtable_entry
{
    char *key;
    void *value;
    struct hashtable_entry *next;
} hashtable_entry_t;

typedef struct hashtable
{
    hashtable_entry_t **buckets;
    size_t size;
    size_t count;
} hashtable_t;

hashtable_t *hashtable_new(void);
void hashtable_free(hashtable_t *ht);
void hashtable_free_full(hashtable_t *ht, void (*free_func)(void *));
void hashtable_insert(hashtable_t *ht, const char *key, void *value);
void *hashtable_lookup(hashtable_t *ht, const char *key);
void *hashtable_remove(hashtable_t *ht, const char *key);
bool hashtable_contains(hashtable_t *ht, const char *key);

typedef void (*hashtable_foreach_fn)(const char *key, void *value, void *user_data);
void hashtable_foreach(hashtable_t *ht, hashtable_foreach_fn fn, void *user_data);

/* ========================================================================
 * Dynamic string
 * ======================================================================== */

typedef struct string
{
    char *str;
    size_t len;
    size_t alloc;
} string_t;

string_t *string_new(const char *init);
string_t *string_new_len(const char *init, size_t len);
char *string_free(string_t *str, bool free_segment);
string_t *string_append(string_t *str, const char *val);
string_t *string_append_len(string_t *str, const char *val, size_t len);
string_t *string_append_c(string_t *str, char c);
string_t *string_append_printf(string_t *str, const char *format, ...);
string_t *string_truncate(string_t *str);

/* ========================================================================
 * String utilities
 * ======================================================================== */

char **str_split(const char *str, const char *delim, int max_tokens);
void str_freev(char **str_array);
char *str_concat(const char *first, ...);
char *str_strip(char *str);
bool str_has_suffix(const char *str, const char *suffix);
bool str_has_prefix(const char *str, const char *prefix);
char *str_dup(const char *str);
char *str_ndup(const char *str, size_t n);

/* ========================================================================
 * File utilities
 * ======================================================================== */

bool file_exists(const char *path);
bool file_is_directory(const char *path);
bool file_is_regular(const char *path);
char *get_current_dir(void);
char *file_get_contents(const char *filename, size_t *length);
bool file_put_contents(const char *filename, const char *contents, size_t length);

/* ========================================================================
 * Miscellaneous
 * ======================================================================== */

unsigned int str_hash(const char *str);
bool parse_boolean(const char *str, bool default_value);

#endif /* UTIL_H */

