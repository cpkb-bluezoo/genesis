/*
 * util.c
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

#include "util.h"

/* ========================================================================
 * Singly-linked list implementation
 * ======================================================================== */

/**
 * Create a new singly-linked list node.
 */
slist_t *slist_new(void *data)
{
    slist_t *node = malloc(sizeof(slist_t));
    if (!node) {
        return NULL;
    }
    node->data = data;
    node->next = NULL;
    return node;
}

/**
 * Append data to the end of a list.
 */
slist_t *slist_append(slist_t *list, void *data)
{
    slist_t *node = slist_new(data);
    if (!node) {
        return NULL;
    }
    
    if (!list) {
        return node;
    }
    
    slist_t *tail = slist_last(list);
    tail->next = node;
    return node;
}

/**
 * Prepend data to the beginning of a list.
 */
slist_t *slist_prepend(slist_t *list, void *data)
{
    slist_t *node = slist_new(data);
    if (!node) {
        return list;
    }
    node->next = list;
    return node;
}

/**
 * Get the last node in a list.
 */
slist_t *slist_last(slist_t *list)
{
    if (!list) {
        return NULL;
    }
    while (list->next) {
        list = list->next;
    }
    return list;
}

/**
 * Reverse a singly linked list in place.
 */
slist_t *slist_reverse(slist_t *list)
{
    slist_t *prev = NULL;
    slist_t *curr = list;
    while (curr) {
        slist_t *next = curr->next;
        curr->next = prev;
        prev = curr;
        curr = next;
    }
    return prev;
}

/**
 * Count elements in a list.
 */
size_t slist_length(slist_t *list)
{
    size_t count = 0;
    while (list) {
        count++;
        list = list->next;
    }
    return count;
}

/**
 * Free all nodes in a list (not the data).
 */
void slist_free(slist_t *list)
{
    while (list) {
        slist_t *next = list->next;
        free(list);
        list = next;
    }
}

/**
 * Free all nodes and their data.
 */
void slist_free_full(slist_t *list, void (*free_func)(void *))
{
    while (list) {
        slist_t *next = list->next;
        if (free_func && list->data) {
            free_func(list->data);
        }
        free(list);
        list = next;
    }
}

/**
 * Append data to a list, handling NULL list case.
 * This is a convenience function that handles the common pattern:
 *   if (list == NULL) list = slist_new(data); else slist_append(list, data);
 */
slist_t *slist_push(slist_t **list_ptr, void *data)
{
    if (!list_ptr) {
        return NULL;
    }
    
    if (*list_ptr == NULL) {
        *list_ptr = slist_new(data);
    } else {
        slist_append(*list_ptr, data);
    }
    return *list_ptr;
}

/* ========================================================================
 * Hash table implementation
 * ======================================================================== */

/**
 * Compute a hash value for a string.
 */
unsigned int str_hash(const char *str)
{
    unsigned int hash = 0;
    if (!str) {
        return 0;
    }
    
    while (*str) {
        hash = (hash * 31) + (unsigned char)*str;
        str++;
    }
    return hash;
}

/**
 * Resize a hash table.
 */
static void hashtable_resize(hashtable_t *ht, size_t new_size)
{
    hashtable_entry_t **old_buckets = ht->buckets;
    size_t old_size = ht->size;
    
    ht->buckets = calloc(new_size, sizeof(hashtable_entry_t *));
    if (!ht->buckets) {
        ht->buckets = old_buckets;
        return;
    }
    ht->size = new_size;
    ht->count = 0;
    
    for (size_t i = 0; i < old_size; i++) {
        hashtable_entry_t *entry = old_buckets[i];
        while (entry) {
            hashtable_entry_t *next = entry->next;
            unsigned int index = str_hash(entry->key) % new_size;
            entry->next = ht->buckets[index];
            ht->buckets[index] = entry;
            ht->count++;
            entry = next;
        }
    }
    
    free(old_buckets);
}

/**
 * Create a new hash table.
 */
hashtable_t *hashtable_new(void)
{
    hashtable_t *ht = malloc(sizeof(hashtable_t));
    if (!ht) {
        return NULL;
    }
    
    ht->size = HASHTABLE_INITIAL_SIZE;
    ht->count = 0;
    ht->buckets = calloc(ht->size, sizeof(hashtable_entry_t *));
    
    if (!ht->buckets) {
        free(ht);
        return NULL;
    }
    
    return ht;
}

/**
 * Free a hash table (not values).
 */
void hashtable_free(hashtable_t *ht)
{
    if (!ht) {
        return;
    }
    
    for (size_t i = 0; i < ht->size; i++) {
        hashtable_entry_t *entry = ht->buckets[i];
        while (entry) {
            hashtable_entry_t *next = entry->next;
            free(entry->key);
            free(entry);
            entry = next;
        }
    }
    
    free(ht->buckets);
    free(ht);
}

/**
 * Free a hash table and all values.
 */
void hashtable_free_full(hashtable_t *ht, void (*free_func)(void *))
{
    if (!ht) {
        return;
    }
    
    for (size_t i = 0; i < ht->size; i++) {
        hashtable_entry_t *entry = ht->buckets[i];
        while (entry) {
            hashtable_entry_t *next = entry->next;
            free(entry->key);
            if (free_func && entry->value) {
                free_func(entry->value);
            }
            free(entry);
            entry = next;
        }
    }
    
    free(ht->buckets);
    free(ht);
}

/**
 * Insert or update a key-value pair.
 */
void hashtable_insert(hashtable_t *ht, const char *key, void *value)
{
    if (!ht || !key) {
        return;
    }
    
    if ((double)ht->count / ht->size >= HASHTABLE_LOAD_FACTOR) {
        hashtable_resize(ht, ht->size * 2);
    }
    
    unsigned int index = str_hash(key) % ht->size;
    
    hashtable_entry_t *entry = ht->buckets[index];
    while (entry) {
        if (strcmp(entry->key, key) == 0) {
            entry->value = value;
            return;
        }
        entry = entry->next;
    }
    
    entry = malloc(sizeof(hashtable_entry_t));
    if (!entry) {
        return;
    }
    
    entry->key = strdup(key);
    entry->value = value;
    entry->next = ht->buckets[index];
    ht->buckets[index] = entry;
    ht->count++;
}

/**
 * Look up a value by key.
 */
void *hashtable_lookup(hashtable_t *ht, const char *key)
{
    if (!ht || !key) {
        return NULL;
    }
    
    unsigned int index = str_hash(key) % ht->size;
    hashtable_entry_t *entry = ht->buckets[index];
    
    while (entry) {
        if (strcmp(entry->key, key) == 0) {
            return entry->value;
        }
        entry = entry->next;
    }
    
    return NULL;
}

/**
 * Remove a key-value pair.
 */
void *hashtable_remove(hashtable_t *ht, const char *key)
{
    if (!ht || !key) {
        return NULL;
    }
    
    unsigned int index = str_hash(key) % ht->size;
    hashtable_entry_t *entry = ht->buckets[index];
    hashtable_entry_t *prev = NULL;
    
    while (entry) {
        if (strcmp(entry->key, key) == 0) {
            void *value = entry->value;
            
            if (prev) {
                prev->next = entry->next;
            } else {
                ht->buckets[index] = entry->next;
            }
            
            free(entry->key);
            free(entry);
            ht->count--;
            return value;
        }
        prev = entry;
        entry = entry->next;
    }
    
    return NULL;
}

/**
 * Check if a key exists.
 */
bool hashtable_contains(hashtable_t *ht, const char *key)
{
    if (!ht || !key) {
        return false;
    }
    
    unsigned int index = str_hash(key) % ht->size;
    hashtable_entry_t *entry = ht->buckets[index];
    
    while (entry) {
        if (strcmp(entry->key, key) == 0) {
            return true;
        }
        entry = entry->next;
    }
    
    return false;
}

/**
 * Iterate over all entries.
 */
void hashtable_foreach(hashtable_t *ht, hashtable_foreach_fn fn, void *user_data)
{
    if (!ht || !fn) {
        return;
    }
    
    for (size_t i = 0; i < ht->size; i++) {
        hashtable_entry_t *entry = ht->buckets[i];
        while (entry) {
            fn(entry->key, entry->value, user_data);
            entry = entry->next;
        }
    }
}

/* ========================================================================
 * Dynamic string implementation
 * ======================================================================== */

#define STRING_INITIAL_SIZE 64

/**
 * Ensure string has enough capacity.
 */
static void string_ensure_capacity(string_t *str, size_t needed)
{
    if (str->alloc >= needed) {
        return;
    }
    
    size_t new_alloc = str->alloc ? str->alloc : STRING_INITIAL_SIZE;
    while (new_alloc < needed) {
        new_alloc *= 2;
    }
    
    char *new_str = realloc(str->str, new_alloc);
    if (!new_str) {
        return;
    }
    
    str->str = new_str;
    str->alloc = new_alloc;
}

/**
 * Create a new dynamic string.
 */
string_t *string_new(const char *init)
{
    string_t *str = malloc(sizeof(string_t));
    if (!str) {
        return NULL;
    }
    
    str->str = NULL;
    str->len = 0;
    str->alloc = 0;
    
    if (init) {
        size_t len = strlen(init);
        string_ensure_capacity(str, len + 1);
        if (str->str) {
            memcpy(str->str, init, len + 1);
            str->len = len;
        }
    } else {
        string_ensure_capacity(str, 1);
        if (str->str) {
            str->str[0] = '\0';
        }
    }
    
    return str;
}

/**
 * Create a new string with specific length.
 */
string_t *string_new_len(const char *init, size_t len)
{
    string_t *str = malloc(sizeof(string_t));
    if (!str) {
        return NULL;
    }
    
    str->str = NULL;
    str->len = 0;
    str->alloc = 0;
    
    string_ensure_capacity(str, len + 1);
    if (str->str) {
        if (init) {
            memcpy(str->str, init, len);
        }
        str->str[len] = '\0';
        str->len = len;
    }
    
    return str;
}

/**
 * Free a dynamic string.
 */
char *string_free(string_t *str, bool free_segment)
{
    if (!str) {
        return NULL;
    }
    
    char *result = NULL;
    if (!free_segment) {
        result = str->str;
    } else {
        free(str->str);
    }
    
    free(str);
    return result;
}

/**
 * Append a C string.
 */
string_t *string_append(string_t *str, const char *val)
{
    if (!str || !val) {
        return str;
    }
    
    size_t val_len = strlen(val);
    string_ensure_capacity(str, str->len + val_len + 1);
    
    if (str->str) {
        memcpy(str->str + str->len, val, val_len + 1);
        str->len += val_len;
    }
    
    return str;
}

/**
 * Append bytes with specific length.
 */
string_t *string_append_len(string_t *str, const char *val, size_t len)
{
    if (!str || !val) {
        return str;
    }
    
    string_ensure_capacity(str, str->len + len + 1);
    
    if (str->str) {
        memcpy(str->str + str->len, val, len);
        str->len += len;
        str->str[str->len] = '\0';
    }
    
    return str;
}

/**
 * Append a single character.
 */
string_t *string_append_c(string_t *str, char c)
{
    if (!str) {
        return str;
    }
    
    string_ensure_capacity(str, str->len + 2);
    
    if (str->str) {
        str->str[str->len++] = c;
        str->str[str->len] = '\0';
    }
    
    return str;
}

/**
 * Append formatted text.
 */
string_t *string_append_printf(string_t *str, const char *format, ...)
{
    if (!str || !format) {
        return str;
    }
    
    va_list args, args_copy;
    va_start(args, format);
    va_copy(args_copy, args);
    
    int needed = vsnprintf(NULL, 0, format, args);
    va_end(args);
    
    if (needed < 0) {
        va_end(args_copy);
        return str;
    }
    
    string_ensure_capacity(str, str->len + needed + 1);
    
    if (str->str) {
        vsnprintf(str->str + str->len, needed + 1, format, args_copy);
        str->len += needed;
    }
    
    va_end(args_copy);
    return str;
}

/**
 * Truncate string to zero length.
 */
string_t *string_truncate(string_t *str)
{
    if (!str) {
        return str;
    }
    
    str->len = 0;
    if (str->str) {
        str->str[0] = '\0';
    }
    
    return str;
}

/* ========================================================================
 * String utilities implementation
 * ======================================================================== */

/**
 * Split a string by delimiter.
 */
char **str_split(const char *str, const char *delim, int max_tokens)
{
    if (!str || !delim) {
        return NULL;
    }
    
    size_t delim_len = strlen(delim);
    if (delim_len == 0) {
        return NULL;
    }
    
    int count = 1;
    const char *p = str;
    while ((p = strstr(p, delim)) != NULL) {
        count++;
        p += delim_len;
        if (max_tokens > 0 && count >= max_tokens) {
            break;
        }
    }
    
    char **result = malloc(sizeof(char *) * (count + 1));
    if (!result) {
        return NULL;
    }
    
    p = str;
    for (int i = 0; i < count; i++) {
        const char *next = strstr(p, delim);
        
        if (!next || (max_tokens > 0 && i == count - 1)) {
            result[i] = strdup(p);
        } else {
            result[i] = strndup(p, next - p);
            p = next + delim_len;
        }
        
        if (!result[i]) {
            for (int j = 0; j < i; j++) free(result[j]);
            free(result);
            return NULL;
        }
    }
    result[count] = NULL;
    
    return result;
}

/**
 * Free a string array.
 */
void str_freev(char **str_array)
{
    if (!str_array) {
        return;
    }
    
    for (char **p = str_array; *p; p++) {
        free(*p);
    }
    free(str_array);
}

/**
 * Concatenate multiple strings.
 */
char *str_concat(const char *first, ...)
{
    if (!first) {
        return NULL;
    }
    
    va_list args;
    
    size_t len = strlen(first);
    va_start(args, first);
    const char *s;
    while ((s = va_arg(args, const char *)) != NULL) {
        len += strlen(s);
    }
    va_end(args);
    
    char *result = malloc(len + 1);
    if (!result) {
        return NULL;
    }
    
    char *p = result;
    size_t first_len = strlen(first);
    memcpy(p, first, first_len);
    p += first_len;
    
    va_start(args, first);
    while ((s = va_arg(args, const char *)) != NULL) {
        size_t s_len = strlen(s);
        memcpy(p, s, s_len);
        p += s_len;
    }
    va_end(args);
    
    *p = '\0';
    return result;
}

/**
 * Strip whitespace from a string.
 */
char *str_strip(char *str)
{
    if (!str) {
        return NULL;
    }
    
    while (isspace((unsigned char)*str)) str++;
    
    if (*str == '\0') {
        return str;
    }
    
    char *end = str + strlen(str) - 1;
    while (end > str && isspace((unsigned char)*end)) end--;
    *(end + 1) = '\0';
    
    return str;
}

/**
 * Check if string has suffix.
 */
bool str_has_suffix(const char *str, const char *suffix)
{
    if (!str || !suffix) {
        return false;
    }
    
    size_t str_len = strlen(str);
    size_t suffix_len = strlen(suffix);
    
    if (suffix_len > str_len) {
        return false;
    }
    
    return strcmp(str + str_len - suffix_len, suffix) == 0;
}

/**
 * Check if string has prefix.
 */
bool str_has_prefix(const char *str, const char *prefix)
{
    if (!str || !prefix) {
        return false;
    }
    return strncmp(str, prefix, strlen(prefix)) == 0;
}

/**
 * Duplicate a string.
 */
char *str_dup(const char *str)
{
    if (!str) {
        return NULL;
    }
    return strdup(str);
}

/**
 * Duplicate up to n characters.
 */
char *str_ndup(const char *str, size_t n)
{
    if (!str) {
        return NULL;
    }
    return strndup(str, n);
}

/* ========================================================================
 * File utilities implementation
 * ======================================================================== */

/**
 * Check if file exists.
 */
bool file_exists(const char *path)
{
    if (!path) {
        return false;
    }
    return access(path, F_OK) == 0;
}

/**
 * Check if path is a directory.
 */
bool file_is_directory(const char *path)
{
    if (!path) {
        return false;
    }
    
    struct stat st;
    if (stat(path, &st) != 0) {
        return false;
    }
    return S_ISDIR(st.st_mode);
}

/**
 * Check if path is a regular file.
 */
bool file_is_regular(const char *path)
{
    if (!path) {
        return false;
    }
    
    struct stat st;
    if (stat(path, &st) != 0) {
        return false;
    }
    return S_ISREG(st.st_mode);
}

/**
 * Get current working directory.
 */
char *get_current_dir(void)
{
    char *buf = NULL;
    size_t size = 256;
    
    while (1) {
        buf = realloc(buf, size);
        if (!buf) {
            return NULL;
        }
        
        if (getcwd(buf, size) != NULL) {
            return buf;
        }
        
        if (errno != ERANGE) {
            free(buf);
            return NULL;
        }
        
        size *= 2;
    }
}

/**
 * Read entire file contents.
 */
char *file_get_contents(const char *filename, size_t *length)
{
    if (!filename) {
        return NULL;
    }
    
    int fd = open(filename, O_RDONLY);
    if (fd < 0) {
        return NULL;
    }
    
    struct stat st;
    if (fstat(fd, &st) < 0) {
        close(fd);
        return NULL;
    }
    
    size_t size = st.st_size;
    char *buf = malloc(size + 1);
    if (!buf) {
        close(fd);
        return NULL;
    }
    
    size_t total = 0;
    while (total < size) {
        ssize_t n = read(fd, buf + total, size - total);
        if (n < 0) {
            if (errno == EINTR) {
                continue;
            }
            free(buf);
            close(fd);
            return NULL;
        }
        if (n == 0) {
            break;
        }
        total += n;
    }
    
    close(fd);
    buf[total] = '\0';
    
    if (length) {
        *length = total;
    }
    return buf;
}

/**
 * Write contents to file.
 */
bool file_put_contents(const char *filename, const char *contents, size_t length)
{
    if (!filename || !contents) {
        return false;
    }
    
    int fd = open(filename, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (fd < 0) {
        return false;
    }
    
    if (length == 0) {
        length = strlen(contents);
    }
    
    size_t total = 0;
    while (total < length) {
        ssize_t n = write(fd, contents + total, length - total);
        if (n < 0) {
            if (errno == EINTR) {
                continue;
            }
            close(fd);
            return false;
        }
        total += n;
    }
    
    close(fd);
    return true;
}

/* ========================================================================
 * Miscellaneous implementation
 * ======================================================================== */

/**
 * Parse a boolean value from string.
 */
bool parse_boolean(const char *str, bool default_value)
{
    if (!str) {
        return default_value;
    }
    
    if (strcmp(str, "true") == 0 ||
        strcmp(str, "yes") == 0 ||
        strcmp(str, "on") == 0 ||
        strcmp(str, "1") == 0) {
        return true;
    }
    
    if (strcmp(str, "false") == 0 ||
        strcmp(str, "no") == 0 ||
        strcmp(str, "off") == 0 ||
        strcmp(str, "0") == 0) {
        return false;
    }
    
    return default_value;
}

