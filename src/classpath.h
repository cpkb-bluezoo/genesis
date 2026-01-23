/*
 * classpath.h
 * Copyright (C) 2016, 2020 Chris Burdess <dog@gnu.org>
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef CLASSPATH_H
#define CLASSPATH_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include "classfile.h"
#include "util.h"

/*
 * Classpath Management
 *
 * Handles searching for Java class files in:
 * - Directories (loose .class files)
 * - JAR files (ZIP archives)
 * - JMod files (Java 9+ module format)
 *
 * Supports both traditional bootclasspath/classpath and
 * Java 9+ module system paths.
 */

/*
 * Classpath entry types
 */
typedef enum cp_entry_type
{
    CP_DIRECTORY,       /* Directory with .class files */
    CP_JAR,             /* JAR/ZIP archive */
    CP_JMOD             /* Java module file (Java 9+) */
} cp_entry_type_t;

/*
 * A single classpath entry
 */
typedef struct cp_entry
{
    cp_entry_type_t type;
    char *path;                     /* Path to directory/jar/jmod */

    /* For JAR files: cached central directory */
    void *jar_handle;               /* Opaque handle for JAR reading */
} cp_entry_t;

/*
 * Classpath configuration
 */
typedef struct classpath
{
    slist_t *boot_entries;          /* Bootstrap classpath entries */
    slist_t *entries;               /* User classpath entries */

    /* Java home for finding system classes */
    char *java_home;

    /* Class cache: maps fully qualified name -> classfile_t* */
    hashtable_t *cache;
    
    /* Negative cache: classes we know don't exist (value is (void*)1) */
    hashtable_t *negative_cache;

    /* Statistics (use atomic operations for thread safety) */
    int classes_loaded;
    int cache_hits;
    int negative_cache_hits;
    
    /* Thread safety for cache access */
    void *cache_mutex;              /* pthread_mutex_t* (opaque for header portability) */
} classpath_t;

/*
 * JDK detection result
 */
typedef struct jdk_info
{
    char *java_home;
    int major_version;              /* 8, 11, 17, 21, etc. */
    bool is_modular;                /* Java 9+ uses modules */
    char *rt_jar_path;              /* Pre-Java 9: path to rt.jar */
    char *modules_path;             /* Java 9+: path to jmods or lib/modules */
} jdk_info_t;

/*
 * Classpath API
 */

/* Create a new classpath */
classpath_t *classpath_new(void);

/* Free a classpath */
void classpath_free(classpath_t *cp);

/* Add a directory or JAR to the boot classpath */
bool classpath_add_boot(classpath_t *cp, const char *path);

/* Add a directory or JAR to the user classpath */
bool classpath_add(classpath_t *cp, const char *path);

/* Add entries from a path string (colon-separated on Unix) */
bool classpath_add_path(classpath_t *cp, const char *pathstr, bool boot);

/* Set up boot classpath from detected JDK */
bool classpath_setup_jdk(classpath_t *cp, jdk_info_t *jdk);

/*
 * Class loading
 */

/* Find and load a class by fully qualified name (e.g., "java.lang.String") */
classfile_t *classpath_find_class(classpath_t *cp, const char *classname);

/* Find a class, searching boot classpath first, then user classpath */
classfile_t *classpath_load_class(classpath_t *cp, const char *classname);

/* Check if a class exists without fully loading it */
bool classpath_class_exists(classpath_t *cp, const char *classname);

/*
 * JDK detection
 */

/* Detect JDK from JAVA_HOME environment variable */
jdk_info_t *jdk_detect(void);

/* Detect JDK from a specific path */
jdk_info_t *jdk_detect_from_path(const char *java_home);

/* Free JDK info */
void jdk_info_free(jdk_info_t *jdk);

/* Print JDK info */
void jdk_info_print(jdk_info_t *jdk);

/*
 * JAR file handling
 */

/* Open a JAR file for reading */
void *jar_open(const char *path);

/* Close a JAR file */
void jar_close(void *handle);

/* Read a file from a JAR (returns allocated buffer, sets *size) */
uint8_t *jar_read_entry(void *handle, const char *entry_path, size_t *size);

/* Check if an entry exists in a JAR */
bool jar_entry_exists(void *handle, const char *entry_path);

/* List entries in a JAR matching a pattern */
slist_t *jar_list_entries(void *handle, const char *pattern);

/*
 * JMod file handling (Java 9+ modules)
 */

/* Open a JMod file */
void *jmod_open(const char *path);

/* Close a JMod file */
void jmod_close(void *handle);

/* Read a class from a JMod */
uint8_t *jmod_read_class(void *handle, const char *classname, size_t *size);

/*
 * Utility functions
 */

/* Convert class name to file path (java.lang.String -> java/lang/String.class) */
char *classname_to_path(const char *classname);

/* Get the package name from a fully qualified class name */
char *get_package_name(const char *classname);

/* Get the simple class name from a fully qualified class name */
char *get_simple_name(const char *classname);

#endif /* CLASSPATH_H */

