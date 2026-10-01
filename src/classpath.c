/*
 * classpath.c
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <dirent.h>
#include <unistd.h>
#include <zlib.h>
#include <pthread.h>
#include "classpath.h"

/*
 * ZIP file format constants
 */
#define ZIP_LOCAL_FILE_HEADER_SIG       0x04034b50
#define ZIP_CENTRAL_DIR_HEADER_SIG      0x02014b50
#define ZIP_END_CENTRAL_DIR_SIG         0x06054b50
#define ZIP_END_CENTRAL_DIR64_SIG       0x06064b50
#define ZIP_END_CENTRAL_DIR64_LOC_SIG   0x07064b50

#define ZIP_METHOD_STORED   0
#define ZIP_METHOD_DEFLATED 8

/*
 * ZIP central directory entry
 */
typedef struct zip_entry
{
    char *filename;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
    uint32_t local_header_offset;
    uint16_t compression_method;
    struct zip_entry *next;
} zip_entry_t;

/*
 * JAR handle structure
 */
typedef struct jar_handle
{
    FILE *fp;
    char *path;
    zip_entry_t *entries;
    int entry_count;
    long base_offset;           /* Offset to start of ZIP data (4 for JMOD, 0 for JAR) */
    hashtable_t *entry_index;   /* O(1) lookup: filename -> zip_entry_t* */
} jar_handle_t;

/*
 * Helper: read little-endian values from buffer
 */
static uint16_t read_le16(const uint8_t *buf)
{
    return (uint16_t)buf[0] | ((uint16_t)buf[1] << 8);
}

static uint32_t read_le32(const uint8_t *buf)
{
    return (uint32_t)buf[0] | ((uint32_t)buf[1] << 8) |
           ((uint32_t)buf[2] << 16) | ((uint32_t)buf[3] << 24);
}

/*
 * Find end of central directory record
 * base_offset is the offset where ZIP data starts (0 for JAR, 4 for JMOD)
 */
static long find_eocd(FILE *fp, long base_offset)
{
    /* EOCD is at least 22 bytes, search backwards from end */
    fseek(fp, 0, SEEK_END);
    long file_size = ftell(fp);

    /* Search backwards for signature (max 65KB for comment) */
    long search_start = file_size - 65536 - 22;
    if (search_start < base_offset) {
        search_start = base_offset;
    }

    uint8_t buf[65536 + 22];
    size_t search_len = file_size - search_start;
    fseek(fp, search_start, SEEK_SET);

    if (fread(buf, 1, search_len, fp) != search_len) {
        return -1;
    }

    /* Search backwards for EOCD signature */
    for (long i = search_len - 22; i >= 0; i--) {
        if (read_le32(buf + i) == ZIP_END_CENTRAL_DIR_SIG) {
            return search_start + i;
        }
    }

    return -1;
}

/*
 * Parse central directory
 */
static bool parse_central_directory(jar_handle_t *jar)
{
    long eocd_offset = find_eocd(jar->fp, jar->base_offset);
    if (eocd_offset < 0) {
        fprintf(stderr, "error: cannot find end of central directory in %s\n", jar->path);
        return false;
    }

    /* Read EOCD */
    uint8_t eocd[22];
    fseek(jar->fp, eocd_offset, SEEK_SET);
    if (fread(eocd, 1, 22, jar->fp) != 22) {
        return false;
    }

    uint16_t total_entries = read_le16(eocd + 10);
    uint32_t cd_size = read_le32(eocd + 12);
    uint32_t cd_offset = read_le32(eocd + 16);

    /* Handle ZIP64 if needed */
    if (cd_offset == 0xFFFFFFFF || total_entries == 0xFFFF) {
        /* TODO: Implement ZIP64 support */
        fprintf(stderr, "error: ZIP64 format not yet supported\n");
        return false;
    }

    /* Read central directory - add base_offset for JMOD files */
    uint8_t *cd = malloc(cd_size);
    if (!cd) {
        return false;
    }

    fseek(jar->fp, jar->base_offset + cd_offset, SEEK_SET);
    if (fread(cd, 1, cd_size, jar->fp) != cd_size) {
        free(cd);
        return false;
    }

    /* Parse entries */
    uint8_t *p = cd;
    uint8_t *end = cd + cd_size;
    zip_entry_t *last = NULL;

    while (p + 46 <= end) {
        uint32_t sig = read_le32(p);
        if (sig != ZIP_CENTRAL_DIR_HEADER_SIG) {
            break;
        }

        uint16_t compression = read_le16(p + 10);
        uint32_t compressed = read_le32(p + 20);
        uint32_t uncompressed = read_le32(p + 24);
        uint16_t name_len = read_le16(p + 28);
        uint16_t extra_len = read_le16(p + 30);
        uint16_t comment_len = read_le16(p + 32);
        uint32_t local_offset = read_le32(p + 42);

        if (p + 46 + name_len > end) {
            break;
        }

        /* Create entry */
        zip_entry_t *entry = calloc(1, sizeof(zip_entry_t));
        if (!entry) {
            free(cd);
            return false;
        }

        entry->filename = malloc(name_len + 1);
        if (!entry->filename) {
            free(entry);
            free(cd);
            return false;
        }
        memcpy(entry->filename, p + 46, name_len);
        entry->filename[name_len] = '\0';

        entry->compression_method = compression;
        entry->compressed_size = compressed;
        entry->uncompressed_size = uncompressed;
        entry->local_header_offset = local_offset;
        entry->next = NULL;

        if (last) {
            last->next = entry;
        } else {
            jar->entries = entry;
        }
        last = entry;
        jar->entry_count++;

        p += 46 + name_len + extra_len + comment_len;
    }

    free(cd);
    
    /* Build hashtable index for O(1) lookups */
    jar->entry_index = hashtable_new_sized(jar->entry_count);
    if (jar->entry_index) {
        for (zip_entry_t *e = jar->entries; e; e = e->next) {
            hashtable_insert(jar->entry_index, e->filename, e);
        }
    }
    
    return true;
}

/*
 * JAR file API
 */

void *jar_open(const char *path)
{
    jar_handle_t *jar = calloc(1, sizeof(jar_handle_t));
    if (!jar) {
        return NULL;
    }

    jar->fp = fopen(path, "rb");
    if (!jar->fp) {
        free(jar);
        return NULL;
    }

    jar->path = strdup(path);
    if (!jar->path) {
        fclose(jar->fp);
        free(jar);
        return NULL;
    }

    jar->base_offset = 0;  /* Standard JAR/ZIP starts at offset 0 */

    if (!parse_central_directory(jar)) {
        jar_close(jar);
        return NULL;
    }

    return jar;
}

/**
 * Open a JAR/ZIP with a custom base offset (internal use).
 */
static void *jar_open_with_offset(const char *path, long base_offset)
{
    jar_handle_t *jar = calloc(1, sizeof(jar_handle_t));
    if (!jar) {
        return NULL;
    }

    jar->fp = fopen(path, "rb");
    if (!jar->fp) {
        free(jar);
        return NULL;
    }

    jar->path = strdup(path);
    if (!jar->path) {
        fclose(jar->fp);
        free(jar);
        return NULL;
    }

    jar->base_offset = base_offset;

    if (!parse_central_directory(jar)) {
        jar_close(jar);
        return NULL;
    }

    return jar;
}

void jar_close(void *handle)
{
    if (!handle) {
        return;
    }

    jar_handle_t *jar = (jar_handle_t *)handle;

    if (jar->fp) {
        fclose(jar->fp);
    }
    free(jar->path);
    
    /* Free entry index */
    if (jar->entry_index) {
        hashtable_free(jar->entry_index);
    }

    /* Free entries */
    zip_entry_t *entry = jar->entries;
    while (entry) {
        zip_entry_t *next = entry->next;
        free(entry->filename);
        free(entry);
        entry = next;
    }

    free(jar);
}

static zip_entry_t *jar_find_entry(jar_handle_t *jar, const char *entry_path)
{
    /* Use O(1) hashtable lookup if available */
    if (jar->entry_index) {
        return (zip_entry_t *)hashtable_lookup(jar->entry_index, entry_path);
    }
    
    /* Fallback to linear search */
    for (zip_entry_t *e = jar->entries; e; e = e->next) {
        if (strcmp(e->filename, entry_path) == 0) {
            return e;
        }
    }
    return NULL;
}

uint8_t *jar_read_entry(void *handle, const char *entry_path, size_t *size)
{
    jar_handle_t *jar = (jar_handle_t *)handle;
    zip_entry_t *entry = jar_find_entry(jar, entry_path);
    if (!entry) {
        return NULL;
    }

    /* Read local file header to find actual data offset */
    /* Add base_offset for JMOD files.
     * The archive is read with positioned reads on its descriptor: classes
     * are loaded from several code generation threads at once, and a
     * seek followed by a read on the one shared stream is not atomic. */
    int fd = fileno(jar->fp);
    long local_offset = jar->base_offset + entry->local_header_offset;
    uint8_t local_header[30];
    if (pread(fd, local_header, 30, (off_t)local_offset) != 30) {
        return NULL;
    }

    if (read_le32(local_header) != ZIP_LOCAL_FILE_HEADER_SIG) {
        return NULL;
    }

    uint16_t name_len = read_le16(local_header + 26);
    uint16_t extra_len = read_le16(local_header + 28);

    /* Read compressed data, which follows the header's name and extra field */
    uint8_t *compressed = malloc(entry->compressed_size);
    if (!compressed) {
        return NULL;
    }

    size_t got = 0;
    while (got < entry->compressed_size) {
        ssize_t n = pread(fd, compressed + got, entry->compressed_size - got,
                          (off_t)(local_offset + 30 + name_len + extra_len) + (off_t)got);
        if (n <= 0) {
            free(compressed);
            return NULL;
        }
        got += (size_t)n;
    }

    uint8_t *result;
    *size = entry->uncompressed_size;

    if (entry->compression_method == ZIP_METHOD_STORED) {
        /* No compression */
        result = compressed;
    } else if (entry->compression_method == ZIP_METHOD_DEFLATED) {
        /* Deflate compression */
        result = malloc(entry->uncompressed_size);
        if (!result) {
            free(compressed);
            return NULL;
        }

        z_stream strm = {0};
        strm.next_in = compressed;
        strm.avail_in = entry->compressed_size;
        strm.next_out = result;
        strm.avail_out = entry->uncompressed_size;

        /* Use raw deflate (negative windowBits) */
        if (inflateInit2(&strm, -MAX_WBITS) != Z_OK) {
            free(compressed);
            free(result);
            return NULL;
        }

        int ret = inflate(&strm, Z_FINISH);
        inflateEnd(&strm);
        free(compressed);

        if (ret != Z_STREAM_END) {
            free(result);
            return NULL;
        }
    } else {
        fprintf(stderr, "error: unsupported compression method %d\n",
                entry->compression_method);
        free(compressed);
        return NULL;
    }

    return result;
}

bool jar_entry_exists(void *handle, const char *entry_path)
{
    jar_handle_t *jar = (jar_handle_t *)handle;
    return jar_find_entry(jar, entry_path) != NULL;
}

slist_t *jar_list_entries(void *handle, const char *pattern)
{
    jar_handle_t *jar = (jar_handle_t *)handle;
    slist_t *list = NULL;

    size_t pattern_len = pattern ? strlen(pattern) : 0;

    for (zip_entry_t *e = jar->entries; e; e = e->next) {
        bool match = true;
        if (pattern && pattern_len > 0) {
            /* Simple suffix match for now */
            size_t name_len = strlen(e->filename);
            if (name_len < pattern_len ||
                strcmp(e->filename + name_len - pattern_len, pattern) != 0) {
                match = false;
            }
        }
        if (match) {
            list = slist_append(list, strdup(e->filename));
        }
    }

    return list;
}

/*
 * JMod file handling
 * 
 * JMOD files are ZIP archives with a 4-byte header prefix "JM\x01\x00".
 * The internal structure is:
 *   classes/       - .class files
 *   conf/          - Configuration files
 *   include/       - Header files
 *   legal/         - License files
 *   lib/           - Native libraries
 *   bin/           - Executables
 */

#define JMOD_HEADER_SIZE 4
#define JMOD_MAGIC_0 'J'
#define JMOD_MAGIC_1 'M'
#define JMOD_MAGIC_2 0x01
#define JMOD_MAGIC_3 0x00

void *jmod_open(const char *path)
{
    /* Verify JMOD signature first */
    FILE *fp = fopen(path, "rb");
    if (!fp) {
        return NULL;
    }

    uint8_t header[4];
    if (fread(header, 1, 4, fp) != 4) {
        fclose(fp);
        return NULL;
    }
    fclose(fp);

    /* Check JMod signature */
    if (header[0] != JMOD_MAGIC_0 || header[1] != JMOD_MAGIC_1 ||
        header[2] != JMOD_MAGIC_2 || header[3] != JMOD_MAGIC_3) {
        return NULL;
    }

    /* Open as ZIP with 4-byte offset */
    return jar_open_with_offset(path, JMOD_HEADER_SIZE);
}

void jmod_close(void *handle)
{
    if (handle) {
        jar_close(handle);
    }
}

uint8_t *jmod_read_class(void *handle, const char *classname, size_t *size)
{
    if (!handle || !classname) {
        return NULL;
    }

    /* Classes in jmod are under classes/ directory */
    /* classname is in internal form: java/lang/String */
    size_t pathlen = strlen("classes/") + strlen(classname) + strlen(".class") + 1;
    char *path = malloc(pathlen);
    if (!path) {
        return NULL;
    }
    snprintf(path, pathlen, "classes/%s.class", classname);

    uint8_t *data = jar_read_entry(handle, path, size);
    free(path);
    return data;
}

/**
 * Check if a class exists in a JMOD file.
 */
bool jmod_class_exists(void *handle, const char *classname)
{
    if (!handle || !classname) {
        return false;
    }

    size_t pathlen = strlen("classes/") + strlen(classname) + strlen(".class") + 1;
    char *path = malloc(pathlen);
    if (!path) {
        return false;
    }
    snprintf(path, pathlen, "classes/%s.class", classname);

    bool exists = jar_entry_exists(handle, path);
    free(path);
    return exists;
}

/*
 * JDK detection
 */

static bool is_directory(const char *path)
{
    struct stat st;
    if (stat(path, &st) != 0) {
        return false;
    }
    return S_ISDIR(st.st_mode);
}

static int detect_jdk_version(const char *java_home)
{
    /* Try to read release file */
    char release_path[1024];
    snprintf(release_path, sizeof(release_path), "%s/release", java_home);

    FILE *fp = fopen(release_path, "r");
    if (!fp) {
        return 0;
    }

    char line[256];
    int version = 0;

    while (fgets(line, sizeof(line), fp)) {
        if (strncmp(line, "JAVA_VERSION=", 13) == 0) {
            char *ver = line + 13;
            /* Skip quotes */
            if (*ver == '"') {
                ver++;
            }

            /* Parse version: "1.8.0_xxx" or "11.0.x" or "17.0.x" */
            if (strncmp(ver, "1.", 2) == 0) {
                version = atoi(ver + 2);  /* Java 1.8 -> 8 */
            } else {
                version = atoi(ver);       /* Java 11+ */
            }
            break;
        }
    }

    fclose(fp);
    return version;
}

jdk_info_t *jdk_detect_from_path(const char *java_home)
{
    if (!java_home || !is_directory(java_home)) {
        return NULL;
    }

    jdk_info_t *jdk = calloc(1, sizeof(jdk_info_t));
    if (!jdk) {
        return NULL;
    }

    jdk->java_home = strdup(java_home);
    jdk->major_version = detect_jdk_version(java_home);

    /* Check for rt.jar (pre-Java 9) */
    char rt_jar[1024];
    snprintf(rt_jar, sizeof(rt_jar), "%s/jre/lib/rt.jar", java_home);
    if (file_exists(rt_jar)) {
        jdk->rt_jar_path = strdup(rt_jar);
        jdk->is_modular = false;
    } else {
        /* Also check without /jre for some JDK layouts */
        snprintf(rt_jar, sizeof(rt_jar), "%s/lib/rt.jar", java_home);
        if (file_exists(rt_jar)) {
            jdk->rt_jar_path = strdup(rt_jar);
            jdk->is_modular = false;
        }
    }

    /* Check for modules (Java 9+) */
    char modules[1024];
    snprintf(modules, sizeof(modules), "%s/jmods", java_home);
    if (is_directory(modules)) {
        jdk->modules_path = strdup(modules);
        jdk->is_modular = true;
    } else {
        /* Check for lib/modules file */
        snprintf(modules, sizeof(modules), "%s/lib/modules", java_home);
        if (file_exists(modules)) {
            jdk->modules_path = strdup(modules);
            jdk->is_modular = true;
        }
    }

    /* If we found neither rt.jar nor modules, this might not be a valid JDK */
    if (!jdk->rt_jar_path && !jdk->modules_path) {
        jdk_info_free(jdk);
        return NULL;
    }

    return jdk;
}

jdk_info_t *jdk_detect(void)
{
    /* Try JAVA_HOME environment variable */
    const char *java_home = getenv("JAVA_HOME");
    if (java_home) {
        jdk_info_t *jdk = jdk_detect_from_path(java_home);
        if (jdk) {
            return jdk;
        }
    }

    /* Try common locations on macOS */
#ifdef __APPLE__
    const char *mac_paths[] = {
        "/Library/Java/JavaVirtualMachines/jdk-25.jdk/Contents/Home",
        "/Library/Java/JavaVirtualMachines/jdk-25-macports.jdk/Contents/Home",
        "/Library/Java/JavaVirtualMachines/jdk-21.jdk/Contents/Home",
        "/Library/Java/JavaVirtualMachines/jdk-17.jdk/Contents/Home",
        "/Library/Java/JavaVirtualMachines/jdk-11.jdk/Contents/Home",
        "/Library/Java/JavaVirtualMachines/jdk1.8.0_381.jdk/Contents/Home",
        NULL
    };
    for (int i = 0; mac_paths[i]; i++) {
        jdk_info_t *jdk = jdk_detect_from_path(mac_paths[i]);
        if (jdk) {
            return jdk;
        }
    }

    /* Try java_home command on macOS */
    FILE *fp = popen("/usr/libexec/java_home 2>/dev/null", "r");
    if (fp) {
        char path[1024];
        if (fgets(path, sizeof(path), fp)) {
            /* Remove trailing newline */
            path[strcspn(path, "\n")] = '\0';
            jdk_info_t *jdk = jdk_detect_from_path(path);
            pclose(fp);
            if (jdk) {
                return jdk;
            }
        } else {
            pclose(fp);
        }
    }
#endif

    /* Try common locations on Linux */
#ifdef __linux__
    const char *linux_paths[] = {
        "/usr/lib/jvm/java-25-openjdk",
        "/usr/lib/jvm/java-21-openjdk",
        "/usr/lib/jvm/java-17-openjdk",
        "/usr/lib/jvm/java-11-openjdk",
        "/usr/lib/jvm/java-8-openjdk",
        "/usr/lib/jvm/default-java",
        NULL
    };
    for (int i = 0; linux_paths[i]; i++) {
        jdk_info_t *jdk = jdk_detect_from_path(linux_paths[i]);
        if (jdk) {
            return jdk;
        }
    }
#endif

    return NULL;
}

void jdk_info_free(jdk_info_t *jdk)
{
    if (jdk) {
        free(jdk->java_home);
        free(jdk->rt_jar_path);
        free(jdk->modules_path);
        free(jdk);
    }
}

void jdk_info_print(jdk_info_t *jdk)
{
    if (!jdk) {
        printf("No JDK detected\n");
        return;
    }

    printf("JDK Information:\n");
    printf("  JAVA_HOME: %s\n", jdk->java_home);
    printf("  Version: Java %d\n", jdk->major_version);
    printf("  Modular: %s\n", jdk->is_modular ? "yes" : "no");
    if (jdk->rt_jar_path) {
        printf("  rt.jar: %s\n", jdk->rt_jar_path);
    }
    if (jdk->modules_path) {
        printf("  Modules: %s\n", jdk->modules_path);
    }
}

/*
 * Classpath entry management
 */

static cp_entry_t *cp_entry_new(const char *path)
{
    struct stat st;
    if (stat(path, &st) != 0) {
        return NULL;
    }

    cp_entry_t *entry = calloc(1, sizeof(cp_entry_t));
    if (!entry) {
        return NULL;
    }

    entry->path = strdup(path);
    if (!entry->path) {
        free(entry);
        return NULL;
    }

    if (S_ISDIR(st.st_mode)) {
        entry->type = CP_DIRECTORY;
    } else {
        /* Check extension */
        size_t len = strlen(path);
        if (len > 5 && strcasecmp(path + len - 5, ".jmod") == 0) {
            entry->type = CP_JMOD;
            entry->jar_handle = jmod_open(path);
            if (!entry->jar_handle) {
                fprintf(stderr, "warning: failed to open JMOD: %s\n", path);
            }
        } else {
            entry->type = CP_JAR;
            entry->jar_handle = jar_open(path);
            if (!entry->jar_handle) {
                fprintf(stderr, "warning: failed to open JAR: %s\n", path);
            }
        }
    }

    return entry;
}

static void cp_entry_free(cp_entry_t *entry)
{
    if (entry) {
        free(entry->path);
        if (entry->type == CP_JAR && entry->jar_handle) {
            jar_close(entry->jar_handle);
        } else if (entry->type == CP_JMOD && entry->jar_handle) {
            jmod_close(entry->jar_handle);
        }
        free(entry);
    }
}

classpath_t *classpath_new(void)
{
    classpath_t *cp = calloc(1, sizeof(classpath_t));
    if (!cp) {
        return NULL;
    }

    cp->boot_entries = NULL;  /* slist starts as NULL */
    cp->entries = NULL;
    cp->cache = hashtable_new();
    cp->negative_cache = hashtable_new_sized(1024);  /* Pre-sized for common misses */
    cp->no_files = hashtable_new();

    if (!cp->cache || !cp->negative_cache || !cp->no_files) {
        classpath_free(cp);
        return NULL;
    }
    
    /* Initialize lock for thread-safe cache access */
    pthread_mutex_t *lock = malloc(sizeof(pthread_mutex_t));
    if (lock) {
        pthread_mutex_init(lock, NULL);
        cp->cache_lock = lock;
    }

    return cp;
}

static void free_cp_entry(void *data)
{
    cp_entry_free((cp_entry_t *)data);
}

static void free_classfile(void *data)
{
    classfile_free((classfile_t *)data);
}

/*
 * Package index
 */

/* The JAR/JMOD entries holding classes of one package, in search order */
typedef struct cp_package
{
    cp_entry_t **entries;
    int count;
    int capacity;
} cp_package_t;

static void cp_package_free(void *data)
{
    cp_package_t *pkg = (cp_package_t *)data;
    if (pkg) {
        free(pkg->entries);
        free(pkg);
    }
}

/* Free the per-package file listings of a directory entry */
static void cp_entry_free_dir_packages(classpath_t *cp, cp_entry_t *entry)
{
    if (!entry || !entry->dir_packages) {
        return;
    }
    for (size_t i = 0; i < entry->dir_packages->size; i++) {
        for (hashtable_entry_t *e = entry->dir_packages->buckets[i]; e; e = e->next) {
            if (e->value != cp->no_files) {
                hashtable_free((hashtable_t *)e->value);
            }
        }
    }
    hashtable_free(entry->dir_packages);
    entry->dir_packages = NULL;
}

static void classpath_free_package_index(classpath_t *cp)
{
    if (cp->package_index) {
        hashtable_free_full(cp->package_index, cp_package_free);
        cp->package_index = NULL;
    }
    free(cp->dir_entries);
    cp->dir_entries = NULL;
    cp->dir_entry_count = 0;
    cp->indexed_entry_count = 0;
}

void classpath_free(classpath_t *cp)
{
    if (!cp) {
        return;
    }

    classpath_free_package_index(cp);
    for (slist_t *node = cp->boot_entries; node; node = node->next) {
        cp_entry_free_dir_packages(cp, (cp_entry_t *)node->data);
    }
    for (slist_t *node = cp->entries; node; node = node->next) {
        cp_entry_free_dir_packages(cp, (cp_entry_t *)node->data);
    }
    if (cp->no_files) {
        hashtable_free(cp->no_files);
    }

    /* Free boot entries */
    if (cp->boot_entries) {
        slist_free_full(cp->boot_entries, free_cp_entry);
    }

    /* Free user entries */
    if (cp->entries) {
        slist_free_full(cp->entries, free_cp_entry);
    }

    /* Free cache */
    if (cp->cache) {
        hashtable_free_full(cp->cache, free_classfile);
    }
    
    /* Free negative cache (values are just sentinel, no need to free) */
    if (cp->negative_cache) {
        hashtable_free(cp->negative_cache);
    }
    
    /* Destroy and free lock */
    if (cp->cache_lock) {
        pthread_mutex_destroy((pthread_mutex_t *)cp->cache_lock);
        free(cp->cache_lock);
    }

    free(cp->java_home);
    free(cp);
}

bool classpath_add_boot(classpath_t *cp, const char *path)
{
    cp_entry_t *entry = cp_entry_new(path);
    if (!entry) {
        return false;
    }
    /* slist_append returns the new node (tail), but we need to keep the head */
    if (!cp->boot_entries) {
        cp->boot_entries = slist_new(entry);
    } else {
        slist_append(cp->boot_entries, entry);
    }
    cp->entry_count++;
    return true;
}

bool classpath_add(classpath_t *cp, const char *path)
{
    cp_entry_t *entry = cp_entry_new(path);
    if (!entry) {
        return false;
    }
    /* slist_append returns the new node (tail), but we need to keep the head */
    if (!cp->entries) {
        cp->entries = slist_new(entry);
    } else {
        slist_append(cp->entries, entry);
    }
    cp->entry_count++;
    return true;
}

bool classpath_add_path(classpath_t *cp, const char *pathstr, bool boot)
{
    if (!pathstr || !*pathstr) {
        return true;
    }

    char *copy = strdup(pathstr);
    if (!copy) {
        return false;
    }

    char *saveptr;
    char *token = strtok_r(copy, ":", &saveptr);
    while (token) {
        if (*token) {
            if (boot) {
                classpath_add_boot(cp, token);
            } else {
                classpath_add(cp, token);
            }
        }
        token = strtok_r(NULL, ":", &saveptr);
    }

    free(copy);
    return true;
}

bool classpath_setup_jdk(classpath_t *cp, jdk_info_t *jdk)
{
    if (!jdk) {
        return false;
    }

    cp->java_home = jdk->java_home ? strdup(jdk->java_home) : NULL;

    if (jdk->rt_jar_path) {
        /* Pre-Java 9: add rt.jar to boot classpath */
        return classpath_add_boot(cp, jdk->rt_jar_path);
    } else if (jdk->modules_path) {
        /* Java 9+: add essential jmod files */
        /* These modules contain the core Java classes */
        static const char *essential_modules[] = {
            "java.base.jmod",       /* Object, String, System, etc. */
            "java.logging.jmod",    /* java.util.logging */
            "java.sql.jmod",        /* java.sql */
            "java.xml.jmod",        /* javax.xml */
            "java.management.jmod", /* java.lang.management */
            "java.compiler.jmod",   /* javax.tools (JavaCompiler, etc.) */
            "java.security.sasl.jmod", /* javax.security.sasl (SASL) */
            "java.security.jgss.jmod", /* org.ietf.jgss (GSSAPI / Kerberos) */
            "java.naming.jmod",     /* javax.naming (JNDI) */
            "java.desktop.jmod",    /* java.beans (BeanInfo, Introspector) */
            NULL
        };
        
        bool any_added = false;
        char jmod_path[1024];
        
        for (int i = 0; essential_modules[i]; i++) {
            snprintf(jmod_path, sizeof(jmod_path), "%s/%s", 
                     jdk->modules_path, essential_modules[i]);
            
            if (file_exists(jmod_path)) {
                if (classpath_add_boot(cp, jmod_path)) {
                    any_added = true;
                }
            }
        }
        
        return any_added;
    }

    return false;
}

/*
 * Class loading
 */

char *classname_to_path(const char *classname)
{
    if (!classname) {
        return NULL;
    }

    size_t len = strlen(classname);
    char *path = malloc(len + 7);  /* +7 for ".class\0" */
    if (!path) {
        return NULL;
    }

    strcpy(path, classname);
    /* Replace . with / */
    for (char *p = path; *p; p++) {
        if (*p == '.') {
            *p = '/';
        }
    }
    strcat(path, ".class");

    return path;
}

static classfile_t *load_from_directory(cp_entry_t *entry, const char *path)
{
    char full_path[1024];
    snprintf(full_path, sizeof(full_path), "%s/%s", entry->path, path);

    return classfile_read_file(full_path);
}

static classfile_t *load_from_jar(cp_entry_t *entry, const char *path)
{
    if (!entry->jar_handle) {
        return NULL;
    }

    size_t size;
    uint8_t *data = jar_read_entry(entry->jar_handle, path, &size);
    if (!data) {
        return NULL;
    }

    classfile_t *cf = classfile_read(data, size);
    free(data);
    return cf;
}

static classfile_t *load_from_jmod(cp_entry_t *entry, const char *path)
{
    if (!entry->jar_handle) {
        return NULL;
    }

    /* JMOD stores classes under classes/: java/lang/String.class is the
     * archive member classes/java/lang/String.class */
    char member[1024];
    int len = snprintf(member, sizeof(member), "classes/%s", path);
    if (len < 0 || (size_t)len >= sizeof(member)) {
        return NULL;
    }

    size_t size;
    uint8_t *data = jar_read_entry(entry->jar_handle, member, &size);
    if (!data) {
        return NULL;
    }

    classfile_t *cf = classfile_read(data, size);
    free(data);
    return cf;
}

static classfile_t *load_from_entry(cp_entry_t *entry, const char *path)
{
    switch (entry->type) {
        case CP_DIRECTORY:
            return load_from_directory(entry, path);
        case CP_JAR:
            return load_from_jar(entry, path);
        case CP_JMOD:
            return load_from_jmod(entry, path);
        default:
            return NULL;
    }
}

static void classpath_index_add(hashtable_t *index, const char *package, cp_entry_t *entry)
{
    cp_package_t *pkg = (cp_package_t *)hashtable_lookup(index, package);
    if (!pkg) {
        pkg = calloc(1, sizeof(cp_package_t));
        if (!pkg) {
            return;
        }
        hashtable_insert(index, package, pkg);
    }
    /* Archives are indexed one at a time, so an entry already listed for
     * this package is the last one */
    if (pkg->count > 0 && pkg->entries[pkg->count - 1] == entry) {
        return;
    }
    if (pkg->count == pkg->capacity) {
        int capacity = pkg->capacity ? pkg->capacity * 2 : 2;
        cp_entry_t **entries = realloc(pkg->entries, (size_t)capacity * sizeof(cp_entry_t *));
        if (!entries) {
            return;
        }
        pkg->entries = entries;
        pkg->capacity = capacity;
    }
    pkg->entries[pkg->count++] = entry;
}

/* Enter every package a JAR/JMOD entry holds classes of */
static void classpath_index_archive(classpath_t *cp, cp_entry_t *entry)
{
    jar_handle_t *jar = (jar_handle_t *)entry->jar_handle;
    if (!jar) {
        return;
    }
    char package[1024];
    for (zip_entry_t *e = jar->entries; e; e = e->next) {
        const char *name = e->filename;
        if (!name) {
            continue;
        }
        if (entry->type == CP_JMOD) {
            /* Classes of a JMOD are under classes/ */
            if (strncmp(name, "classes/", 8) != 0) {
                continue;
            }
            name += 8;
        }
        size_t len = strlen(name);
        if (len < 7 || strcmp(name + len - 6, ".class") != 0) {
            continue;
        }
        const char *slash = strrchr(name, '/');
        size_t package_len = slash ? (size_t)(slash - name) : 0;
        if (package_len >= sizeof(package)) {
            continue;
        }
        memcpy(package, name, package_len);
        package[package_len] = '\0';
        classpath_index_add(cp->package_index, package, entry);
    }
}

/*
 * Whether the package index is there and covers every entry. The caller
 * holds the cache lock.
 */
static bool classpath_package_index_ready(classpath_t *cp)
{
    return cp->package_index && cp->indexed_entry_count == cp->entry_count;
}

/*
 * Build the package index, or rebuild it if entries have been added since.
 * The caller holds the cache lock.
 *
 * A class can only be in an archive that has its package, and the central
 * directory of every archive is already in memory. Without the index each
 * lookup asked every entry in turn - ten JMODs and every JAR of the
 * classpath, each computing its own hash of the member name - and semantic
 * analysis makes a great many lookups for names that turn out not to exist
 * (a simple name tried as a member of each implemented interface, a
 * package prefix tried as a class). Directory entries are not indexed;
 * they are searched in their place in the order (see
 * classpath_dir_files()).
 */
static void classpath_build_package_index(classpath_t *cp)
{
    if (classpath_package_index_ready(cp)) {
        return;
    }

    classpath_free_package_index(cp);
    cp->package_index = hashtable_new_sized(2048);
    cp->dir_entries = calloc(cp->entry_count > 0 ? (size_t)cp->entry_count : 1,
                             sizeof(cp_entry_t *));
    if (!cp->package_index || !cp->dir_entries) {
        return;
    }

    int order = 0;
    slist_t *lists[2];
    lists[0] = cp->boot_entries;
    lists[1] = cp->entries;
    for (int n = 0; n < 2; n++) {
        for (slist_t *node = lists[n]; node; node = node->next) {
            cp_entry_t *entry = (cp_entry_t *)node->data;
            entry->order = order++;
            if (entry->type == CP_DIRECTORY) {
                cp->dir_entries[cp->dir_entry_count++] = entry;
            } else {
                classpath_index_archive(cp, entry);
            }
        }
    }
    cp->indexed_entry_count = cp->entry_count;
}

/*
 * The names of the files in the directory of `package` ("java/util", or ""
 * for the unnamed package) under a directory entry, as a set. The directory
 * is read once, on the first lookup in that package: finding out that a
 * class is not there then costs a hash lookup rather than a failed open().
 * (The output directory and "." are always on the classpath, and are asked
 * for nearly every class the compilation refers to.)
 *
 * The listing is not refreshed. A class file written into the directory
 * later in the same run - the compiler's own output - is not seen, which
 * is as it should be: the classes of this compilation are resolved from
 * their sources, not read back from disk while other threads are still
 * writing them.
 */
static hashtable_t *classpath_dir_files(classpath_t *cp, cp_entry_t *entry, const char *package)
{
    pthread_mutex_t *lock = (pthread_mutex_t *)cp->cache_lock;

    if (lock) {
        pthread_mutex_lock(lock);
    }
    hashtable_t *files = entry->dir_packages ?
        (hashtable_t *)hashtable_lookup(entry->dir_packages, package) : NULL;
    if (lock) {
        pthread_mutex_unlock(lock);
    }
    if (files) {
        return files;
    }

    /* Read the directory without holding the lock */
    char dir_path[1024];
    int len = snprintf(dir_path, sizeof(dir_path), "%s%s%s", entry->path,
                       *package ? "/" : "", package);
    DIR *dir = (len > 0 && (size_t)len < sizeof(dir_path)) ? opendir(dir_path) : NULL;
    if (dir) {
        files = hashtable_new();
        struct dirent *de;
        while (files && (de = readdir(dir)) != NULL) {
            hashtable_insert(files, de->d_name, (void *)1);
        }
        closedir(dir);
    }
    if (!files) {
        files = cp->no_files;
    }

    if (lock) {
        pthread_mutex_lock(lock);
    }
    if (!entry->dir_packages) {
        entry->dir_packages = hashtable_new();
    }
    hashtable_t *existing = entry->dir_packages ?
        (hashtable_t *)hashtable_lookup(entry->dir_packages, package) : NULL;
    if (existing) {
        /* Another thread read it meanwhile */
        if (files != cp->no_files) {
            hashtable_free(files);
        }
        files = existing;
    } else if (entry->dir_packages) {
        hashtable_insert(entry->dir_packages, package, files);
    }
    if (lock) {
        pthread_mutex_unlock(lock);
    }
    return files;
}

classfile_t *classpath_find_class(classpath_t *cp, const char *classname)
{
    pthread_mutex_t *lock = (pthread_mutex_t *)cp->cache_lock;

    /* The class file's path, split into package directory and file name:
     * java.util.Map$Entry is java/util + Map$Entry.class */
    char path_buf[512];
    size_t name_len = strlen(classname);
    char *path = name_len + 7 <= sizeof(path_buf) ? path_buf : malloc(name_len + 7);
    if (!path) {
        return NULL;
    }
    char *slash = NULL;
    for (size_t i = 0; i < name_len; i++) {
        if (classname[i] == '.') {
            path[i] = '/';
            slash = path + i;
        } else {
            path[i] = classname[i];
        }
    }
    memcpy(path + name_len, ".class", 7);
    const char *file = slash ? slash + 1 : path;
    
    /* Lock for cache access */
    if (lock) {
        pthread_mutex_lock(lock);
    }
    
    /* Check positive cache first */
    classfile_t *cached = (classfile_t *)hashtable_lookup(cp->cache, classname);

    /* Check negative cache - avoid repeated failed lookups */
    bool known_missing = !cached && cp->negative_cache &&
                         hashtable_lookup(cp->negative_cache, classname);

    /* Find the archives that hold classes of the package */
    cp_package_t *pkg = NULL;
    bool index_ready = classpath_package_index_ready(cp);
    if (!cached && !known_missing && index_ready) {
        if (slash) {
            *slash = '\0';
        }
        pkg = (cp_package_t *)hashtable_lookup(cp->package_index, slash ? path : "");
        if (slash) {
            *slash = '/';
        }
    }

    if (lock) {
        pthread_mutex_unlock(lock);
    }

    if (cached || known_missing) {
        if (path != path_buf) {
            free(path);
        }
        return cached;
    }

    if (!index_ready) {
        /* First lookup: build the package index */
        if (lock) {
            pthread_mutex_lock(lock);
        }
        classpath_build_package_index(cp);
        if (cp->package_index) {
            if (slash) {
                *slash = '\0';
            }
            pkg = (cp_package_t *)hashtable_lookup(cp->package_index, slash ? path : "");
            if (slash) {
                *slash = '/';
            }
        }
        if (lock) {
            pthread_mutex_unlock(lock);
        }
    }

    classfile_t *cf = NULL;
    bool package_exists = pkg != NULL;

    /* Search in classpath order - boot classpath first, then user
     * classpath - visiting only the archives that have the package, and
     * each directory entry in its turn. The I/O is done without the lock. */
    int archive = 0;
    int directory = 0;
    while (!cf) {
        cp_entry_t *a = (pkg && archive < pkg->count) ? pkg->entries[archive] : NULL;
        cp_entry_t *d = directory < cp->dir_entry_count ? cp->dir_entries[directory] : NULL;
        if (!a && !d) {
            break;
        }
        if (a && (!d || a->order < d->order)) {
            archive++;
            cf = load_from_entry(a, path);
        } else {
            directory++;
            if (slash) {
                *slash = '\0';
            }
            hashtable_t *files = classpath_dir_files(cp, d, slash ? path : "");
            if (slash) {
                *slash = '/';
            }
            if (files != cp->no_files) {
                package_exists = true;
            }
            if (hashtable_contains(files, file)) {
                cf = load_from_entry(d, path);
            }
        }
    }

    if (path != path_buf) {
        free(path);
    }

    /* A name in a package that nothing on the classpath has: establishing
     * that again costs a few hash lookups, less than remembering every
     * such name would. They are by far the most numerous - a qualified
     * name is retried with each of its dots in turn read as a nested-class
     * boundary ("org.example.Outer$Inner" as class "example$Outer$Inner"
     * of package "org", ...), and those packages hold no classes. */
    if (!cf && !package_exists) {
        return NULL;
    }

    /* Lock for cache update */
    if (lock) {
        pthread_mutex_lock(lock);
    }
    
    /* Check cache again - another thread may have loaded it while we were doing I/O */
    classfile_t *existing = (classfile_t *)hashtable_lookup(cp->cache, classname);
    if (existing) {
        /* Another thread loaded it - use that, free our copy */
        if (cf) {
            classfile_free(cf);
        }
        if (lock) {
            pthread_mutex_unlock(lock);
        }
        return existing;
    }

    if (cf) {
        /* Add to positive cache */
        hashtable_insert(cp->cache, classname, cf);
        cp->classes_loaded++;
    } else if (cp->negative_cache) {
        /* Add to negative cache so we don't search again */
        hashtable_insert(cp->negative_cache, classname, (void *)1);
        if (debug_getenv("GENESIS_DEBUG_CLASSPATH_MISS")) {
            fprintf(stderr, "DEBUG classpath miss: %s\n", classname);
        }
    }
    
    if (lock) {
        pthread_mutex_unlock(lock);
    }

    return cf;
}

classfile_t *classpath_load_class(classpath_t *cp, const char *classname)
{
    return classpath_find_class(cp, classname);
}

bool classpath_class_exists(classpath_t *cp, const char *classname)
{
    /* For now, just try to load it */
    /* A more efficient implementation would check without full parsing */
    return classpath_find_class(cp, classname) != NULL;
}

/**
 * Load exported packages from a module's module-info.class in the JDK jmods.
 */
char **classpath_module_exports(classpath_t *cp, const char *module_name, int *count_out)
{
    if (count_out) {
        *count_out = 0;
    }
    if (!cp || !module_name || !cp->java_home) {
        return NULL;
    }
    
    char jmod_path[1024];
    snprintf(jmod_path, sizeof(jmod_path), "%s/jmods/%s.jmod",
             cp->java_home, module_name);
    
    void *handle = jmod_open(jmod_path);
    if (!handle) {
        return NULL;
    }
    
    size_t size = 0;
    uint8_t *data = jmod_read_class(handle, "module-info", &size);
    jmod_close(handle);
    if (!data || size == 0) {
        free(data);
        return NULL;
    }
    
    classfile_t *cf = classfile_read(data, size);
    free(data);
    if (!cf) {
        return NULL;
    }
    
    char **exports = classfile_get_module_exports(cf, count_out);
    classfile_free(cf);
    return exports;
}

/*
 * Utility functions
 */

char *get_package_name(const char *classname)
{
    if (!classname) {
        return NULL;
    }

    const char *last_dot = strrchr(classname, '.');
    if (!last_dot) {
        return strdup("");  /* Default package */
    }

    size_t len = last_dot - classname;
    char *pkg = malloc(len + 1);
    if (pkg) {
        memcpy(pkg, classname, len);
        pkg[len] = '\0';
    }
    return pkg;
}

char *get_simple_name(const char *classname)
{
    if (!classname) {
        return NULL;
    }

    const char *last_dot = strrchr(classname, '.');
    if (!last_dot) {
        return strdup(classname);
    }

    return strdup(last_dot + 1);
}

