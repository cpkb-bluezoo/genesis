/*
 * jarwriter.c
 * JAR file output for the genesis Java compiler
 * Copyright (C) 2026 Chris Burdess <dog@gnu.org>
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

#include "jarwriter.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include <zlib.h>

/* ========================================================================
 * ZIP format constants
 * ======================================================================== */

#define ZIP_LOCAL_FILE_HEADER_SIG       0x04034b50
#define ZIP_CENTRAL_DIR_HEADER_SIG      0x02014b50
#define ZIP_END_CENTRAL_DIR_SIG         0x06054b50

#define ZIP_VERSION_NEEDED              20  /* 2.0 for deflate */
#define ZIP_VERSION_MADE_BY             ((3 << 8) | 20)  /* Unix, version 2.0 */

#define ZIP_METHOD_STORED               0
#define ZIP_METHOD_DEFLATED             8

#define ZIP_FLAG_UTF8                   (1 << 11)

/* ========================================================================
 * Internal structures
 * ======================================================================== */

typedef struct jar_entry {
    char *name;                 /* Entry path (e.g., "com/example/Foo.class") */
    uint32_t crc32;             /* CRC-32 of uncompressed data */
    uint32_t compressed_size;   /* Compressed size */
    uint32_t uncompressed_size; /* Uncompressed size */
    uint32_t local_header_offset; /* Offset to local file header */
    uint16_t compression_method;  /* 0=stored, 8=deflated */
    struct jar_entry *next;
} jar_entry_t;

struct jar_writer {
    FILE *fp;                   /* Output file */
    char *path;                 /* Output path (for cleanup on abort) */
    jar_entry_t *entries;       /* Linked list of entries */
    jar_entry_t *entries_tail;  /* Tail for efficient append */
    uint16_t entry_count;       /* Number of entries */
    uint16_t mod_time;          /* DOS modification time */
    uint16_t mod_date;          /* DOS modification date */
};

/* ========================================================================
 * Helper functions
 * ======================================================================== */

static void write_le16(FILE *fp, uint16_t val)
{
    fputc(val & 0xFF, fp);
    fputc((val >> 8) & 0xFF, fp);
}

static void write_le32(FILE *fp, uint32_t val)
{
    fputc(val & 0xFF, fp);
    fputc((val >> 8) & 0xFF, fp);
    fputc((val >> 16) & 0xFF, fp);
    fputc((val >> 24) & 0xFF, fp);
}

/**
 * Convert time_t to DOS date/time format.
 */
static void time_to_dos(time_t t, uint16_t *date, uint16_t *time_val)
{
    struct tm *tm = localtime(&t);
    if (!tm) {
        *date = 0;
        *time_val = 0;
        return;
    }
    
    /* DOS time: bits 0-4=seconds/2, 5-10=minutes, 11-15=hours */
    *time_val = (tm->tm_sec / 2) | (tm->tm_min << 5) | (tm->tm_hour << 11);
    
    /* DOS date: bits 0-4=day, 5-8=month, 9-15=year-1980 */
    *date = tm->tm_mday | ((tm->tm_mon + 1) << 5) | ((tm->tm_year - 80) << 9);
}

/**
 * Compress data using deflate.
 * Returns compressed data (caller must free) or NULL on error.
 */
static uint8_t *deflate_data(const uint8_t *data, size_t size,
                             size_t *compressed_size)
{
    /* Allocate output buffer (worst case is slightly larger than input) */
    uLong bound = compressBound(size);
    uint8_t *compressed = malloc(bound);
    if (!compressed) {
        return NULL;
    }
    
    /* Use raw deflate (no zlib header) for ZIP compatibility */
    z_stream strm;
    memset(&strm, 0, sizeof(strm));
    
    if (deflateInit2(&strm, Z_DEFAULT_COMPRESSION, Z_DEFLATED,
                     -MAX_WBITS, 8, Z_DEFAULT_STRATEGY) != Z_OK) {
        free(compressed);
        return NULL;
    }
    
    strm.next_in = (Bytef *)data;
    strm.avail_in = size;
    strm.next_out = compressed;
    strm.avail_out = bound;
    
    int ret = deflate(&strm, Z_FINISH);
    deflateEnd(&strm);
    
    if (ret != Z_STREAM_END) {
        free(compressed);
        return NULL;
    }
    
    *compressed_size = strm.total_out;
    return compressed;
}

/* ========================================================================
 * JAR Writer Implementation
 * ======================================================================== */

jar_writer_t *jar_writer_new(const char *path, const char *main_class)
{
    jar_writer_t *jw = calloc(1, sizeof(jar_writer_t));
    if (!jw) {
        return NULL;
    }
    
    jw->path = strdup(path);
    if (!jw->path) {
        free(jw);
        return NULL;
    }
    
    jw->fp = fopen(path, "wb");
    if (!jw->fp) {
        free(jw->path);
        free(jw);
        return NULL;
    }
    
    /* Set modification time to now */
    time_to_dos(time(NULL), &jw->mod_date, &jw->mod_time);
    
    /* Create manifest */
    char manifest[1024];
    int len;
    if (main_class) {
        len = snprintf(manifest, sizeof(manifest),
                       "Manifest-Version: 1.0\r\n"
                       "Created-By: genesis\r\n"
                       "Main-Class: %s\r\n"
                       "\r\n",
                       main_class);
    } else {
        len = snprintf(manifest, sizeof(manifest),
                       "Manifest-Version: 1.0\r\n"
                       "Created-By: genesis\r\n"
                       "\r\n");
    }
    
    /* Add manifest as first entry */
    if (!jar_writer_add_entry(jw, "META-INF/MANIFEST.MF",
                              (const uint8_t *)manifest, len)) {
        fclose(jw->fp);
        remove(path);
        free(jw->path);
        free(jw);
        return NULL;
    }
    
    return jw;
}

bool jar_writer_add_entry(jar_writer_t *jw, const char *entry_path,
                          const uint8_t *data, size_t size)
{
    if (!jw || !jw->fp || !entry_path || !data) {
        return false;
    }
    
    /* Calculate CRC-32 */
    uint32_t crc = crc32(0, data, size);
    
    /* Try to compress */
    size_t compressed_size;
    uint8_t *compressed = deflate_data(data, size, &compressed_size);
    
    uint16_t method;
    const uint8_t *write_data;
    size_t write_size;
    
    /* Use compressed data only if it's actually smaller */
    if (compressed && compressed_size < size) {
        method = ZIP_METHOD_DEFLATED;
        write_data = compressed;
        write_size = compressed_size;
    } else {
        method = ZIP_METHOD_STORED;
        write_data = data;
        write_size = size;
        free(compressed);
        compressed = NULL;
    }
    
    /* Create entry record */
    jar_entry_t *entry = calloc(1, sizeof(jar_entry_t));
    if (!entry) {
        free(compressed);
        return false;
    }
    
    entry->name = strdup(entry_path);
    if (!entry->name) {
        free(entry);
        free(compressed);
        return false;
    }
    
    entry->crc32 = crc;
    entry->compressed_size = write_size;
    entry->uncompressed_size = size;
    entry->compression_method = method;
    entry->local_header_offset = ftell(jw->fp);
    
    /* Write local file header */
    size_t name_len = strlen(entry_path);
    
    write_le32(jw->fp, ZIP_LOCAL_FILE_HEADER_SIG);
    write_le16(jw->fp, ZIP_VERSION_NEEDED);
    write_le16(jw->fp, ZIP_FLAG_UTF8);
    write_le16(jw->fp, method);
    write_le16(jw->fp, jw->mod_time);
    write_le16(jw->fp, jw->mod_date);
    write_le32(jw->fp, crc);
    write_le32(jw->fp, write_size);
    write_le32(jw->fp, size);
    write_le16(jw->fp, name_len);
    write_le16(jw->fp, 0);  /* Extra field length */
    fwrite(entry_path, 1, name_len, jw->fp);
    
    /* Write file data */
    fwrite(write_data, 1, write_size, jw->fp);
    
    free(compressed);
    
    /* Add to entry list */
    if (jw->entries_tail) {
        jw->entries_tail->next = entry;
    } else {
        jw->entries = entry;
    }
    jw->entries_tail = entry;
    jw->entry_count++;
    
    return true;
}

bool jar_writer_add_class(jar_writer_t *jw, const char *class_name,
                          const uint8_t *data, size_t size)
{
    if (!jw || !class_name) {
        return false;
    }
    
    /* Convert class name to entry path (com.example.Foo -> com/example/Foo.class) */
    size_t name_len = strlen(class_name);
    char *entry_path = malloc(name_len + 7);  /* +7 for ".class\0" */
    if (!entry_path) {
        return false;
    }
    
    strcpy(entry_path, class_name);
    for (char *p = entry_path; *p; p++) {
        if (*p == '.') {
            *p = '/';
        }
    }
    strcat(entry_path, ".class");
    
    bool result = jar_writer_add_entry(jw, entry_path, data, size);
    free(entry_path);
    return result;
}

bool jar_writer_close(jar_writer_t *jw)
{
    if (!jw) {
        return false;
    }
    
    if (!jw->fp) {
        free(jw->path);
        free(jw);
        return false;
    }
    
    /* Record start of central directory */
    long central_dir_offset = ftell(jw->fp);
    
    /* Write central directory entries */
    for (jar_entry_t *e = jw->entries; e; e = e->next) {
        size_t name_len = strlen(e->name);
        
        write_le32(jw->fp, ZIP_CENTRAL_DIR_HEADER_SIG);
        write_le16(jw->fp, ZIP_VERSION_MADE_BY);
        write_le16(jw->fp, ZIP_VERSION_NEEDED);
        write_le16(jw->fp, ZIP_FLAG_UTF8);
        write_le16(jw->fp, e->compression_method);
        write_le16(jw->fp, jw->mod_time);
        write_le16(jw->fp, jw->mod_date);
        write_le32(jw->fp, e->crc32);
        write_le32(jw->fp, e->compressed_size);
        write_le32(jw->fp, e->uncompressed_size);
        write_le16(jw->fp, name_len);
        write_le16(jw->fp, 0);  /* Extra field length */
        write_le16(jw->fp, 0);  /* Comment length */
        write_le16(jw->fp, 0);  /* Disk number start */
        write_le16(jw->fp, 0);  /* Internal file attributes */
        write_le32(jw->fp, 0);  /* External file attributes */
        write_le32(jw->fp, e->local_header_offset);
        fwrite(e->name, 1, name_len, jw->fp);
    }
    
    /* Calculate central directory size */
    long central_dir_size = ftell(jw->fp) - central_dir_offset;
    
    /* Write end of central directory */
    write_le32(jw->fp, ZIP_END_CENTRAL_DIR_SIG);
    write_le16(jw->fp, 0);  /* Disk number */
    write_le16(jw->fp, 0);  /* Disk with central dir */
    write_le16(jw->fp, jw->entry_count);
    write_le16(jw->fp, jw->entry_count);
    write_le32(jw->fp, central_dir_size);
    write_le32(jw->fp, central_dir_offset);
    write_le16(jw->fp, 0);  /* Comment length */
    
    fclose(jw->fp);
    jw->fp = NULL;
    
    /* Free entries */
    jar_entry_t *e = jw->entries;
    while (e) {
        jar_entry_t *next = e->next;
        free(e->name);
        free(e);
        e = next;
    }
    
    free(jw->path);
    free(jw);
    return true;
}

void jar_writer_abort(jar_writer_t *jw)
{
    if (!jw) {
        return;
    }
    
    if (jw->fp) {
        fclose(jw->fp);
        remove(jw->path);
    }
    
    /* Free entries */
    jar_entry_t *e = jw->entries;
    while (e) {
        jar_entry_t *next = e->next;
        free(e->name);
        free(e);
        e = next;
    }
    
    free(jw->path);
    free(jw);
}

