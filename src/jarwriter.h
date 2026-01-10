/*
 * jarwriter.h
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

#ifndef JARWRITER_H
#define JARWRITER_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/* ========================================================================
 * JAR Writer Handle
 * ======================================================================== */

typedef struct jar_writer jar_writer_t;

/**
 * Create a new JAR writer.
 * 
 * @param path        Path for the output JAR file
 * @param main_class  Optional main class for MANIFEST.MF (can be NULL)
 * @return            JAR writer handle, or NULL on error
 */
jar_writer_t *jar_writer_new(const char *path, const char *main_class);

/**
 * Add a class file to the JAR.
 * The entry path is derived from the class name (e.g., "com/example/Foo.class").
 * 
 * @param jw          JAR writer handle
 * @param class_name  Fully qualified class name (e.g., "com.example.Foo")
 * @param data        Class file bytes
 * @param size        Size of class file data
 * @return            true on success, false on error
 */
bool jar_writer_add_class(jar_writer_t *jw, const char *class_name,
                          const uint8_t *data, size_t size);

/**
 * Add a raw entry to the JAR.
 * 
 * @param jw          JAR writer handle
 * @param entry_path  Entry path within the JAR (e.g., "META-INF/services/...")
 * @param data        Entry data
 * @param size        Size of data
 * @return            true on success, false on error
 */
bool jar_writer_add_entry(jar_writer_t *jw, const char *entry_path,
                          const uint8_t *data, size_t size);

/**
 * Finalize and close the JAR file.
 * Writes the central directory and end-of-central-directory records.
 * 
 * @param jw  JAR writer handle (freed by this call)
 * @return    true on success, false on error
 */
bool jar_writer_close(jar_writer_t *jw);

/**
 * Abort JAR writing and clean up.
 * Deletes the partial JAR file.
 * 
 * @param jw  JAR writer handle (freed by this call)
 */
void jar_writer_abort(jar_writer_t *jw);

#endif /* JARWRITER_H */

