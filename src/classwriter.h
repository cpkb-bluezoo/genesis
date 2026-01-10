/*
 * classwriter.h
 * Class file binary output for the genesis Java compiler
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

#ifndef CLASSWRITER_H
#define CLASSWRITER_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/* Forward declarations */
typedef struct class_gen class_gen_t;

/* Class file version (Java 1.1 to avoid StackMapTable requirement) */
#define CLASS_MAJOR_VERSION 45
#define CLASS_MINOR_VERSION 3

/* ========================================================================
 * Class File Writing
 * ======================================================================== */

/**
 * Write a class file to a byte buffer.
 * The caller is responsible for freeing the returned buffer.
 *
 * @param cg   Class generator containing all class data
 * @param size Output parameter for buffer size
 * @return     Allocated buffer containing class file bytes, or NULL on error
 */
uint8_t *write_class_bytes(class_gen_t *cg, size_t *size);

/**
 * Write a class file to disk.
 *
 * @param cg          Class generator containing all class data
 * @param output_path Path to write the .class file
 * @return            true on success, false on error
 */
bool write_class_file(class_gen_t *cg, const char *output_path);

#endif /* CLASSWRITER_H */

