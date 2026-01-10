/*
 * encoding.h
 * Source file encoding detection and UTF validation
 * Copyright (C) 2020 Chris Burdess <dog@gnu.org>
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

#ifndef ENCODING_H
#define ENCODING_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

/* ========================================================================
 * Encoding types
 * ======================================================================== */

typedef enum encoding_type {
    ENCODING_UTF8,        /* UTF-8 (default, or BOM EF BB BF) */
    ENCODING_UTF16_BE,    /* UTF-16 Big Endian (BOM FE FF) */
    ENCODING_UTF16_LE,    /* UTF-16 Little Endian (BOM FF FE) */
    ENCODING_UTF32_BE,    /* UTF-32 Big Endian (BOM 00 00 FE FF) */
    ENCODING_UTF32_LE,    /* UTF-32 Little Endian (BOM FF FE 00 00) */
    ENCODING_UNKNOWN      /* Unknown/unsupported encoding */
} encoding_type_t;

/* ========================================================================
 * Encoding detection result
 * ======================================================================== */

typedef struct encoding_result {
    encoding_type_t encoding;   /* Detected encoding */
    size_t bom_length;          /* Length of BOM in bytes (0 if no BOM) */
    bool valid;                 /* True if encoding is supported */
    char *error_msg;            /* Error message if invalid (caller frees) */
    size_t error_offset;        /* Byte offset of error (if any) */
    int error_line;             /* Line number of error (if any) */
    int error_column;           /* Column number of error (if any) */
} encoding_result_t;

/* ========================================================================
 * BOM detection
 * ======================================================================== */

/**
 * Detect encoding from BOM (Byte Order Mark).
 * 
 * @param data      Raw file data
 * @param length    Length of data in bytes
 * @return          Detected encoding and BOM length
 * 
 * If no BOM is present, returns ENCODING_UTF8 with bom_length=0.
 * UTF-32 is checked before UTF-16 because UTF-32 LE BOM starts with FF FE.
 */
encoding_result_t detect_bom(const uint8_t *data, size_t length);

/* ========================================================================
 * UTF-8 validation
 * ======================================================================== */

/**
 * Validate UTF-8 encoding.
 * 
 * @param data      UTF-8 data (after any BOM)
 * @param length    Length of data in bytes
 * @return          Validation result with error details if invalid
 * 
 * Validates that:
 * - All bytes form valid UTF-8 sequences
 * - No overlong encodings
 * - No surrogate pairs (U+D800-U+DFFF)
 * - No code points above U+10FFFF
 */
encoding_result_t validate_utf8(const uint8_t *data, size_t length);

/* ========================================================================
 * UTF-16/32 to UTF-8 conversion
 * ======================================================================== */

/**
 * Convert UTF-16 data to UTF-8.
 * 
 * @param data      UTF-16 data (after BOM)
 * @param length    Length of data in bytes (must be even)
 * @param big_endian  True for UTF-16 BE, false for UTF-16 LE
 * @param out_length  Output: length of converted data
 * @param result     Output: conversion result with error details
 * @return          Newly allocated UTF-8 string, or NULL on error
 * 
 * The returned string is null-terminated and caller must free it.
 */
char *convert_utf16_to_utf8(const uint8_t *data, size_t length,
                            bool big_endian, size_t *out_length,
                            encoding_result_t *result);

/**
 * Convert UTF-32 data to UTF-8.
 * 
 * @param data      UTF-32 data (after BOM)
 * @param length    Length of data in bytes (must be multiple of 4)
 * @param big_endian  True for UTF-32 BE, false for UTF-32 LE
 * @param out_length  Output: length of converted data
 * @param result     Output: conversion result with error details
 * @return          Newly allocated UTF-8 string, or NULL on error
 * 
 * The returned string is null-terminated and caller must free it.
 */
char *convert_utf32_to_utf8(const uint8_t *data, size_t length,
                            bool big_endian, size_t *out_length,
                            encoding_result_t *result);

/* ========================================================================
 * High-level API
 * ======================================================================== */

/**
 * Load and decode source file with encoding detection.
 * 
 * @param raw_data    Raw file data
 * @param raw_length  Length of raw data in bytes
 * @param out_length  Output: length of decoded UTF-8 data
 * @param result      Output: encoding result with error details
 * @return           Newly allocated UTF-8 string, or NULL on error
 * 
 * This function:
 * 1. Detects encoding via BOM (defaults to UTF-8 if no BOM)
 * 2. Converts UTF-16/32 to UTF-8 if needed
 * 3. Validates UTF-8 encoding
 * 4. Returns validated UTF-8 string
 * 
 * On error, result->error_msg contains the error message.
 * The returned string is null-terminated and caller must free it.
 */
char *decode_source_file(const uint8_t *raw_data, size_t raw_length,
                         size_t *out_length, encoding_result_t *result);

/**
 * Free error message in encoding result.
 */
void encoding_result_free(encoding_result_t *result);

#endif /* ENCODING_H */

