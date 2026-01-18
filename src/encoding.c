/*
 * encoding.c
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

#include "encoding.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* ========================================================================
 * UTF-8 encoding helpers
 * ======================================================================== */

/**
 * Encode a Unicode code point as UTF-8.
 * Returns number of bytes written (1-4), or 0 on error.
 */
static int encode_utf8(uint32_t codepoint, uint8_t *out)
{
    if (codepoint <= 0x7F) {
        out[0] = (uint8_t)codepoint;
        return 1;
    } else if (codepoint <= 0x7FF) {
        out[0] = 0xC0 | (codepoint >> 6);
        out[1] = 0x80 | (codepoint & 0x3F);
        return 2;
    } else if (codepoint <= 0xFFFF) {
        /* Check for surrogates (invalid in UTF-8) */
        if (codepoint >= 0xD800 && codepoint <= 0xDFFF) {
            return 0;
        }
        out[0] = 0xE0 | (codepoint >> 12);
        out[1] = 0x80 | ((codepoint >> 6) & 0x3F);
        out[2] = 0x80 | (codepoint & 0x3F);
        return 3;
    } else if (codepoint <= 0x10FFFF) {
        out[0] = 0xF0 | (codepoint >> 18);
        out[1] = 0x80 | ((codepoint >> 12) & 0x3F);
        out[2] = 0x80 | ((codepoint >> 6) & 0x3F);
        out[3] = 0x80 | (codepoint & 0x3F);
        return 4;
    }
    return 0;  /* Code point out of range */
}

/**
 * Calculate line and column from byte offset.
 */
static void calc_line_column(const uint8_t *data, size_t offset,
                             int *line, int *column)
{
    *line = 1;
    *column = 1;
    
    for (size_t i = 0; i < offset; i++) {
        if (data[i] == '\n') {
            (*line)++;
            *column = 1;
        } else {
            (*column)++;
        }
    }
}

/* ========================================================================
 * BOM detection
 * ======================================================================== */

encoding_result_t detect_bom(const uint8_t *data, size_t length)
{
    encoding_result_t result = {
        .encoding = ENCODING_UTF8,
        .bom_length = 0,
        .valid = true,
        .error_msg = NULL,
        .error_offset = 0,
        .error_line = 0,
        .error_column = 0
    };
    
    if (!data || length == 0) {
        return result;
    }
    
    /* Check UTF-32 first (4 bytes) - must check before UTF-16 */
    if (length >= 4) {
        /* UTF-32 BE: 00 00 FE FF */
        if (data[0] == 0x00 && data[1] == 0x00 &&
            data[2] == 0xFE && data[3] == 0xFF) {
            result.encoding = ENCODING_UTF32_BE;
            result.bom_length = 4;
            return result;
        }
        /* UTF-32 LE: FF FE 00 00 */
        if (data[0] == 0xFF && data[1] == 0xFE &&
            data[2] == 0x00 && data[3] == 0x00) {
            result.encoding = ENCODING_UTF32_LE;
            result.bom_length = 4;
            return result;
        }
    }
    
    /* Check UTF-8 BOM (3 bytes) */
    if (length >= 3) {
        if (data[0] == 0xEF && data[1] == 0xBB && data[2] == 0xBF) {
            result.encoding = ENCODING_UTF8;
            result.bom_length = 3;
            return result;
        }
    }
    
    /* Check UTF-16 (2 bytes) */
    if (length >= 2) {
        /* UTF-16 BE: FE FF */
        if (data[0] == 0xFE && data[1] == 0xFF) {
            result.encoding = ENCODING_UTF16_BE;
            result.bom_length = 2;
            return result;
        }
        /* UTF-16 LE: FF FE */
        if (data[0] == 0xFF && data[1] == 0xFE) {
            result.encoding = ENCODING_UTF16_LE;
            result.bom_length = 2;
            return result;
        }
    }
    
    /* No BOM found - default to UTF-8 */
    result.encoding = ENCODING_UTF8;
    result.bom_length = 0;
    return result;
}

/* ========================================================================
 * UTF-8 validation
 * ======================================================================== */

encoding_result_t validate_utf8(const uint8_t *data, size_t length)
{
    encoding_result_t result = {
        .encoding = ENCODING_UTF8,
        .bom_length = 0,
        .valid = true,
        .error_msg = NULL,
        .error_offset = 0,
        .error_line = 0,
        .error_column = 0
    };
    
    if (!data || length == 0) {
        return result;
    }
    
    size_t i = 0;
    while (i < length) {
        uint8_t b = data[i];
        size_t seq_start = i;
        uint32_t codepoint;
        int seq_len;
        
        if (b <= 0x7F) {
            /* ASCII: single byte */
            i++;
            continue;
        } else if ((b & 0xE0) == 0xC0) {
            /* 2-byte sequence: 110xxxxx 10xxxxxx */
            seq_len = 2;
            codepoint = b & 0x1F;
        } else if ((b & 0xF0) == 0xE0) {
            /* 3-byte sequence: 1110xxxx 10xxxxxx 10xxxxxx */
            seq_len = 3;
            codepoint = b & 0x0F;
        } else if ((b & 0xF8) == 0xF0) {
            /* 4-byte sequence: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx */
            seq_len = 4;
            codepoint = b & 0x07;
        } else {
            /* Invalid leading byte */
            result.valid = false;
            result.error_offset = i;
            calc_line_column(data, i, &result.error_line, &result.error_column);
            char msg[128];
            snprintf(msg, sizeof(msg),
                     "invalid UTF-8 byte 0x%02X at offset %zu", b, i);
            result.error_msg = strdup(msg);
            return result;
        }
        
        /* Check we have enough bytes */
        if (i + seq_len > length) {
            result.valid = false;
            result.error_offset = i;
            calc_line_column(data, i, &result.error_line, &result.error_column);
            result.error_msg = strdup("truncated UTF-8 sequence at end of file");
            return result;
        }
        
        /* Read continuation bytes */
        for (int j = 1; j < seq_len; j++) {
            uint8_t cont = data[i + j];
            if ((cont & 0xC0) != 0x80) {
                result.valid = false;
                result.error_offset = i + j;
                calc_line_column(data, i + j, &result.error_line, &result.error_column);
                char msg[128];
                snprintf(msg, sizeof(msg),
                         "invalid UTF-8 continuation byte 0x%02X at offset %zu",
                         cont, i + j);
                result.error_msg = strdup(msg);
                return result;
            }
            codepoint = (codepoint << 6) | (cont & 0x3F);
        }
        
        /* Check for overlong encoding */
        bool overlong = false;
        switch (seq_len) {
            case 2: overlong = (codepoint < 0x80); break;
            case 3: overlong = (codepoint < 0x800); break;
            case 4: overlong = (codepoint < 0x10000); break;
        }
        if (overlong) {
            result.valid = false;
            result.error_offset = seq_start;
            calc_line_column(data, seq_start, &result.error_line, &result.error_column);
            char msg[128];
            snprintf(msg, sizeof(msg),
                     "overlong UTF-8 encoding for U+%04X at offset %zu",
                     codepoint, seq_start);
            result.error_msg = strdup(msg);
            return result;
        }
        
        /* Check for surrogates (U+D800-U+DFFF) */
        if (codepoint >= 0xD800 && codepoint <= 0xDFFF) {
            result.valid = false;
            result.error_offset = seq_start;
            calc_line_column(data, seq_start, &result.error_line, &result.error_column);
            char msg[128];
            snprintf(msg, sizeof(msg),
                     "invalid surrogate code point U+%04X at offset %zu",
                     codepoint, seq_start);
            result.error_msg = strdup(msg);
            return result;
        }
        
        /* Check for code points above U+10FFFF */
        if (codepoint > 0x10FFFF) {
            result.valid = false;
            result.error_offset = seq_start;
            calc_line_column(data, seq_start, &result.error_line, &result.error_column);
            char msg[128];
            snprintf(msg, sizeof(msg),
                     "code point U+%X exceeds maximum U+10FFFF at offset %zu",
                     codepoint, seq_start);
            result.error_msg = strdup(msg);
            return result;
        }
        
        i += seq_len;
    }
    
    return result;
}

/* ========================================================================
 * UTF-16 to UTF-8 conversion
 * ======================================================================== */

char *convert_utf16_to_utf8(const uint8_t *data, size_t length,
                            bool big_endian, size_t *out_length,
                            encoding_result_t *result)
{
    result->encoding = big_endian ? ENCODING_UTF16_BE : ENCODING_UTF16_LE;
    result->bom_length = 0;
    result->valid = true;
    result->error_msg = NULL;
    result->error_offset = 0;
    result->error_line = 0;
    result->error_column = 0;
    
    /* Check length is even */
    if (length % 2 != 0) {
        result->valid = false;
        result->error_offset = length - 1;
        result->error_msg = strdup("UTF-16 data has odd byte count");
        return NULL;
    }
    
    /* Allocate output buffer (worst case: 3 bytes per code unit + null) */
    size_t max_size = (length / 2) * 3 + 1;
    char *output = malloc(max_size);
    if (!output) {
        result->valid = false;
        result->error_msg = strdup("out of memory");
        return NULL;
    }
    
    size_t out_pos = 0;
    size_t i = 0;
    int line = 1, column = 1;
    
    while (i < length) {
        /* Read UTF-16 code unit */
        uint16_t unit;
        if (big_endian) {
            unit = ((uint16_t)data[i] << 8) | data[i + 1];
        } else {
            unit = ((uint16_t)data[i + 1] << 8) | data[i];
        }
        
        uint32_t codepoint;
        
        /* Check for surrogate pair */
        if (unit >= 0xD800 && unit <= 0xDBFF) {
            /* High surrogate - must be followed by low surrogate */
            if (i + 4 > length) {
                result->valid = false;
                result->error_offset = i;
                result->error_line = line;
                result->error_column = column;
                result->error_msg = strdup("truncated surrogate pair at end of file");
                free(output);
                return NULL;
            }
            
            uint16_t low;
            if (big_endian) {
                low = ((uint16_t)data[i + 2] << 8) | data[i + 3];
            } else {
                low = ((uint16_t)data[i + 3] << 8) | data[i + 2];
            }
            
            if (low < 0xDC00 || low > 0xDFFF) {
                result->valid = false;
                result->error_offset = i + 2;
                result->error_line = line;
                result->error_column = column;
                char msg[128];
                snprintf(msg, sizeof(msg),
                         "invalid low surrogate 0x%04X at offset %zu", low, i + 2);
                result->error_msg = strdup(msg);
                free(output);
                return NULL;
            }
            
            /* Decode surrogate pair */
            codepoint = 0x10000 + (((uint32_t)(unit - 0xD800) << 10) |
                                   (low - 0xDC00));
            i += 4;
        } else if (unit >= 0xDC00 && unit <= 0xDFFF) {
            /* Unpaired low surrogate */
            result->valid = false;
            result->error_offset = i;
            result->error_line = line;
            result->error_column = column;
            char msg[128];
            snprintf(msg, sizeof(msg),
                     "unpaired low surrogate 0x%04X at offset %zu", unit, i);
            result->error_msg = strdup(msg);
            free(output);
            return NULL;
        } else {
            codepoint = unit;
            i += 2;
        }
        
        /* Encode as UTF-8 */
        int utf8_len = encode_utf8(codepoint, (uint8_t *)(output + out_pos));
        if (utf8_len == 0) {
            result->valid = false;
            result->error_offset = i - 2;
            result->error_line = line;
            result->error_column = column;
            char msg[128];
            snprintf(msg, sizeof(msg),
                     "invalid code point U+%X at offset %zu", codepoint, i - 2);
            result->error_msg = strdup(msg);
            free(output);
            return NULL;
        }
        out_pos += utf8_len;
        
        /* Track line/column for error reporting */
        if (codepoint == '\n') {
            line++;
            column = 1;
        } else {
            column++;
        }
    }
    
    output[out_pos] = '\0';
    *out_length = out_pos;
    return output;
}

/* ========================================================================
 * UTF-32 to UTF-8 conversion
 * ======================================================================== */

char *convert_utf32_to_utf8(const uint8_t *data, size_t length,
                            bool big_endian, size_t *out_length,
                            encoding_result_t *result)
{
    result->encoding = big_endian ? ENCODING_UTF32_BE : ENCODING_UTF32_LE;
    result->bom_length = 0;
    result->valid = true;
    result->error_msg = NULL;
    result->error_offset = 0;
    result->error_line = 0;
    result->error_column = 0;
    
    /* Check length is multiple of 4 */
    if (length % 4 != 0) {
        result->valid = false;
        result->error_offset = length - (length % 4);
        result->error_msg = strdup("UTF-32 data length is not a multiple of 4");
        return NULL;
    }
    
    /* Allocate output buffer (worst case: 4 bytes per code point + null) */
    size_t max_size = length + 1;
    char *output = malloc(max_size);
    if (!output) {
        result->valid = false;
        result->error_msg = strdup("out of memory");
        return NULL;
    }
    
    size_t out_pos = 0;
    size_t i = 0;
    int line = 1, column = 1;
    
    while (i < length) {
        /* Read UTF-32 code point */
        uint32_t codepoint;
        if (big_endian) {
            codepoint = ((uint32_t)data[i] << 24) |
                        ((uint32_t)data[i + 1] << 16) |
                        ((uint32_t)data[i + 2] << 8) |
                        data[i + 3];
        } else {
            codepoint = ((uint32_t)data[i + 3] << 24) |
                        ((uint32_t)data[i + 2] << 16) |
                        ((uint32_t)data[i + 1] << 8) |
                        data[i];
        }
        
        /* Check for surrogates */
        if (codepoint >= 0xD800 && codepoint <= 0xDFFF) {
            result->valid = false;
            result->error_offset = i;
            result->error_line = line;
            result->error_column = column;
            char msg[128];
            snprintf(msg, sizeof(msg),
                     "invalid surrogate code point U+%04X at offset %zu",
                     codepoint, i);
            result->error_msg = strdup(msg);
            free(output);
            return NULL;
        }
        
        /* Check for valid range */
        if (codepoint > 0x10FFFF) {
            result->valid = false;
            result->error_offset = i;
            result->error_line = line;
            result->error_column = column;
            char msg[128];
            snprintf(msg, sizeof(msg),
                     "code point U+%X exceeds maximum U+10FFFF at offset %zu",
                     codepoint, i);
            result->error_msg = strdup(msg);
            free(output);
            return NULL;
        }
        
        /* Encode as UTF-8 */
        int utf8_len = encode_utf8(codepoint, (uint8_t *)(output + out_pos));
        if (utf8_len == 0) {
            result->valid = false;
            result->error_offset = i;
            result->error_line = line;
            result->error_column = column;
            char msg[128];
            snprintf(msg, sizeof(msg),
                     "failed to encode U+%X at offset %zu", codepoint, i);
            result->error_msg = strdup(msg);
            free(output);
            return NULL;
        }
        out_pos += utf8_len;
        i += 4;
        
        /* Track line/column for error reporting */
        if (codepoint == '\n') {
            line++;
            column = 1;
        } else {
            column++;
        }
    }
    
    output[out_pos] = '\0';
    *out_length = out_pos;
    return output;
}

/* ========================================================================
 * High-level API
 * ======================================================================== */

char *decode_source_file(const uint8_t *raw_data, size_t raw_length,
                         size_t *out_length, encoding_result_t *result)
{
    /* Initialize result */
    result->encoding = ENCODING_UTF8;
    result->bom_length = 0;
    result->valid = true;
    result->error_msg = NULL;
    result->error_offset = 0;
    result->error_line = 0;
    result->error_column = 0;
    
    if (!raw_data || raw_length == 0) {
        *out_length = 0;
        char *empty = malloc(1);
        if (empty) {
            empty[0] = '\0';
        }
        return empty;
    }
    
    /* Detect BOM */
    encoding_result_t bom_result = detect_bom(raw_data, raw_length);
    result->encoding = bom_result.encoding;
    result->bom_length = bom_result.bom_length;
    
    /* Get data after BOM */
    const uint8_t *content_data = raw_data + bom_result.bom_length;
    size_t content_length = raw_length - bom_result.bom_length;
    
    char *output = NULL;
    
    switch (bom_result.encoding) {
        case ENCODING_UTF8:
            /* Validate UTF-8 */
            *result = validate_utf8(content_data, content_length);
            result->bom_length = bom_result.bom_length;
            
            if (!result->valid) {
                return NULL;
            }
            
            /* Copy the UTF-8 data */
            output = malloc(content_length + 1);
            if (!output) {
                result->valid = false;
                result->error_msg = strdup("out of memory");
                return NULL;
            }
            memcpy(output, content_data, content_length);
            output[content_length] = '\0';
            *out_length = content_length;
            break;
            
        case ENCODING_UTF16_BE:
        case ENCODING_UTF16_LE:
            output = convert_utf16_to_utf8(
                content_data, content_length,
                bom_result.encoding == ENCODING_UTF16_BE,
                out_length, result);
            result->bom_length = bom_result.bom_length;
            break;
            
        case ENCODING_UTF32_BE:
        case ENCODING_UTF32_LE:
            output = convert_utf32_to_utf8(
                content_data, content_length,
                bom_result.encoding == ENCODING_UTF32_BE,
                out_length, result);
            result->bom_length = bom_result.bom_length;
            break;
            
        default:
            result->valid = false;
            result->error_msg = strdup("unsupported encoding");
            return NULL;
    }
    
    return output;
}

void encoding_result_free(encoding_result_t *result)
{
    if (result) {
        free(result->error_msg);
        result->error_msg = NULL;
    }
}

