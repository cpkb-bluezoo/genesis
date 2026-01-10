/*
 * lexer.c
 * Lexical analyzer for Java source files
 * Copyright (C) 2016, 2020, 2026 Chris Burdess <dog@gnu.org>
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

#include "genesis.h"

/* ========================================================================
 * Token helper functions
 * ======================================================================== */

/**
 * Set current token with text pointing into source.
 */
static void lexer_set_token(lexer_t *lexer, token_type_t type,
                           const char *text_start, size_t text_len,
                           int line, int column)
{
    lexer->token.type = type;
    lexer->token.text_start = text_start;
    lexer->token.text_len = text_len;
    lexer->token.line = line;
    lexer->token.column = column;
    lexer->token.value.int_value = 0;
}

/**
 * Set current token using internal text buffer (for processed strings).
 */
static void lexer_set_token_buf(lexer_t *lexer, token_type_t type,
                               const char *text, size_t text_len,
                               int line, int column)
{
    /* Copy text to internal buffer */
    if (text_len >= sizeof(lexer->text_buf)) {
        text_len = sizeof(lexer->text_buf) - 1;
    }
    memcpy(lexer->text_buf, text, text_len);
    lexer->text_buf[text_len] = '\0';
    
    lexer->token.type = type;
    lexer->token.text_start = lexer->text_buf;
    lexer->token.text_len = text_len;
    lexer->token.line = line;
    lexer->token.column = column;
    lexer->token.value.int_value = 0;
}

/**
 * Set current token with a static string (for operators, punctuation).
 */
static void lexer_set_token_static(lexer_t *lexer, token_type_t type,
                                  const char *static_text,
                                  int line, int column)
{
    lexer->token.type = type;
    lexer->token.text_start = static_text;
    lexer->token.text_len = strlen(static_text);
    lexer->token.line = line;
    lexer->token.column = column;
    lexer->token.value.int_value = 0;
}

/**
 * Get the name of a token type.
 */
const char *token_type_name(token_type_t type)
{
    switch (type) {
        case TOK_EOF:               return "EOF";
        case TOK_IDENTIFIER:        return "IDENTIFIER";
        case TOK_INTEGER_LITERAL:   return "INTEGER_LITERAL";
        case TOK_LONG_LITERAL:      return "LONG_LITERAL";
        case TOK_FLOAT_LITERAL:     return "FLOAT_LITERAL";
        case TOK_DOUBLE_LITERAL:    return "DOUBLE_LITERAL";
        case TOK_CHAR_LITERAL:      return "CHAR_LITERAL";
        case TOK_STRING_LITERAL:    return "STRING_LITERAL";
        case TOK_TEXT_BLOCK:        return "TEXT_BLOCK";
        case TOK_TRUE:              return "true";
        case TOK_FALSE:             return "false";
        case TOK_NULL:              return "null";
        
        /* Keywords */
        case TOK_ABSTRACT:          return "abstract";
        case TOK_ASSERT:            return "assert";
        case TOK_BOOLEAN:           return "boolean";
        case TOK_BREAK:             return "break";
        case TOK_BYTE:              return "byte";
        case TOK_CASE:              return "case";
        case TOK_CATCH:             return "catch";
        case TOK_CHAR:              return "char";
        case TOK_CLASS:             return "class";
        case TOK_CONST:             return "const";
        case TOK_CONTINUE:          return "continue";
        case TOK_DEFAULT:           return "default";
        case TOK_DO:                return "do";
        case TOK_DOUBLE:            return "double";
        case TOK_ELSE:              return "else";
        case TOK_ENUM:              return "enum";
        case TOK_EXTENDS:           return "extends";
        case TOK_FINAL:             return "final";
        case TOK_FINALLY:           return "finally";
        case TOK_FLOAT:             return "float";
        case TOK_FOR:               return "for";
        case TOK_GOTO:              return "goto";
        case TOK_IF:                return "if";
        case TOK_IMPLEMENTS:        return "implements";
        case TOK_IMPORT:            return "import";
        case TOK_INSTANCEOF:        return "instanceof";
        case TOK_INT:               return "int";
        case TOK_INTERFACE:         return "interface";
        case TOK_LONG:              return "long";
        case TOK_NATIVE:            return "native";
        case TOK_NEW:               return "new";
        case TOK_PACKAGE:           return "package";
        case TOK_PRIVATE:           return "private";
        case TOK_PROTECTED:         return "protected";
        case TOK_PUBLIC:            return "public";
        case TOK_RETURN:            return "return";
        case TOK_SHORT:             return "short";
        case TOK_STATIC:            return "static";
        case TOK_STRICTFP:          return "strictfp";
        case TOK_SUPER:             return "super";
        case TOK_SWITCH:            return "switch";
        case TOK_SYNCHRONIZED:      return "synchronized";
        case TOK_THIS:              return "this";
        case TOK_THROW:             return "throw";
        case TOK_THROWS:            return "throws";
        case TOK_TRANSIENT:         return "transient";
        case TOK_TRY:               return "try";
        case TOK_VOID:              return "void";
        case TOK_VOLATILE:          return "volatile";
        case TOK_WHILE:             return "while";
        
        /* Contextual keywords */
        case TOK_VAR:               return "var";
        case TOK_YIELD:             return "yield";
        case TOK_RECORD:            return "record";
        case TOK_SEALED:            return "sealed";
        case TOK_NON_SEALED:        return "non-sealed";
        case TOK_PERMITS:           return "permits";
        case TOK_WHEN:              return "when";
        
        /* Module keywords (Java 9+) */
        case TOK_MODULE:            return "module";
        case TOK_REQUIRES:          return "requires";
        case TOK_EXPORTS:           return "exports";
        case TOK_OPENS:             return "opens";
        case TOK_USES:              return "uses";
        case TOK_PROVIDES:          return "provides";
        case TOK_WITH:              return "with";
        case TOK_TO:                return "to";
        case TOK_TRANSITIVE:        return "transitive";
        case TOK_OPEN:              return "open";
        
        /* Separators */
        case TOK_LPAREN:            return "(";
        case TOK_RPAREN:            return ")";
        case TOK_LBRACE:            return "{";
        case TOK_RBRACE:            return "}";
        case TOK_LBRACKET:          return "[";
        case TOK_RBRACKET:          return "]";
        case TOK_SEMICOLON:         return ";";
        case TOK_COMMA:             return ",";
        case TOK_DOT:               return ".";
        case TOK_ELLIPSIS:          return "...";
        case TOK_AT:                return "@";
        case TOK_DOUBLE_COLON:      return "::";
        
        /* Operators */
        case TOK_ASSIGN:            return "=";
        case TOK_GT:                return ">";
        case TOK_LT:                return "<";
        case TOK_NOT:               return "!";
        case TOK_TILDE:             return "~";
        case TOK_QUESTION:          return "?";
        case TOK_COLON:             return ":";
        case TOK_ARROW:             return "->";
        case TOK_EQ:                return "==";
        case TOK_GE:                return ">=";
        case TOK_LE:                return "<=";
        case TOK_NE:                return "!=";
        case TOK_AND:               return "&&";
        case TOK_OR:                return "||";
        case TOK_INC:               return "++";
        case TOK_DEC:               return "--";
        case TOK_PLUS:              return "+";
        case TOK_MINUS:             return "-";
        case TOK_STAR:              return "*";
        case TOK_SLASH:             return "/";
        case TOK_BITAND:            return "&";
        case TOK_BITOR:             return "|";
        case TOK_CARET:             return "^";
        case TOK_MOD:               return "%";
        case TOK_LSHIFT:            return "<<";
        case TOK_RSHIFT:            return ">>";
        case TOK_URSHIFT:           return ">>>";
        case TOK_PLUS_ASSIGN:       return "+=";
        case TOK_MINUS_ASSIGN:      return "-=";
        case TOK_STAR_ASSIGN:       return "*=";
        case TOK_SLASH_ASSIGN:      return "/=";
        case TOK_AND_ASSIGN:        return "&=";
        case TOK_OR_ASSIGN:         return "|=";
        case TOK_XOR_ASSIGN:        return "^=";
        case TOK_MOD_ASSIGN:        return "%=";
        case TOK_LSHIFT_ASSIGN:     return "<<=";
        case TOK_RSHIFT_ASSIGN:     return ">>=";
        case TOK_URSHIFT_ASSIGN:    return ">>>=";
        
        case TOK_ERROR:             return "ERROR";
        case TOK_COMMENT:           return "COMMENT";
        default:                    return "UNKNOWN";
    }
}

/* ========================================================================
 * Keyword lookup table
 * ======================================================================== */

typedef struct keyword_entry
{
    const char *name;
    token_type_t type;
} keyword_entry_t;

static keyword_entry_t keywords[] = {
    {"abstract",     TOK_ABSTRACT},
    {"assert",       TOK_ASSERT},
    {"boolean",      TOK_BOOLEAN},
    {"break",        TOK_BREAK},
    {"byte",         TOK_BYTE},
    {"case",         TOK_CASE},
    {"catch",        TOK_CATCH},
    {"char",         TOK_CHAR},
    {"class",        TOK_CLASS},
    {"const",        TOK_CONST},
    {"continue",     TOK_CONTINUE},
    {"default",      TOK_DEFAULT},
    {"do",           TOK_DO},
    {"double",       TOK_DOUBLE},
    {"else",         TOK_ELSE},
    {"enum",         TOK_ENUM},
    {"extends",      TOK_EXTENDS},
    {"false",        TOK_FALSE},
    {"final",        TOK_FINAL},
    {"finally",      TOK_FINALLY},
    {"float",        TOK_FLOAT},
    {"for",          TOK_FOR},
    {"goto",         TOK_GOTO},
    {"if",           TOK_IF},
    {"implements",   TOK_IMPLEMENTS},
    {"import",       TOK_IMPORT},
    {"instanceof",   TOK_INSTANCEOF},
    {"int",          TOK_INT},
    {"interface",    TOK_INTERFACE},
    {"long",         TOK_LONG},
    {"native",       TOK_NATIVE},
    {"new",          TOK_NEW},
    {"null",         TOK_NULL},
    {"package",      TOK_PACKAGE},
    {"private",      TOK_PRIVATE},
    {"protected",    TOK_PROTECTED},
    {"public",       TOK_PUBLIC},
    {"record",       TOK_RECORD},
    {"return",       TOK_RETURN},
    {"short",        TOK_SHORT},
    {"static",       TOK_STATIC},
    {"strictfp",     TOK_STRICTFP},
    {"super",        TOK_SUPER},
    {"switch",       TOK_SWITCH},
    {"synchronized", TOK_SYNCHRONIZED},
    {"this",         TOK_THIS},
    {"throw",        TOK_THROW},
    {"throws",       TOK_THROWS},
    {"transient",    TOK_TRANSIENT},
    {"true",         TOK_TRUE},
    {"try",          TOK_TRY},
    {"var",          TOK_VAR},
    {"void",         TOK_VOID},
    {"volatile",     TOK_VOLATILE},
    {"while",        TOK_WHILE},
    {"yield",        TOK_YIELD},
    /* Sealed class keywords (Java 17+) */
    {"sealed",       TOK_SEALED},
    {"permits",      TOK_PERMITS},
    /* Note: "non-sealed" handled specially in lexer_scan_identifier */
    /* Pattern matching (Java 21+) */
    {"when",         TOK_WHEN},
    /* Module keywords (Java 9+) - contextual, but we lex them as keywords */
    {"exports",      TOK_EXPORTS},
    {"module",       TOK_MODULE},
    {"open",         TOK_OPEN},
    {"opens",        TOK_OPENS},
    {"provides",     TOK_PROVIDES},
    {"requires",     TOK_REQUIRES},
    {"to",           TOK_TO},
    {"transitive",   TOK_TRANSITIVE},
    {"uses",         TOK_USES},
    {"with",         TOK_WITH},
    {NULL,           TOK_EOF}
};

/**
 * Look up a keyword by name.
 */
static token_type_t lookup_keyword(const char *name)
{
    for (int i = 0; keywords[i].name != NULL; i++) {
        if (strcmp(keywords[i].name, name) == 0) {
            return keywords[i].type;
        }
    }
    return TOK_IDENTIFIER;
}

/* ========================================================================
 * Lexer implementation
 * ======================================================================== */

/* Forward declarations for internal functions */
static void lexer_advance_char(lexer_t *lexer);
static char lexer_peek(lexer_t *lexer);
static char lexer_peek_ahead(lexer_t *lexer, int offset);
static void lexer_skip_whitespace(lexer_t *lexer);
static void lexer_scan_identifier(lexer_t *lexer);
static void lexer_scan_number(lexer_t *lexer);
static void lexer_scan_string(lexer_t *lexer);
static void lexer_scan_char(lexer_t *lexer);

/**
 * Create a new lexer for a source file.
 */
lexer_t *lexer_new(source_file_t *source)
{
    if (!source || !source->contents) {
        return NULL;
    }
    
    lexer_t *lexer = malloc(sizeof(lexer_t));
    if (!lexer) {
        return NULL;
    }
    
    lexer->source = source;
    lexer->pos = source->contents;
    lexer->end = source->contents + source->length;
    lexer->line = 1;
    lexer->column = 1;
    lexer->error_msg = NULL;
    
    /* Initialize token state */
    lexer->token.type = TOK_EOF;
    lexer->token.text_start = "";
    lexer->token.text_len = 0;
    lexer->token.line = 1;
    lexer->token.column = 1;
    lexer->token.value.int_value = 0;
    lexer->text_buf[0] = '\0';
    
    /* Advance to first token */
    lexer_advance(lexer);
    
    return lexer;
}

/**
 * Free a lexer.
 */
void lexer_free(lexer_t *lexer)
{
    if (!lexer) {
        return;
    }
    free(lexer->error_msg);
    free(lexer);
}

/**
 * Peek at current character without advancing.
 */
static char lexer_peek(lexer_t *lexer)
{
    if (lexer->pos >= lexer->end) {
        return '\0';
    }
    return *lexer->pos;
}

/**
 * Peek at character at offset from current position.
 */
static char lexer_peek_ahead(lexer_t *lexer, int offset)
{
    if (lexer->pos + offset >= lexer->end) {
        return '\0';
    }
    return lexer->pos[offset];
}

/**
 * Advance to next character (internal).
 */
static void lexer_advance_char(lexer_t *lexer)
{
    if (lexer->pos >= lexer->end) {
        return;
    }
    
    if (*lexer->pos == '\n') {
        lexer->line++;
        lexer->column = 1;
    } else {
        lexer->column++;
    }
    lexer->pos++;
}

/**
 * Skip whitespace and comments.
 */
static void lexer_skip_whitespace(lexer_t *lexer)
{
    while (lexer->pos < lexer->end) {
        char c = lexer_peek(lexer);
        
        /* Whitespace */
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
            lexer_advance_char(lexer);
            continue;
        }
        
        /* Single-line comment */
        if (c == '/' && lexer_peek_ahead(lexer, 1) == '/') {
            lexer_advance_char(lexer);
            lexer_advance_char(lexer);
            while (lexer->pos < lexer->end && lexer_peek(lexer) != '\n') {
                lexer_advance_char(lexer);
            }
            continue;
        }
        
        /* Multi-line comment */
        if (c == '/' && lexer_peek_ahead(lexer, 1) == '*') {
            lexer_advance_char(lexer);
            lexer_advance_char(lexer);
            while (lexer->pos < lexer->end) {
                if (lexer_peek(lexer) == '*' && lexer_peek_ahead(lexer, 1) == '/') {
                    lexer_advance_char(lexer);
                    lexer_advance_char(lexer);
                    break;
                }
                lexer_advance_char(lexer);
            }
            continue;
        }
        
        break;
    }
}

/**
 * Check if character is a Java identifier start.
 */
static bool is_java_identifier_start(char c)
{
    return (c >= 'a' && c <= 'z') ||
           (c >= 'A' && c <= 'Z') ||
           c == '_' || c == '$';
}

/**
 * Check if character is a Java identifier part.
 */
static bool is_java_identifier_part(char c)
{
    return is_java_identifier_start(c) ||
           (c >= '0' && c <= '9');
}

/**
 * Scan an identifier or keyword.
 */
static void lexer_scan_identifier(lexer_t *lexer)
{
    int start_line = lexer->line;
    int start_column = lexer->column;
    const char *start = lexer->pos;
    
    while (lexer->pos < lexer->end && is_java_identifier_part(lexer_peek(lexer))) {
        lexer_advance_char(lexer);
    }
    
    size_t len = lexer->pos - start;
    
    /* Copy to buffer for null-terminated keyword lookup */
    if (len < sizeof(lexer->text_buf)) {
        memcpy(lexer->text_buf, start, len);
        lexer->text_buf[len] = '\0';
    }
    
    /* Special handling for "non-sealed" (Java 17+) */
    if (len == 3 && strncmp(start, "non", 3) == 0) {
        /* Check if followed by "-sealed" */
        if (lexer->pos < lexer->end && lexer_peek(lexer) == '-') {
            const char *after_hyphen = lexer->pos + 1;
            if (lexer->end - after_hyphen >= 6 && strncmp(after_hyphen, "sealed", 6) == 0) {
                /* Check that "sealed" is not followed by more identifier chars */
                const char *after_sealed = after_hyphen + 6;
                if (after_sealed >= lexer->end || !is_java_identifier_part(*after_sealed)) {
                    /* Consume "-sealed" */
                    lexer_advance_char(lexer);  /* '-' */
                    for (int i = 0; i < 6; i++) {
                        lexer_advance_char(lexer);  /* 'sealed' */
                    }
                    lexer_set_token_static(lexer, TOK_NON_SEALED, "non-sealed", 
                                          start_line, start_column);
                    return;
                }
            }
        }
    }
    
    token_type_t type = lookup_keyword(lexer->text_buf);
    
    /* For keywords, we can point directly to source; for identifiers too */
    lexer_set_token(lexer, type, start, len, start_line, start_column);
}

/**
 * Scan a number literal.
 */
static void lexer_scan_number(lexer_t *lexer)
{
    int start_line = lexer->line;
    int start_column = lexer->column;
    const char *start = lexer->pos;
    token_type_t type = TOK_INTEGER_LITERAL;
    bool is_hex = false;
    bool is_binary = false;
    bool has_dot = false;
    bool has_exponent = false;
    
    /* Check for hex or binary prefix */
    if (lexer_peek(lexer) == '0') {
        char next = lexer_peek_ahead(lexer, 1);
        if (next == 'x' || next == 'X') {
            is_hex = true;
            lexer_advance_char(lexer);
            lexer_advance_char(lexer);
        } else if (next == 'b' || next == 'B') {
            is_binary = true;
            lexer_advance_char(lexer);
            lexer_advance_char(lexer);
        }
    }
    
    /* Scan digits */
    while (lexer->pos < lexer->end) {
        char c = lexer_peek(lexer);
        
        if (is_hex) {
            if ((c >= '0' && c <= '9') || 
                (c >= 'a' && c <= 'f') || 
                (c >= 'A' && c <= 'F') ||
                c == '_') {
                lexer_advance_char(lexer);
                continue;
            }
        } else if (is_binary) {
            if (c == '0' || c == '1' || c == '_') {
                lexer_advance_char(lexer);
                continue;
            }
        } else {
            if ((c >= '0' && c <= '9') || c == '_') {
                lexer_advance_char(lexer);
                continue;
            }
            
            /* Decimal point */
            if (c == '.' && !has_dot && !has_exponent) {
                char next = lexer_peek_ahead(lexer, 1);
                if (next >= '0' && next <= '9') {
                    has_dot = true;
                    type = TOK_DOUBLE_LITERAL;
                    lexer_advance_char(lexer);
                    continue;
                }
            }
            
            /* Exponent */
            if ((c == 'e' || c == 'E') && !has_exponent) {
                has_exponent = true;
                type = TOK_DOUBLE_LITERAL;
                lexer_advance_char(lexer);
                if (lexer_peek(lexer) == '+' || lexer_peek(lexer) == '-') {
                    lexer_advance_char(lexer);
                }
                continue;
            }
        }
        
        break;
    }
    
    /* Check for type suffix */
    char c = lexer_peek(lexer);
    if (c == 'l' || c == 'L') {
        type = TOK_LONG_LITERAL;
        lexer_advance_char(lexer);
    } else if (c == 'f' || c == 'F') {
        type = TOK_FLOAT_LITERAL;
        lexer_advance_char(lexer);
    } else if (c == 'd' || c == 'D') {
        type = TOK_DOUBLE_LITERAL;
        lexer_advance_char(lexer);
    }
    
    size_t len = lexer->pos - start;
    
    /* Set token pointing to source */
    lexer_set_token(lexer, type, start, len, start_line, start_column);
    
    /* Parse the numeric value - use text_buf for cleaning */
    if (type == TOK_INTEGER_LITERAL || type == TOK_LONG_LITERAL) {
        /* Remove underscores for parsing */
        size_t j = 0;
        for (size_t i = 0; i < len && j < sizeof(lexer->text_buf) - 1; i++) {
            if (start[i] != '_' && start[i] != 'l' && start[i] != 'L') {
                lexer->text_buf[j++] = start[i];
            }
        }
        lexer->text_buf[j] = '\0';
        
        if (is_hex) {
            lexer->token.value.int_value = strtoll(lexer->text_buf, NULL, 16);
        } else if (is_binary) {
            lexer->token.value.int_value = strtoll(lexer->text_buf + 2, NULL, 2);  /* Skip 0b */
        } else {
            lexer->token.value.int_value = strtoll(lexer->text_buf, NULL, 10);
        }
    } else if (type == TOK_FLOAT_LITERAL || type == TOK_DOUBLE_LITERAL) {
        /* Remove underscores and suffix for parsing */
        size_t j = 0;
        for (size_t i = 0; i < len && j < sizeof(lexer->text_buf) - 1; i++) {
            if (start[i] != '_' && start[i] != 'f' && start[i] != 'F' &&
                start[i] != 'd' && start[i] != 'D') {
                lexer->text_buf[j++] = start[i];
            }
        }
        lexer->text_buf[j] = '\0';
        lexer->token.value.float_value = strtod(lexer->text_buf, NULL);
    }
}

/**
 * Calculate the minimum indentation for text block processing.
 * Returns the number of leading spaces to strip from each line.
 */
static int textblock_min_indent(const char *raw, size_t len, int closing_indent)
{
    int min_indent = closing_indent;
    int current_indent = 0;
    bool at_line_start = true;
    bool line_has_content = false;
    
    for (size_t i = 0; i < len; i++) {
        char c = raw[i];
        if (c == '\n') {
            /* Only count this line if it has non-whitespace content */
            if (line_has_content && current_indent < min_indent) {
                min_indent = current_indent;
            }
            at_line_start = true;
            line_has_content = false;
            current_indent = 0;
        } else if (at_line_start) {
            if (c == ' ') {
                current_indent++;
            } else if (c == '\t') {
                current_indent += 4;  /* Tab = 4 spaces for indent calculation */
            } else {
                at_line_start = false;
                line_has_content = true;
                if (current_indent < min_indent) {
                    min_indent = current_indent;
                }
            }
        } else {
            line_has_content = true;
        }
    }
    
    /* Handle last line if it has content */
    if (line_has_content && current_indent < min_indent) {
        min_indent = current_indent;
    }
    
    return min_indent;
}

/**
 * Process a text block: strip incidental whitespace and handle escape sequences.
 */
static size_t textblock_process(const char *raw, size_t raw_len, 
                                char *out, size_t out_size, int strip_indent)
{
    size_t out_pos = 0;
    int current_indent = 0;
    bool at_line_start = true;
    bool skip_chars = true;  /* Skip leading whitespace up to strip_indent */
    
    for (size_t i = 0; i < raw_len && out_pos < out_size - 1; i++) {
        char c = raw[i];
        
        if (c == '\n') {
            out[out_pos++] = '\n';
            at_line_start = true;
            skip_chars = true;
            current_indent = 0;
            continue;
        }
        
        if (at_line_start && skip_chars) {
            if (c == ' ') {
                current_indent++;
                if (current_indent >= strip_indent) {
                    skip_chars = false;
                }
                continue;
            } else if (c == '\t') {
                current_indent += 4;
                if (current_indent >= strip_indent) {
                    skip_chars = false;
                }
                continue;
            } else {
                at_line_start = false;
                skip_chars = false;
            }
        }
        
        /* Handle escape sequences */
        if (c == '\\' && i + 1 < raw_len) {
            char next = raw[i + 1];
            switch (next) {
                case 'n':  out[out_pos++] = '\n'; i++; break;
                case 't':  out[out_pos++] = '\t'; i++; break;
                case 'r':  out[out_pos++] = '\r'; i++; break;
                case 'b':  out[out_pos++] = '\b'; i++; break;
                case 'f':  out[out_pos++] = '\f'; i++; break;
                case '\\': out[out_pos++] = '\\'; i++; break;
                case '"':  out[out_pos++] = '"';  i++; break;
                case '\'': out[out_pos++] = '\''; i++; break;
                case 's':  out[out_pos++] = ' ';  i++; break;  /* \s = space */
                case '\n':
                    /* Line continuation - skip the newline */
                    i++;
                    /* Skip any leading whitespace on the next line */
                    while (i + 1 < raw_len && (raw[i + 1] == ' ' || raw[i + 1] == '\t')) {
                        i++;
                    }
                    break;
                case '\r':
                    /* Line continuation with \r\n */
                    i++;
                    if (i + 1 < raw_len && raw[i + 1] == '\n') {
                        i++;
                    }
                    /* Skip any leading whitespace on the next line */
                    while (i + 1 < raw_len && (raw[i + 1] == ' ' || raw[i + 1] == '\t')) {
                        i++;
                    }
                    break;
                default:
                    out[out_pos++] = c;
                    break;
            }
        } else {
            out[out_pos++] = c;
        }
    }
    
    out[out_pos] = '\0';
    return out_pos;
}

/**
 * Scan a string literal.
 */
static void lexer_scan_string(lexer_t *lexer)
{
    int start_line = lexer->line;
    int start_column = lexer->column;
    size_t buf_pos = 0;
    
    /* Check for text block (""") */
    if (lexer_peek_ahead(lexer, 1) == '"' && lexer_peek_ahead(lexer, 2) == '"') {
        lexer_advance_char(lexer);  /* Skip first " */
        lexer_advance_char(lexer);  /* Skip second " */
        lexer_advance_char(lexer);  /* Skip third " */
        
        /* Text block must have line terminator after opening """ */
        /* Skip optional whitespace and mandatory line terminator */
        while (lexer->pos < lexer->end && 
               (lexer_peek(lexer) == ' ' || lexer_peek(lexer) == '\t')) {
            lexer_advance_char(lexer);
        }
        
        if (lexer->pos >= lexer->end || 
            (lexer_peek(lexer) != '\n' && lexer_peek(lexer) != '\r')) {
            lexer_set_token_static(lexer, TOK_ERROR, 
                "Text block opening must be followed by line terminator",
                start_line, start_column);
            return;
        }
        
        /* Skip the line terminator */
        if (lexer_peek(lexer) == '\r') {
            lexer_advance_char(lexer);
        }
        if (lexer_peek(lexer) == '\n') {
            lexer_advance_char(lexer);
        }
        
        /* Collect raw content until closing """ */
        /* We use a separate buffer for raw content, then process it */
        char raw_buf[4096];
        size_t raw_pos = 0;
        int closing_indent = 0;
        int current_line_indent = 0;
        bool counting_indent = true;
        
        while (lexer->pos < lexer->end) {
            if (lexer_peek(lexer) == '"' && 
                lexer_peek_ahead(lexer, 1) == '"' && 
                lexer_peek_ahead(lexer, 2) == '"') {
                /* Found closing delimiter - record its indentation */
                closing_indent = current_line_indent;
                lexer_advance_char(lexer);
                lexer_advance_char(lexer);
                lexer_advance_char(lexer);
                break;
            }
            
            char c = lexer_peek(lexer);
            if (raw_pos < sizeof(raw_buf) - 1) {
                raw_buf[raw_pos++] = c;
            }
            
            /* Track indentation for the line containing closing """ */
            if (c == '\n') {
                counting_indent = true;
                current_line_indent = 0;
            } else if (counting_indent) {
                if (c == ' ') {
                    current_line_indent++;
                } else if (c == '\t') {
                    current_line_indent += 4;
                } else {
                    counting_indent = false;
                }
            }
            
            lexer_advance_char(lexer);
        }
        raw_buf[raw_pos] = '\0';
        
        /* Calculate minimum indentation to strip */
        int min_indent = textblock_min_indent(raw_buf, raw_pos, closing_indent);
        
        /* Process the text block: strip indentation and handle escapes */
        buf_pos = textblock_process(raw_buf, raw_pos, lexer->text_buf, 
                                    sizeof(lexer->text_buf), min_indent);
        
        /* Remove trailing newline if present (Java spec: final line terminator removed
           only if the closing """ is on its own line) */
        if (buf_pos > 0 && lexer->text_buf[buf_pos - 1] == '\n') {
            /* Check if closing """ was on its own line (closing_indent tracked above) */
            buf_pos--;
            lexer->text_buf[buf_pos] = '\0';
        }
        
        lexer_set_token_buf(lexer, TOK_TEXT_BLOCK, lexer->text_buf, buf_pos,
                           start_line, start_column);
        return;
    }
    
    /* Regular string */
    lexer_advance_char(lexer);  /* Skip opening quote */
    
    while (lexer->pos < lexer->end && lexer_peek(lexer) != '"') {
        char c = lexer_peek(lexer);
        
        if (c == '\n') {
            /* Unterminated string */
            lexer_set_token_static(lexer, TOK_ERROR, "Unterminated string literal",
                                  start_line, start_column);
            return;
        }
        
        if (c == '\\') {
            lexer_advance_char(lexer);
            if (lexer->pos >= lexer->end) {
                break;
            }
            
            c = lexer_peek(lexer);
            char esc;
            switch (c) {
                case 'n':  esc = '\n'; break;
                case 't':  esc = '\t'; break;
                case 'r':  esc = '\r'; break;
                case 'b':  esc = '\b'; break;
                case 'f':  esc = '\f'; break;
                case '\\': esc = '\\'; break;
                case '"':  esc = '"'; break;
                case '\'': esc = '\''; break;
                default:   esc = c; break;
            }
            if (buf_pos < sizeof(lexer->text_buf) - 1) {
                lexer->text_buf[buf_pos++] = esc;
            }
        } else {
            if (buf_pos < sizeof(lexer->text_buf) - 1) {
                lexer->text_buf[buf_pos++] = c;
            }
        }
        lexer_advance_char(lexer);
    }
    
    if (lexer_peek(lexer) == '"') {
        lexer_advance_char(lexer);  /* Skip closing quote */
    }
    
    lexer->text_buf[buf_pos] = '\0';
    lexer_set_token_buf(lexer, TOK_STRING_LITERAL, lexer->text_buf, buf_pos,
                       start_line, start_column);
}

/**
 * Scan a character literal.
 */
static void lexer_scan_char(lexer_t *lexer)
{
    int start_line = lexer->line;
    int start_column = lexer->column;
    char ch;
    
    lexer_advance_char(lexer);  /* Skip opening quote */
    
    if (lexer_peek(lexer) == '\\') {
        lexer_advance_char(lexer);
        char c = lexer_peek(lexer);
        switch (c) {
            case 'n':  ch = '\n'; break;
            case 't':  ch = '\t'; break;
            case 'r':  ch = '\r'; break;
            case 'b':  ch = '\b'; break;
            case 'f':  ch = '\f'; break;
            case '\\': ch = '\\'; break;
            case '"':  ch = '"'; break;
            case '\'': ch = '\''; break;
            default:   ch = c; break;
        }
        lexer_advance_char(lexer);
    } else {
        ch = lexer_peek(lexer);
        lexer_advance_char(lexer);
    }
    
    if (lexer_peek(lexer) == '\'') {
        lexer_advance_char(lexer);  /* Skip closing quote */
    }
    
    /* Store single character in buffer */
    lexer->text_buf[0] = ch;
    lexer->text_buf[1] = '\0';
    lexer_set_token_buf(lexer, TOK_CHAR_LITERAL, lexer->text_buf, 1,
                       start_line, start_column);
}

/**
 * Advance to the next token (feedforward tokenization).
 */
void lexer_advance(lexer_t *lexer)
{
    lexer_skip_whitespace(lexer);
    
    if (lexer->pos >= lexer->end) {
        lexer_set_token_static(lexer, TOK_EOF, "", lexer->line, lexer->column);
        return;
    }
    
    int start_line = lexer->line;
    int start_column = lexer->column;
    char c = lexer_peek(lexer);
    
    /* Identifiers and keywords */
    if (is_java_identifier_start(c)) {
        lexer_scan_identifier(lexer);
        return;
    }
    
    /* Number literals */
    if (c >= '0' && c <= '9') {
        lexer_scan_number(lexer);
        return;
    }
    
    /* String literals */
    if (c == '"') {
        lexer_scan_string(lexer);
        return;
    }
    
    /* Character literals */
    if (c == '\'') {
        lexer_scan_char(lexer);
        return;
    }
    
    /* Operators and punctuation */
    lexer_advance_char(lexer);
    
    switch (c) {
        case '(':
            lexer_set_token_static(lexer, TOK_LPAREN, "(", start_line, start_column);
            return;
        case ')':
            lexer_set_token_static(lexer, TOK_RPAREN, ")", start_line, start_column);
            return;
        case '{':
            lexer_set_token_static(lexer, TOK_LBRACE, "{", start_line, start_column);
            return;
        case '}':
            lexer_set_token_static(lexer, TOK_RBRACE, "}", start_line, start_column);
            return;
        case '[':
            lexer_set_token_static(lexer, TOK_LBRACKET, "[", start_line, start_column);
            return;
        case ']':
            lexer_set_token_static(lexer, TOK_RBRACKET, "]", start_line, start_column);
            return;
        case ';':
            lexer_set_token_static(lexer, TOK_SEMICOLON, ";", start_line, start_column);
            return;
        case ',':
            lexer_set_token_static(lexer, TOK_COMMA, ",", start_line, start_column);
            return;
        case '@':
            lexer_set_token_static(lexer, TOK_AT, "@", start_line, start_column);
            return;
        case '~':
            lexer_set_token_static(lexer, TOK_TILDE, "~", start_line, start_column);
            return;
        case '?':
            lexer_set_token_static(lexer, TOK_QUESTION, "?", start_line, start_column);
            return;
            
        case '.':
            if (lexer_peek(lexer) == '.' && lexer_peek_ahead(lexer, 1) == '.') {
                lexer_advance_char(lexer);
                lexer_advance_char(lexer);
                lexer_set_token_static(lexer, TOK_ELLIPSIS, "...", start_line, start_column);
                return;
            }
            lexer_set_token_static(lexer, TOK_DOT, ".", start_line, start_column);
            return;
            
        case ':':
            if (lexer_peek(lexer) == ':') {
                lexer_advance_char(lexer);
                lexer_set_token_static(lexer, TOK_DOUBLE_COLON, "::", start_line, start_column);
                return;
            }
            lexer_set_token_static(lexer, TOK_COLON, ":", start_line, start_column);
            return;
            
        case '=':
            if (lexer_peek(lexer) == '=') {
                lexer_advance_char(lexer);
                lexer_set_token_static(lexer, TOK_EQ, "==", start_line, start_column);
                return;
            }
            lexer_set_token_static(lexer, TOK_ASSIGN, "=", start_line, start_column);
            return;
            
        case '!':
            if (lexer_peek(lexer) == '=') {
                lexer_advance_char(lexer);
                lexer_set_token_static(lexer, TOK_NE, "!=", start_line, start_column);
                return;
            }
            lexer_set_token_static(lexer, TOK_NOT, "!", start_line, start_column);
            return;
            
        case '<':
            if (lexer_peek(lexer) == '=') {
                lexer_advance_char(lexer);
                lexer_set_token_static(lexer, TOK_LE, "<=", start_line, start_column);
                return;
            }
            if (lexer_peek(lexer) == '<') {
                lexer_advance_char(lexer);
                if (lexer_peek(lexer) == '=') {
                    lexer_advance_char(lexer);
                    lexer_set_token_static(lexer, TOK_LSHIFT_ASSIGN, "<<=", start_line, start_column);
                    return;
                }
                lexer_set_token_static(lexer, TOK_LSHIFT, "<<", start_line, start_column);
                return;
            }
            lexer_set_token_static(lexer, TOK_LT, "<", start_line, start_column);
            return;
            
        case '>':
            if (lexer_peek(lexer) == '=') {
                lexer_advance_char(lexer);
                lexer_set_token_static(lexer, TOK_GE, ">=", start_line, start_column);
                return;
            }
            if (lexer_peek(lexer) == '>') {
                lexer_advance_char(lexer);
                if (lexer_peek(lexer) == '>') {
                    lexer_advance_char(lexer);
                    if (lexer_peek(lexer) == '=') {
                        lexer_advance_char(lexer);
                        lexer_set_token_static(lexer, TOK_URSHIFT_ASSIGN, ">>>=", start_line, start_column);
                        return;
                    }
                    lexer_set_token_static(lexer, TOK_URSHIFT, ">>>", start_line, start_column);
                    return;
                }
                if (lexer_peek(lexer) == '=') {
                    lexer_advance_char(lexer);
                    lexer_set_token_static(lexer, TOK_RSHIFT_ASSIGN, ">>=", start_line, start_column);
                    return;
                }
                lexer_set_token_static(lexer, TOK_RSHIFT, ">>", start_line, start_column);
                return;
            }
            lexer_set_token_static(lexer, TOK_GT, ">", start_line, start_column);
            return;
            
        case '+':
            if (lexer_peek(lexer) == '+') {
                lexer_advance_char(lexer);
                lexer_set_token_static(lexer, TOK_INC, "++", start_line, start_column);
                return;
            }
            if (lexer_peek(lexer) == '=') {
                lexer_advance_char(lexer);
                lexer_set_token_static(lexer, TOK_PLUS_ASSIGN, "+=", start_line, start_column);
                return;
            }
            lexer_set_token_static(lexer, TOK_PLUS, "+", start_line, start_column);
            return;
            
        case '-':
            if (lexer_peek(lexer) == '-') {
                lexer_advance_char(lexer);
                lexer_set_token_static(lexer, TOK_DEC, "--", start_line, start_column);
                return;
            }
            if (lexer_peek(lexer) == '=') {
                lexer_advance_char(lexer);
                lexer_set_token_static(lexer, TOK_MINUS_ASSIGN, "-=", start_line, start_column);
                return;
            }
            if (lexer_peek(lexer) == '>') {
                lexer_advance_char(lexer);
                lexer_set_token_static(lexer, TOK_ARROW, "->", start_line, start_column);
                return;
            }
            lexer_set_token_static(lexer, TOK_MINUS, "-", start_line, start_column);
            return;
            
        case '*':
            if (lexer_peek(lexer) == '=') {
                lexer_advance_char(lexer);
                lexer_set_token_static(lexer, TOK_STAR_ASSIGN, "*=", start_line, start_column);
                return;
            }
            lexer_set_token_static(lexer, TOK_STAR, "*", start_line, start_column);
            return;
            
        case '/':
            if (lexer_peek(lexer) == '=') {
                lexer_advance_char(lexer);
                lexer_set_token_static(lexer, TOK_SLASH_ASSIGN, "/=", start_line, start_column);
                return;
            }
            lexer_set_token_static(lexer, TOK_SLASH, "/", start_line, start_column);
            return;
            
        case '%':
            if (lexer_peek(lexer) == '=') {
                lexer_advance_char(lexer);
                lexer_set_token_static(lexer, TOK_MOD_ASSIGN, "%=", start_line, start_column);
                return;
            }
            lexer_set_token_static(lexer, TOK_MOD, "%", start_line, start_column);
            return;
            
        case '&':
            if (lexer_peek(lexer) == '&') {
                lexer_advance_char(lexer);
                lexer_set_token_static(lexer, TOK_AND, "&&", start_line, start_column);
                return;
            }
            if (lexer_peek(lexer) == '=') {
                lexer_advance_char(lexer);
                lexer_set_token_static(lexer, TOK_AND_ASSIGN, "&=", start_line, start_column);
                return;
            }
            lexer_set_token_static(lexer, TOK_BITAND, "&", start_line, start_column);
            return;
            
        case '|':
            if (lexer_peek(lexer) == '|') {
                lexer_advance_char(lexer);
                lexer_set_token_static(lexer, TOK_OR, "||", start_line, start_column);
                return;
            }
            if (lexer_peek(lexer) == '=') {
                lexer_advance_char(lexer);
                lexer_set_token_static(lexer, TOK_OR_ASSIGN, "|=", start_line, start_column);
                return;
            }
            lexer_set_token_static(lexer, TOK_BITOR, "|", start_line, start_column);
            return;
            
        case '^':
            if (lexer_peek(lexer) == '=') {
                lexer_advance_char(lexer);
                lexer_set_token_static(lexer, TOK_XOR_ASSIGN, "^=", start_line, start_column);
                return;
            }
            lexer_set_token_static(lexer, TOK_CARET, "^", start_line, start_column);
            return;
            
        default:
            snprintf(lexer->text_buf, sizeof(lexer->text_buf),
                    "Unexpected character: '%c'", c);
            lexer_set_token_buf(lexer, TOK_ERROR, lexer->text_buf,
                               strlen(lexer->text_buf), start_line, start_column);
            return;
    }
}

/* ========================================================================
 * Lexer accessor functions
 * ======================================================================== */

token_type_t lexer_type(lexer_t *lexer)
{
    return lexer->token.type;
}

const char *lexer_text(lexer_t *lexer)
{
    /* Ensure null-termination - text_buf is always null-terminated,
     * but source pointers need special handling */
    if (lexer->token.text_start == lexer->text_buf) {
        return lexer->text_buf;  /* Already null-terminated */
    }
    
    /* Copy to buffer and null-terminate for source pointers */
    size_t len = lexer->token.text_len;
    if (len >= sizeof(lexer->text_buf)) {
        len = sizeof(lexer->text_buf) - 1;
    }
    memcpy(lexer->text_buf, lexer->token.text_start, len);
    lexer->text_buf[len] = '\0';
    return lexer->text_buf;
}

size_t lexer_text_len(lexer_t *lexer)
{
    return lexer->token.text_len;
}

int lexer_line(lexer_t *lexer)
{
    return lexer->token.line;
}

int lexer_column(lexer_t *lexer)
{
    return lexer->token.column;
}

token_value_t lexer_value(lexer_t *lexer)
{
    return lexer->token.value;
}

lexer_pos_t lexer_save_pos(lexer_t *lexer)
{
    lexer_pos_t p;
    p.pos = lexer->pos;
    p.line = lexer->line;
    p.column = lexer->column;
    p.token_type = lexer->token.type;
    p.token_text_start = lexer->token.text_start;
    p.token_text_len = lexer->token.text_len;
    p.token_line = lexer->token.line;
    p.token_column = lexer->token.column;
    p.token_value = lexer->token.value;
    return p;
}

void lexer_restore_pos(lexer_t *lexer, lexer_pos_t p)
{
    lexer->pos = p.pos;
    lexer->line = p.line;
    lexer->column = p.column;
    lexer->token.type = p.token_type;
    lexer->token.text_start = p.token_text_start;
    lexer->token.text_len = p.token_text_len;
    lexer->token.line = p.token_line;
    lexer->token.column = p.token_column;
    lexer->token.value = p.token_value;
}

