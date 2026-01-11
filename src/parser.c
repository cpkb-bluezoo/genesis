/*
 * parser.c
 * Iterative parser for Java source files using explicit stacks
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
 * AST Node Implementation
 * ======================================================================== */

/**
 * Create a new AST node.
 */
ast_node_t *ast_new(ast_node_type_t type, int line, int column)
{
    ast_node_t *node = calloc(1, sizeof(ast_node_t));
    if (!node) {
        return NULL;
    }
    
    node->type = type;
    node->line = line;
    node->column = column;
    
    return node;
}

/**
 * Create a new leaf AST node (identifier, etc.).
 */
ast_node_t *ast_new_leaf(ast_node_type_t type, const char *name, int line, int column)
{
    ast_node_t *node = ast_new(type, line, column);
    if (!node) {
        return NULL;
    }
    
    if (name) {
        node->data.leaf.name = strdup(name);
    }
    
    return node;
}

/**
 * Create a literal AST node from current lexer token.
 */
ast_node_t *ast_new_literal_from_lexer(lexer_t *lexer)
{
    ast_node_t *node = ast_new(AST_LITERAL, lexer_line(lexer), lexer_column(lexer));
    if (!node) {
        return NULL;
    }
    
    token_type_t type = lexer_type(lexer);
    const char *text = lexer_text(lexer);
    token_value_t value = lexer_value(lexer);
    
    node->data.leaf.token_type = type;
    
    switch (type) {
        case TOK_INTEGER_LITERAL:
        case TOK_LONG_LITERAL:
            node->data.leaf.value.int_val = value.int_value;
            node->data.leaf.name = text ? strdup(text) : NULL;
            break;
        case TOK_FLOAT_LITERAL:
        case TOK_DOUBLE_LITERAL:
            node->data.leaf.value.float_val = value.float_value;
            node->data.leaf.name = text ? strdup(text) : NULL;
            break;
        case TOK_STRING_LITERAL:
        case TOK_TEXT_BLOCK:
        case TOK_CHAR_LITERAL:
            node->data.leaf.value.str_val = text ? strdup(text) : NULL;
            node->data.leaf.name = text ? strdup(text) : NULL;
            break;
        case TOK_TRUE:
            node->data.leaf.value.int_val = 1;
            node->data.leaf.name = strdup("true");
            break;
        case TOK_FALSE:
            node->data.leaf.value.int_val = 0;
            node->data.leaf.name = strdup("false");
            break;
        case TOK_NULL:
            node->data.leaf.name = strdup("null");
            break;
        default:
            node->data.leaf.name = text ? strdup(text) : NULL;
            break;
    }
    
    return node;
}

/**
 * Free an AST node and all children.
 */
void ast_free(ast_node_t *node)
{
    if (!node) {
        return;
    }
    
    /* Use an explicit stack to avoid recursion */
    slist_t *stack = slist_new(node);
    slist_t *stack_tail = stack;
    
    while (stack) {
        ast_node_t *current = (ast_node_t *)stack->data;
        slist_t *next = stack->next;
        free(stack);
        stack = next;
        
        if (!current) {
            continue;
        }
        
        /* Check if this node has children to process */
        bool has_children = false;
        switch (current->type) {
            case AST_LITERAL:
            case AST_IDENTIFIER:
            case AST_THIS_EXPR:
            case AST_SUPER_EXPR:
            case AST_PRIMITIVE_TYPE:
            case AST_VAR_TYPE:
                /* Leaf nodes - free name */
                free(current->data.leaf.name);
                if (current->type == AST_LITERAL &&
                    (current->data.leaf.token_type == TOK_STRING_LITERAL ||
                     current->data.leaf.token_type == TOK_TEXT_BLOCK ||
                     current->data.leaf.token_type == TOK_CHAR_LITERAL)) {
                    free(current->data.leaf.value.str_val);
                }
                break;
            default:
                has_children = true;
                break;
        }
        
        if (has_children) {
            /* Add children to stack */
            slist_t *child = current->data.node.children;
            while (child) {
                if (stack) {
                    stack_tail = slist_append(stack_tail, child->data);
                } else {
                    stack = slist_new(child->data);
                    stack_tail = stack;
                }
                child = child->next;
            }
            
            /* Add extra node if present */
            if (current->data.node.extra) {
                if (stack) {
                    stack_tail = slist_append(stack_tail, current->data.node.extra);
                } else {
                    stack = slist_new(current->data.node.extra);
                    stack_tail = stack;
                }
            }
            
            /* Free children list (not the children themselves) */
            slist_free(current->data.node.children);
            free(current->data.node.name);
        }
        
        free(current);
    }
}

/**
 * Add a child to an AST node.
 */
void ast_add_child(ast_node_t *parent, ast_node_t *child)
{
    if (!parent || !child) {
        return;
    }
    
    if (!parent->data.node.children) {
        parent->data.node.children = slist_new(child);
    } else {
        slist_append(parent->data.node.children, child);
    }
}

/**
 * Get string name for AST node type.
 */
const char *ast_type_name(ast_node_type_t type)
{
    switch (type) {
        case AST_COMPILATION_UNIT:   return "CompilationUnit";
        case AST_PACKAGE_DECL:       return "PackageDecl";
        case AST_IMPORT_DECL:        return "ImportDecl";
        case AST_CLASS_DECL:         return "ClassDecl";
        case AST_INTERFACE_DECL:     return "InterfaceDecl";
        case AST_ENUM_DECL:          return "EnumDecl";
        case AST_RECORD_DECL:        return "RecordDecl";
        case AST_ANNOTATION_DECL:    return "AnnotationDecl";
        case AST_FIELD_DECL:         return "FieldDecl";
        case AST_METHOD_DECL:        return "MethodDecl";
        case AST_CONSTRUCTOR_DECL:   return "ConstructorDecl";
        case AST_INITIALIZER_BLOCK:  return "InitializerBlock";
        case AST_ENUM_CONSTANT:      return "EnumConstant";
        case AST_PRIMITIVE_TYPE:     return "PrimitiveType";
        case AST_CLASS_TYPE:         return "ClassType";
        case AST_ARRAY_TYPE:         return "ArrayType";
        case AST_TYPE_PARAMETER:     return "TypeParameter";
        case AST_WILDCARD_TYPE:      return "WildcardType";
        case AST_VAR_TYPE:           return "VarType";
        case AST_PARAMETER:          return "Parameter";
        case AST_VAR_DECL:           return "VarDecl";
        case AST_VAR_DECLARATOR:     return "VarDeclarator";
        case AST_BLOCK:              return "Block";
        case AST_EMPTY_STMT:         return "EmptyStmt";
        case AST_EXPR_STMT:          return "ExprStmt";
        case AST_IF_STMT:            return "IfStmt";
        case AST_FOR_STMT:           return "ForStmt";
        case AST_ENHANCED_FOR_STMT:  return "EnhancedForStmt";
        case AST_WHILE_STMT:         return "WhileStmt";
        case AST_DO_STMT:            return "DoStmt";
        case AST_SWITCH_STMT:        return "SwitchStmt";
        case AST_SWITCH_EXPR:        return "SwitchExpr";
        case AST_CASE_LABEL:         return "CaseLabel";
        case AST_SWITCH_RULE:        return "SwitchRule";
        case AST_TYPE_PATTERN:       return "TypePattern";
        case AST_GUARDED_PATTERN:    return "GuardedPattern";
        case AST_UNNAMED_PATTERN:    return "UnnamedPattern";
        case AST_RECORD_PATTERN:     return "RecordPattern";
        case AST_RETURN_STMT:        return "ReturnStmt";
        case AST_THROW_STMT:         return "ThrowStmt";
        case AST_BREAK_STMT:         return "BreakStmt";
        case AST_CONTINUE_STMT:      return "ContinueStmt";
        case AST_TRY_STMT:           return "TryStmt";
        case AST_RESOURCE_SPEC:      return "ResourceSpec";
        case AST_CATCH_CLAUSE:       return "CatchClause";
        case AST_FINALLY_CLAUSE:     return "FinallyClause";
        case AST_SYNCHRONIZED_STMT:  return "SynchronizedStmt";
        case AST_ASSERT_STMT:        return "AssertStmt";
        case AST_LABELED_STMT:       return "LabeledStmt";
        case AST_YIELD_STMT:         return "YieldStmt";
        case AST_LITERAL:            return "Literal";
        case AST_IDENTIFIER:         return "Identifier";
        case AST_THIS_EXPR:          return "ThisExpr";
        case AST_SUPER_EXPR:         return "SuperExpr";
        case AST_BINARY_EXPR:        return "BinaryExpr";
        case AST_UNARY_EXPR:         return "UnaryExpr";
        case AST_CONDITIONAL_EXPR:   return "ConditionalExpr";
        case AST_ASSIGNMENT_EXPR:    return "AssignmentExpr";
        case AST_CAST_EXPR:          return "CastExpr";
        case AST_INSTANCEOF_EXPR:    return "InstanceofExpr";
        case AST_METHOD_CALL:        return "MethodCall";
        case AST_FIELD_ACCESS:       return "FieldAccess";
        case AST_ARRAY_ACCESS:       return "ArrayAccess";
        case AST_NEW_OBJECT:         return "NewObject";
        case AST_NEW_ARRAY:          return "NewArray";
        case AST_ARRAY_INIT:         return "ArrayInit";
        case AST_LAMBDA_EXPR:        return "LambdaExpr";
        case AST_LAMBDA_PARAMS:      return "LambdaParams";
        case AST_METHOD_REF:         return "MethodRef";
        case AST_CLASS_LITERAL:      return "ClassLiteral";
        case AST_PARENTHESIZED:      return "Parenthesized";
        case AST_EXPLICIT_CTOR_CALL: return "ExplicitCtorCall";
        case AST_ANNOTATION:         return "Annotation";
        case AST_ANNOTATION_VALUE:   return "AnnotationValue";
        case AST_MODIFIERS:          return "Modifiers";
        default:                     return "Unknown";
    }
}

/**
 * Print AST node (for debugging) - iterative.
 */
void ast_print(ast_node_t *node, int indent)
{
    if (!node) {
        return;
    }
    
    /* Stack entry for iterative printing */
    typedef struct print_entry {
        ast_node_t *node;
        int indent;
        bool visited;
    } print_entry_t;
    
    /* Use a simple array-based stack */
    int stack_size = 256;
    int stack_top = 0;
    print_entry_t *stack = malloc(sizeof(print_entry_t) * stack_size);
    
    stack[stack_top].node = node;
    stack[stack_top].indent = indent;
    stack[stack_top].visited = false;
    stack_top++;
    
    while (stack_top > 0) {
        print_entry_t *entry = &stack[stack_top - 1];
        ast_node_t *current = entry->node;
        int cur_indent = entry->indent;
        
        if (!current) {
            stack_top--;
            continue;
        }
        
        /* Print indentation and node info */
        for (int i = 0; i < cur_indent; i++) printf("  ");
        printf("%s", ast_type_name(current->type));
        
        /* Print additional info based on node type */
        switch (current->type) {
            case AST_LITERAL:
            case AST_IDENTIFIER:
            case AST_PRIMITIVE_TYPE:
            case AST_VAR_TYPE:
                if (current->data.leaf.name) {
                    printf(" '%s'", current->data.leaf.name);
                }
                break;
            case AST_BINARY_EXPR:
            case AST_UNARY_EXPR:
            case AST_ASSIGNMENT_EXPR:
            case AST_CLASS_TYPE:
            case AST_METHOD_CALL:
            case AST_TYPE_PATTERN:
                if (current->data.node.name) {
                    printf(" '%s'", current->data.node.name);
                }
                break;
            case AST_CLASS_DECL:
            case AST_INTERFACE_DECL:
            case AST_ENUM_DECL:
            case AST_METHOD_DECL:
            case AST_CONSTRUCTOR_DECL:
            case AST_FIELD_DECL:
            case AST_PARAMETER:
            case AST_VAR_DECLARATOR:
                if (current->data.node.name) {
                    printf(" '%s'", current->data.node.name);
                }
                if (current->data.node.flags) {
                    printf(" flags=0x%x", current->data.node.flags);
                }
                break;
            default:
                break;
        }
        
        printf(" [%d:%d]\n", current->line, current->column);
        
        stack_top--;
        
        /* Add children in reverse order so they print in correct order */
        slist_t *children = NULL;
        switch (current->type) {
            case AST_LITERAL:
            case AST_IDENTIFIER:
            case AST_THIS_EXPR:
            case AST_SUPER_EXPR:
            case AST_PRIMITIVE_TYPE:
            case AST_VAR_TYPE:
                /* Leaf nodes - no children */
                break;
            default:
                children = current->data.node.children;
                break;
        }
        
        /* Count children and add in reverse */
        if (children) {
            int count = 0;
            slist_t *tmp = children;
            while (tmp) {
                count++;
                tmp = tmp->next;
            }
            
            /* Grow stack if needed */
            while (stack_top + count >= stack_size) {
                stack_size *= 2;
                stack = realloc(stack, sizeof(print_entry_t) * stack_size);
            }
            
            /* Add children in reverse order */
            ast_node_t **child_array = malloc(sizeof(ast_node_t *) * count);
            tmp = children;
            for (int i = 0; i < count; i++) {
                child_array[i] = (ast_node_t *)tmp->data;
                tmp = tmp->next;
            }
            for (int i = count - 1; i >= 0; i--) {
                stack[stack_top].node = child_array[i];
                stack[stack_top].indent = cur_indent + 1;
                stack[stack_top].visited = false;
                stack_top++;
            }
            free(child_array);
        }
    }
    
    free(stack);
}

/* ========================================================================
 * Parser Implementation
 * ======================================================================== */

/**
 * Create a new parser.
 */
parser_t *parser_new(lexer_t *lexer, source_file_t *source)
{
    parser_t *parser = calloc(1, sizeof(parser_t));
    if (!parser) {
        return NULL;
    }
    
    parser->lexer = lexer;
    parser->source = source;
    
    return parser;
}

/**
 * Free a parser.
 */
void parser_free(parser_t *parser)
{
    if (!parser) {
        return;
    }
    free(parser->error_msg);
    free(parser);
}

/* ========================================================================
 * Parser Helpers
 * ======================================================================== */

/**
 * Get current token type.
 */
static inline token_type_t parser_current_type(parser_t *parser)
{
    if (parser->pending_token != 0) {
        return parser->pending_token;
    }
    return lexer_type(parser->lexer);
}

/**
 * Get current token text.
 */
static inline const char *parser_current_text(parser_t *parser)
{
    return lexer_text(parser->lexer);
}

/**
 * Get current token line.
 */
static inline int parser_current_line(parser_t *parser)
{
    return lexer_line(parser->lexer);
}

/**
 * Get current token column.
 */
static inline int parser_current_column(parser_t *parser)
{
    return lexer_column(parser->lexer);
}

/**
 * Get current token value.
 */
static inline token_value_t parser_current_value(parser_t *parser)
{
    return lexer_value(parser->lexer);
}

/**
 * Peek at the next token type without consuming the current one.
 */
static inline token_type_t parser_peek_type(parser_t *parser)
{
    lexer_pos_t saved = lexer_save_pos(parser->lexer);
    lexer_advance(parser->lexer);
    token_type_t next_type = lexer_type(parser->lexer);
    lexer_restore_pos(parser->lexer, saved);
    return next_type;
}

/**
 * Advance to next token.
 * If there's a pending token (from split >> or >>>), consume that first.
 */
static void parser_advance(parser_t *parser)
{
    if (parser->pending_token != 0) {
        /* Consume the pending token and advance past the split token (>> or >>>) */
        parser->pending_token = 0;
        lexer_advance(parser->lexer);
    } else {
        lexer_advance(parser->lexer);
    }
}

/**
 * Check if current token matches type.
 */
static bool parser_check(parser_t *parser, token_type_t type)
{
    return parser_current_type(parser) == type;
}

/**
 * Check and consume token if it matches.
 */
static bool parser_match(parser_t *parser, token_type_t type)
{
    if (parser_check(parser, type)) {
        parser_advance(parser);
        return true;
    }
    return false;
}

/**
 * Expect a specific token type, error if not found.
 */
static bool parser_expect(parser_t *parser, token_type_t type)
{
    if (parser_check(parser, type)) {
        parser_advance(parser);
        return true;
    }
    
    /* Set error */
    free(parser->error_msg);
    parser->error_msg = malloc(256);
    snprintf(parser->error_msg, 256, "Expected '%s', found '%s'",
             token_type_name(type),
             token_type_name(parser_current_type(parser)));
    parser->error_line = parser_current_line(parser);
    parser->error_column = parser_current_column(parser);
    return false;
}

/**
 * Expect a '>' in a generic type context.
 * Handles >> and >>> tokens by consuming just the first '>'.
 * This is necessary because List<List<String>> lexes >> as RSHIFT.
 */
static bool parser_expect_gt(parser_t *parser)
{
    token_type_t tok = parser_current_type(parser);
    
    if (tok == TOK_GT) {
        parser_advance(parser);
        return true;
    }
    
    /* Handle >> by splitting it into > and > */
    if (tok == TOK_RSHIFT) {
        /* Set pending token to > for the next parser_current_type call */
        parser->pending_token = TOK_GT;
        return true;
    }
    
    /* Handle >>> by splitting it into > and >> */
    if (tok == TOK_URSHIFT) {
        /* Set pending token to >> for the next parser_current_type call */
        parser->pending_token = TOK_RSHIFT;
        return true;
    }
    
    /* Set error */
    free(parser->error_msg);
    parser->error_msg = malloc(256);
    snprintf(parser->error_msg, 256, "Expected '>', found '%s'",
             token_type_name(tok));
    parser->error_line = parser_current_line(parser);
    parser->error_column = parser_current_column(parser);
    return false;
}

/**
 * Report a parser error.
 */
static void parser_error(parser_t *parser, const char *message)
{
    free(parser->error_msg);
    parser->error_msg = strdup(message);
    parser->error_line = parser_current_line(parser);
    parser->error_column = parser_current_column(parser);
}

/**
 * Check if token is a modifier keyword.
 */
static bool is_modifier(token_type_t type)
{
    switch (type) {
        case TOK_PUBLIC:
        case TOK_PRIVATE:
        case TOK_PROTECTED:
        case TOK_STATIC:
        case TOK_FINAL:
        case TOK_ABSTRACT:
        case TOK_NATIVE:
        case TOK_SYNCHRONIZED:
        case TOK_TRANSIENT:
        case TOK_VOLATILE:
        case TOK_STRICTFP:
        case TOK_DEFAULT:
        case TOK_SEALED:
        case TOK_NON_SEALED:
            return true;
        default:
            return false;
    }
}

/**
 * Convert modifier token to flag.
 */
static uint32_t modifier_flag(token_type_t type)
{
    switch (type) {
        case TOK_PUBLIC:       return MOD_PUBLIC;
        case TOK_PRIVATE:      return MOD_PRIVATE;
        case TOK_PROTECTED:    return MOD_PROTECTED;
        case TOK_STATIC:       return MOD_STATIC;
        case TOK_FINAL:        return MOD_FINAL;
        case TOK_ABSTRACT:     return MOD_ABSTRACT;
        case TOK_NATIVE:       return MOD_NATIVE;
        case TOK_SYNCHRONIZED: return MOD_SYNCHRONIZED;
        case TOK_TRANSIENT:    return MOD_TRANSIENT;
        case TOK_VOLATILE:     return MOD_VOLATILE;
        case TOK_STRICTFP:     return MOD_STRICTFP;
        case TOK_DEFAULT:      return MOD_DEFAULT;
        case TOK_SEALED:       return MOD_SEALED;
        case TOK_NON_SEALED:   return MOD_NON_SEALED;
        default:               return 0;
    }
}

/**
 * Check if token starts a type.
 */
static bool is_type_start(token_type_t type)
{
    switch (type) {
        case TOK_BOOLEAN:
        case TOK_BYTE:
        case TOK_CHAR:
        case TOK_SHORT:
        case TOK_INT:
        case TOK_LONG:
        case TOK_FLOAT:
        case TOK_DOUBLE:
        case TOK_VOID:
        case TOK_VAR:        /* var for type inference (Java 10+) */
        case TOK_IDENTIFIER:
            return true;
        default:
            return false;
    }
}

/**
 * Check if token is a primitive type.
 */
static bool is_primitive_type(token_type_t type)
{
    switch (type) {
        case TOK_BOOLEAN:
        case TOK_BYTE:
        case TOK_CHAR:
        case TOK_SHORT:
        case TOK_INT:
        case TOK_LONG:
        case TOK_FLOAT:
        case TOK_DOUBLE:
            return true;
        default:
            return false;
    }
}

/**
 * Check if token is a contextual keyword that can be used as identifier.
 * These are keywords introduced for modules/records/etc. that can still
 * be used as identifiers in non-module contexts.
 */
static bool is_contextual_keyword(token_type_t type)
{
    switch (type) {
        case TOK_MODULE:
        case TOK_REQUIRES:
        case TOK_EXPORTS:
        case TOK_OPENS:
        case TOK_USES:
        case TOK_PROVIDES:
        case TOK_WITH:
        case TOK_TO:
        case TOK_TRANSITIVE:
        case TOK_OPEN:
        case TOK_PERMITS:
        case TOK_SEALED:
        case TOK_NON_SEALED:
        case TOK_RECORD:
        case TOK_VAR:
        case TOK_YIELD:
        case TOK_WHEN:
            return true;
        default:
            return false;
    }
}

/* ========================================================================
 * Expression Parsing - Pratt Parser (Iterative)
 * 
 * Uses operator precedence climbing with explicit stacks.
 * ======================================================================== */

/* Operator precedence levels (higher = binds tighter) */
typedef enum precedence
{
    PREC_NONE = 0,
    PREC_ASSIGNMENT,    /* = += -= etc. */
    PREC_CONDITIONAL,   /* ?: */
    PREC_OR,            /* || */
    PREC_AND,           /* && */
    PREC_BITOR,         /* | */
    PREC_BITXOR,        /* ^ */
    PREC_BITAND,        /* & */
    PREC_EQUALITY,      /* == != */
    PREC_RELATIONAL,    /* < > <= >= instanceof */
    PREC_SHIFT,         /* << >> >>> */
    PREC_ADDITIVE,      /* + - */
    PREC_MULTIPLICATIVE,/* * / % */
    PREC_UNARY,         /* ! ~ ++ -- + - (cast) */
    PREC_POSTFIX,       /* ++ -- [] . () */
    PREC_PRIMARY        /* literals, identifiers */
} precedence_t;

/**
 * Get precedence of binary operator.
 */
static precedence_t get_binary_precedence(token_type_t type)
{
    switch (type) {
        case TOK_ASSIGN:
        case TOK_PLUS_ASSIGN:
        case TOK_MINUS_ASSIGN:
        case TOK_STAR_ASSIGN:
        case TOK_SLASH_ASSIGN:
        case TOK_MOD_ASSIGN:
        case TOK_AND_ASSIGN:
        case TOK_OR_ASSIGN:
        case TOK_XOR_ASSIGN:
        case TOK_LSHIFT_ASSIGN:
        case TOK_RSHIFT_ASSIGN:
        case TOK_URSHIFT_ASSIGN:
            return PREC_ASSIGNMENT;
        case TOK_QUESTION:
            return PREC_CONDITIONAL;
        case TOK_OR:
            return PREC_OR;
        case TOK_AND:
            return PREC_AND;
        case TOK_BITOR:
            return PREC_BITOR;
        case TOK_CARET:
            return PREC_BITXOR;
        case TOK_BITAND:
            return PREC_BITAND;
        case TOK_EQ:
        case TOK_NE:
            return PREC_EQUALITY;
        case TOK_LT:
        case TOK_GT:
        case TOK_LE:
        case TOK_GE:
        case TOK_INSTANCEOF:
            return PREC_RELATIONAL;
        case TOK_LSHIFT:
        case TOK_RSHIFT:
        case TOK_URSHIFT:
            return PREC_SHIFT;
        case TOK_PLUS:
        case TOK_MINUS:
            return PREC_ADDITIVE;
        case TOK_STAR:
        case TOK_SLASH:
        case TOK_MOD:
            return PREC_MULTIPLICATIVE;
        default:
            return PREC_NONE;
    }
}

/**
 * Check if operator is right-associative.
 */
static bool is_right_associative(token_type_t type)
{
    switch (type) {
        case TOK_ASSIGN:
        case TOK_PLUS_ASSIGN:
        case TOK_MINUS_ASSIGN:
        case TOK_STAR_ASSIGN:
        case TOK_SLASH_ASSIGN:
        case TOK_MOD_ASSIGN:
        case TOK_AND_ASSIGN:
        case TOK_OR_ASSIGN:
        case TOK_XOR_ASSIGN:
        case TOK_LSHIFT_ASSIGN:
        case TOK_RSHIFT_ASSIGN:
        case TOK_URSHIFT_ASSIGN:
        case TOK_QUESTION:
            return true;
        default:
            return false;
    }
}

/* Stack entry for expression parsing */
typedef struct expr_frame
{
    ast_node_t *node;           /* Current node being built */
    token_type_t op;            /* Pending operator */
    precedence_t min_prec;      /* Minimum precedence for this level */
    int state;                  /* State within this frame */
} expr_frame_t;

/* Forward declarations for mutual recursion */
static ast_node_t *parse_unary(parser_t *parser);
static ast_node_t *parse_statement(parser_t *parser);
static ast_node_t *parse_pattern(parser_t *parser);
static ast_node_t *parse_new_expression_with_outer(parser_t *parser, ast_node_t *outer);
static ast_node_t *parse_annotation(parser_t *parser);

/**
 * Parse a qualified new expression: outer.new InnerClass(args)
 * The 'new' keyword has already been consumed.
 * The outer instance is stored in node.extra for semantic analysis/codegen.
 */
static ast_node_t *parse_new_expression_with_outer(parser_t *parser, ast_node_t *outer)
{
    int line = parser_current_line(parser);
    int col = parser_current_column(parser);
    
    /* Parse type (the inner class name) */
    ast_node_t *type_node = parse_type(parser);
    if (!type_node) {
        ast_free(outer);
        return NULL;
    }
    
    /* Object creation with outer instance */
    ast_node_t *node = ast_new(AST_NEW_OBJECT, line, col);
    
    /* Store outer instance in extra field */
    node->data.node.extra = outer;
    
    /* First child is the type (same as regular new) */
    ast_add_child(node, type_node);
    
    /* Parse arguments */
    if (!parser_expect(parser, TOK_LPAREN)) {
        ast_free(node);
        return NULL;
    }
    
    if (!parser_check(parser, TOK_RPAREN)) {
        do {
            ast_node_t *arg = parse_expression(parser);
            if (arg) {
                ast_add_child(node, arg);
            }
        } while (parser_match(parser, TOK_COMMA));
    }
    
    if (!parser_expect(parser, TOK_RPAREN)) {
        ast_free(node);
        return NULL;
    }
    
    /* Optional anonymous class body */
    if (parser_check(parser, TOK_LBRACE)) {
        ast_node_t *body = parse_class_body(parser);
        if (body) {
            ast_add_child(node, body);
        }
    }
    
    return node;
}

/**
 * Parse a pattern for switch expressions (Java 21+).
 * Patterns can be:
 *   - Type patterns: String s, int x
 *   - Unnamed patterns: _
 *   - Unnamed type patterns: String _
 *   - Record patterns: Point(int x, int y), Point(_, int y)
 *   - Nested record patterns: Pair(Point(int x, _), Point p2)
 * Returns NULL if not a valid pattern (should parse as expression instead).
 */
static ast_node_t *parse_pattern(parser_t *parser)
{
    int pattern_line = parser_current_line(parser);
    int pattern_col = parser_current_column(parser);
    
    /* Check for standalone unnamed pattern: _ */
    if (parser_check(parser, TOK_IDENTIFIER)) {
        const char *first_ident = parser_current_text(parser);
        if (strcmp(first_ident, "_") == 0) {
            lexer_pos_t save_pos = lexer_save_pos(parser->lexer);
            parser_advance(parser);
            
            /* If next is ), , or when, it's a standalone unnamed pattern */
            if (parser_check(parser, TOK_RPAREN) || parser_check(parser, TOK_COMMA) || 
                parser_check(parser, TOK_WHEN) || parser_check(parser, TOK_ARROW)) {
                return ast_new(AST_UNNAMED_PATTERN, pattern_line, pattern_col);
            }
            /* Otherwise restore and fall through */
            lexer_restore_pos(parser->lexer, save_pos);
        }
    }
    
    /* Try to parse as type (can be identifier or primitive type) */
    if (!is_type_start(parser_current_type(parser))) {
        return NULL;
    }
    
    lexer_pos_t save_pos = lexer_save_pos(parser->lexer);
    ast_node_t *type_node = parse_type(parser);
    
    if (!type_node) {
        lexer_restore_pos(parser->lexer, save_pos);
        return NULL;
    }
    
    /* Check for record pattern: Type(...) */
    if (parser_check(parser, TOK_LPAREN)) {
        parser_advance(parser);  /* consume ( */
        
        ast_node_t *record_pattern = ast_new(AST_RECORD_PATTERN, pattern_line, pattern_col);
        ast_add_child(record_pattern, type_node);  /* First child is the record type */
        
        /* Parse component patterns */
        if (!parser_check(parser, TOK_RPAREN)) {
            do {
                ast_node_t *component = parse_pattern(parser);
                if (component) {
                    ast_add_child(record_pattern, component);
                } else {
                    /* Failed to parse pattern - error */
                    parser_error(parser, "Expected pattern in record pattern");
                    ast_free(record_pattern);
                    return NULL;
                }
            } while (parser_match(parser, TOK_COMMA));
        }
        
        if (!parser_expect(parser, TOK_RPAREN)) {
            ast_free(record_pattern);
            return NULL;
        }
        
        return record_pattern;
    }
    
    /* Check for type pattern: Type varName */
    if (parser_check(parser, TOK_IDENTIFIER)) {
        const char *var_name = parser_current_text(parser);
        int is_unnamed = (strcmp(var_name, "_") == 0);
        parser_advance(parser);
        
        ast_node_t *pattern = ast_new(AST_TYPE_PATTERN, pattern_line, pattern_col);
        pattern->data.node.name = is_unnamed ? NULL : strdup(var_name);
        ast_add_child(pattern, type_node);
        
        return pattern;
    }
    
    /* Not a pattern - restore and return NULL */
    lexer_restore_pos(parser->lexer, save_pos);
    ast_free(type_node);
    return NULL;
}

/**
 * Parse a primary expression (literals, identifiers, parenthesized, etc.)
 */
static ast_node_t *parse_primary(parser_t *parser)
{
    token_type_t tok_type = parser_current_type(parser);
    
    if (tok_type == TOK_EOF) {
        parser_error(parser, "Unexpected end of input");
        return NULL;
    }
    
    switch (tok_type) {
        case TOK_INTEGER_LITERAL:
        case TOK_LONG_LITERAL:
        case TOK_FLOAT_LITERAL:
        case TOK_DOUBLE_LITERAL:
        case TOK_CHAR_LITERAL:
        case TOK_STRING_LITERAL:
        case TOK_TEXT_BLOCK:
        case TOK_TRUE:
        case TOK_FALSE:
        case TOK_NULL:
            {
                ast_node_t *node = ast_new_literal_from_lexer(parser->lexer);
                parser_advance(parser);
                return node;
            }
            
        case TOK_IDENTIFIER:
            {
                int line = parser_current_line(parser);
                int col = parser_current_column(parser);
                ast_node_t *node = ast_new_leaf(AST_IDENTIFIER, parser_current_text(parser), 
                                                line, col);
                parser_advance(parser);
                
                /* Check for lambda: identifier -> body */
                if (parser_check(parser, TOK_ARROW)) {
                    parser_advance(parser);
                    ast_node_t *lambda = ast_new(AST_LAMBDA_EXPR, line, col);
                    ast_add_child(lambda, node);  /* Parameter */
                    
                    /* Parse lambda body (expression or block) */
                    if (parser_check(parser, TOK_LBRACE)) {
                        ast_node_t *body = parse_block(parser);
                        ast_add_child(lambda, body);
                    } else {
                        ast_node_t *body = parse_expression(parser);
                        ast_add_child(lambda, body);
                    }
                    return lambda;
                }
                
                return node;
            }
            
        case TOK_THIS:
            {
                ast_node_t *node = ast_new_leaf(AST_THIS_EXPR, "this",
                                                parser_current_line(parser), parser_current_column(parser));
                parser_advance(parser);
                return node;
            }
            
        case TOK_SUPER:
            {
                ast_node_t *node = ast_new_leaf(AST_SUPER_EXPR, "super",
                                                parser_current_line(parser), parser_current_column(parser));
                parser_advance(parser);
                return node;
            }
            
        case TOK_LPAREN:
            {
                /* Could be parenthesized expression, cast, or lambda params */
                int line = parser_current_line(parser);
                int col = parser_current_column(parser);
                parser_advance(parser);
                
                /* Check for empty lambda: () -> body */
                if (parser_check(parser, TOK_RPAREN)) {
                    parser_advance(parser);
                    if (parser_check(parser, TOK_ARROW)) {
                        parser_advance(parser);
                        ast_node_t *lambda = ast_new(AST_LAMBDA_EXPR, line, col);
                        /* Add empty params placeholder */
                        ast_node_t *empty_params = ast_new(AST_PARENTHESIZED, line, col);
                        ast_add_child(lambda, empty_params);
                        
                        /* Parse lambda body (expression or block) */
                        if (parser_check(parser, TOK_LBRACE)) {
                            ast_node_t *body = parse_block(parser);
                            ast_add_child(lambda, body);
                        } else {
                            ast_node_t *body = parse_expression(parser);
                            ast_add_child(lambda, body);
                        }
                        return lambda;
                    }
                    /* Empty parentheses not followed by arrow - syntax error or unit */
                    parser_error(parser, "Empty parentheses without lambda arrow");
                    return NULL;
                }
                
                /* Check if this looks like a cast: (Type) expr */
                /* Cast if followed by primitive type, or identifier followed by ) and primary */
                /* But (int x) -> is a lambda, not a cast! */
                if (is_primitive_type(parser_current_type(parser))) {
                    /* Lookahead to distinguish cast (int)x from lambda (int x) -> */
                    lexer_pos_t prim_save = lexer_save_pos(parser->lexer);
                    parser_advance(parser);  /* consume primitive type */
                    
                    /* Check for array dimensions after type: (int[])x vs (int[] x) -> */
                    while (parser_check(parser, TOK_LBRACKET)) {
                        parser_advance(parser);
                        if (!parser_check(parser, TOK_RBRACKET)) {
                            /* Expression in brackets - not a type */
                            lexer_restore_pos(parser->lexer, prim_save);
                            goto check_lambda_params;  /* Parse as lambda */
                        }
                        parser_advance(parser);  /* consume ] */
                    }
                    
                    if (parser_check(parser, TOK_RPAREN)) {
                        /* It's a cast: (int) or (int[]) */
                        lexer_restore_pos(parser->lexer, prim_save);
                        ast_node_t *type_node = parse_type(parser);
                        if (!type_node) {
                            return NULL;
                        }
                        
                        if (!parser_expect(parser, TOK_RPAREN)) {
                            ast_free(type_node);
                            return NULL;
                        }
                        
                        /* Parse the expression being cast */
                        ast_node_t *operand = parse_unary(parser);
                        if (!operand) {
                            ast_free(type_node);
                            return NULL;
                        }
                        
                        ast_node_t *cast = ast_new(AST_CAST_EXPR, line, col);
                        ast_add_child(cast, type_node);
                        ast_add_child(cast, operand);
                        return cast;
                    } else {
                        /* It's a lambda with typed params: (int x) -> */
                        lexer_restore_pos(parser->lexer, prim_save);
                        goto check_lambda_params;
                    }
                }
                
                /* Check for reference type cast: (ClassName) expr */
                if (parser_current_type(parser) == TOK_IDENTIFIER) {
                    /* Peek ahead to see if this is (Type) expr or (expr) */
                    lexer_pos_t save_pos = lexer_save_pos(parser->lexer);
                    
                    /* Try parsing as a type */
                    ast_node_t *maybe_type = parse_type(parser);
                    
                    if (maybe_type && parser_check(parser, TOK_RPAREN)) {
                        /* Look ahead: if followed by identifier, literal, (, !, ~, +, -, ++, --, new, this
                         * then it's a cast. Otherwise it's a parenthesized expression */
                        parser_advance(parser);  /* Move past ) to see what's next */
                        token_type_t next_type = parser_current_type(parser);
                        /* Note: TOK_INC and TOK_DEC are NOT in this list because
                         * (expr)++ should parse as postfix increment, not a cast.
                         * A cast like (int)++x is rare and would need prefix ++. */
                        bool looks_like_cast = 
                            next_type == TOK_IDENTIFIER ||
                            next_type == TOK_INTEGER_LITERAL ||
                            next_type == TOK_LONG_LITERAL ||
                            next_type == TOK_FLOAT_LITERAL ||
                            next_type == TOK_DOUBLE_LITERAL ||
                            next_type == TOK_CHAR_LITERAL ||
                            next_type == TOK_STRING_LITERAL ||
                            next_type == TOK_TRUE ||
                            next_type == TOK_FALSE ||
                            next_type == TOK_NULL ||
                            next_type == TOK_LPAREN ||
                            next_type == TOK_NOT ||
                            next_type == TOK_TILDE ||
                            next_type == TOK_PLUS ||
                            next_type == TOK_MINUS ||
                            next_type == TOK_NEW ||
                            next_type == TOK_THIS ||
                            next_type == TOK_SUPER;
                        
                        if (looks_like_cast) {
                            /* It's a cast - already past ), parse operand */
                            ast_node_t *operand = parse_unary(parser);
                            if (!operand) {
                                ast_free(maybe_type);
                                return NULL;
                            }
                            
                            ast_node_t *cast = ast_new(AST_CAST_EXPR, line, col);
                            ast_add_child(cast, maybe_type);
                            ast_add_child(cast, operand);
                            return cast;
                        }
                    }
                    
                    /* Not a cast, restore position and parse as parenthesized expression */
                    ast_free(maybe_type);
                    lexer_restore_pos(parser->lexer, save_pos);
                }
                
                /* Parse as parenthesized expression or lambda parameters */
                /* Lambda forms:
                 *   (id, id, ...) ->           - untyped parameters
                 *   (Type id, Type id, ...) -> - typed parameters
                 *   (var id, var id, ...) ->   - var parameters (Java 11+)
                 */
                check_lambda_params:;
                lexer_pos_t lambda_check_pos = lexer_save_pos(parser->lexer);
                bool looks_like_lambda_params = true;
                bool typed_params = false;
                int param_count = 0;
                
                /* Try to parse as lambda parameter list */
                while (looks_like_lambda_params) {
                    token_type_t tok = parser_current_type(parser);
                    
                    /* Check for typed parameter: Type name or var name */
                    if (tok == TOK_VAR || is_primitive_type(tok)) {
                        /* Definitely typed: var x or int x, etc. */
                        typed_params = true;
                        parser_advance(parser);  /* consume type */
                        if (!parser_check(parser, TOK_IDENTIFIER)) {
                            looks_like_lambda_params = false;
                            break;
                        }
                        parser_advance(parser);  /* consume name */
                        param_count++;
                        /* After primitive type param, check for ) or , */
                        if (parser_check(parser, TOK_RPAREN)) {
                            parser_advance(parser);
                            if (parser_check(parser, TOK_ARROW)) {
                                /* This IS a lambda with parameters */
                                break;
                            } else {
                                looks_like_lambda_params = false;
                            }
                        } else if (parser_check(parser, TOK_COMMA)) {
                            parser_advance(parser);
                            /* Continue to next parameter */
                        } else {
                            looks_like_lambda_params = false;
                        }
                        continue;
                    } else if (tok == TOK_IDENTIFIER) {
                        /* Could be untyped (x) or typed (Type x) or typed with generics (List<T> x)
                         * or typed with qualified name (Map.Entry<K,V> x) */
                        parser_advance(parser);
                        
                        /* Handle qualified type names: Outer.Inner or a.b.c.Type */
                        while (parser_check(parser, TOK_DOT)) {
                            parser_advance(parser);  /* consume . */
                            if (parser_check(parser, TOK_IDENTIFIER)) {
                                parser_advance(parser);  /* consume next part */
                            } else {
                                looks_like_lambda_params = false;
                                break;
                            }
                        }
                        if (!looks_like_lambda_params) break;
                        
                        /* Check for parameterized type: Type<...> or Outer.Inner<...> */
                        if (parser_check(parser, TOK_LT)) {
                            /* Skip over type arguments <...> - track nested angle brackets */
                            int depth = 1;
                            parser_advance(parser);  /* consume < */
                            while (depth > 0 && !parser_check(parser, TOK_EOF)) {
                                if (parser_check(parser, TOK_LT)) {
                                    depth++;
                                } else if (parser_check(parser, TOK_GT)) {
                                    depth--;
                                } else if (parser_check(parser, TOK_RSHIFT)) {
                                    /* >> can close two levels of generics */
                                    depth -= 2;
                                }
                                parser_advance(parser);
                            }
                            if (depth != 0) {
                                looks_like_lambda_params = false;
                                break;
                            }
                            /* After <...>, should have parameter name */
                            if (parser_check(parser, TOK_IDENTIFIER)) {
                                typed_params = true;
                                parser_advance(parser);  /* consume name */
                            } else {
                                looks_like_lambda_params = false;
                                break;
                            }
                        } else if (parser_check(parser, TOK_IDENTIFIER)) {
                            /* Two identifiers = typed: Type name */
                            typed_params = true;
                            parser_advance(parser);  /* consume name */
                        }
                        /* else single identifier = untyped */
                        param_count++;
                    } else {
                        looks_like_lambda_params = false;
                        break;
                    }
                    
                    if (parser_check(parser, TOK_RPAREN)) {
                        parser_advance(parser);
                        if (parser_check(parser, TOK_ARROW)) {
                            /* This IS a lambda with parameters */
                            break;
                        } else {
                            looks_like_lambda_params = false;
                        }
                    } else if (parser_check(parser, TOK_COMMA)) {
                        parser_advance(parser);
                        /* Continue to next parameter */
                    } else {
                        looks_like_lambda_params = false;
                    }
                }
                
                /* Restore position after lookahead */
                lexer_restore_pos(parser->lexer, lambda_check_pos);
                
                if (looks_like_lambda_params && param_count > 0) {
                    /* Parse as lambda with parameters */
                    ast_node_t *params_list = ast_new(AST_LAMBDA_PARAMS, line, col);
                    
                    do {
                        ast_node_t *param_type = NULL;
                        
                        if (typed_params) {
                            /* Parse type (var, primitive, or class type) */
                            param_type = parse_type(parser);
                        }
                        
                        if (!parser_check(parser, TOK_IDENTIFIER)) {
                            ast_free(params_list);
                            return NULL;
                        }
                        
                        /* Create parameter node */
                        ast_node_t *param = ast_new(AST_PARAMETER, 
                                                    parser_current_line(parser),
                                                    parser_current_column(parser));
                        param->data.node.name = strdup(parser_current_text(parser));
                        if (param_type) {
                            ast_add_child(param, param_type);
                        }
                        parser_advance(parser);
                        
                        ast_add_child(params_list, param);
                    } while (parser_match(parser, TOK_COMMA));
                    
                    if (!parser_expect(parser, TOK_RPAREN)) {
                        ast_free(params_list);
                        return NULL;
                    }
                    
                    if (!parser_expect(parser, TOK_ARROW)) {
                        ast_free(params_list);
                        return NULL;
                    }
                    
                    ast_node_t *lambda = ast_new(AST_LAMBDA_EXPR, line, col);
                    ast_add_child(lambda, params_list);
                    
                    /* Parse lambda body */
                    if (parser_check(parser, TOK_LBRACE)) {
                        ast_node_t *body = parse_block(parser);
                        ast_add_child(lambda, body);
                    } else {
                        ast_node_t *body = parse_expression(parser);
                        ast_add_child(lambda, body);
                    }
                    return lambda;
                }
                
                /* Parse as regular parenthesized expression */
                ast_node_t *inner = parse_expression(parser);
                if (!inner) {
                    return NULL;
                }
                
                if (!parser_expect(parser, TOK_RPAREN)) {
                    ast_free(inner);
                    return NULL;
                }
                
                /* Check for lambda arrow */
                if (parser_check(parser, TOK_ARROW)) {
                    parser_advance(parser);
                    ast_node_t *lambda = ast_new(AST_LAMBDA_EXPR, line, col);
                    ast_add_child(lambda, inner);
                    
                    /* Parse lambda body (expression or block) */
                    if (parser_check(parser, TOK_LBRACE)) {
                        ast_node_t *body = parse_block(parser);
                        ast_add_child(lambda, body);
                    } else {
                        ast_node_t *body = parse_expression(parser);
                        ast_add_child(lambda, body);
                    }
                    return lambda;
                }
                
                ast_node_t *node = ast_new(AST_PARENTHESIZED, line, col);
                ast_add_child(node, inner);
                return node;
            }
            
        case TOK_NEW:
            {
                int line = parser_current_line(parser);
                int col = parser_current_column(parser);
                parser_advance(parser);
                
                /* Parse type */
                ast_node_t *type_node = parse_type(parser);
                if (!type_node) {
                    return NULL;
                }
                
                /* Check if type is an array type OR if next token is [ */
                /* Note: parse_type may have consumed int[] as AST_ARRAY_TYPE */
                if (parser_check(parser, TOK_LBRACKET) || type_node->type == AST_ARRAY_TYPE) {
                    /* Array creation */
                    ast_node_t *node = ast_new(AST_NEW_ARRAY, line, col);
                    
                    /* If type is already AST_ARRAY_TYPE, unwrap to get element type */
                    if (type_node->type == AST_ARRAY_TYPE) {
                        /* Get element type from array type */
                        ast_node_t *elem_type = NULL;
                        if (type_node->data.node.children) {
                            elem_type = (ast_node_t *)type_node->data.node.children->data;
                        }
                        if (elem_type) {
                            ast_add_child(node, elem_type);
                        }
                        /* Don't free type_node as elem_type is still referenced */
                    } else {
                        ast_add_child(node, type_node);
                    }
                    
                    /* Parse any additional dimensions */
                    while (parser_match(parser, TOK_LBRACKET)) {
                        if (!parser_check(parser, TOK_RBRACKET)) {
                            ast_node_t *dim = parse_expression(parser);
                            if (dim) {
                                ast_add_child(node, dim);
                            }
                        }
                        if (!parser_expect(parser, TOK_RBRACKET)) {
                            ast_free(node);
                            return NULL;
                        }
                    }
                    
                    /* Optional array initializer */
                    if (parser_check(parser, TOK_LBRACE)) {
                        ast_node_t *init = parse_array_initializer(parser);
                        if (init) {
                            ast_add_child(node, init);
                        }
                    }
                    
                    return node;
                } else {
                    /* Object creation */
                    ast_node_t *node = ast_new(AST_NEW_OBJECT, line, col);
                    ast_add_child(node, type_node);
                    
                    /* Parse arguments */
                    if (!parser_expect(parser, TOK_LPAREN)) {
                        ast_free(node);
                        return NULL;
                    }
                    
                    if (!parser_check(parser, TOK_RPAREN)) {
                        do {
                            ast_node_t *arg = parse_expression(parser);
                            if (arg) {
                                ast_add_child(node, arg);
                            }
                        } while (parser_match(parser, TOK_COMMA));
                    }
                    
                    if (!parser_expect(parser, TOK_RPAREN)) {
                        ast_free(node);
                        return NULL;
                    }
                    
                    /* Optional anonymous class body */
                    if (parser_check(parser, TOK_LBRACE)) {
                        ast_node_t *body = parse_class_body(parser);
                        if (body) {
                            ast_add_child(node, body);
                        }
                    }
                    
                    return node;
                }
            }
            
        case TOK_SWITCH:
            {
                /* Switch expression (Java 12+) */
                parser_advance(parser);
                int sw_line = parser_current_line(parser);
                int sw_col = parser_current_column(parser);
                
                if (!parser_expect(parser, TOK_LPAREN)) {
                    return NULL;
                }
                
                ast_node_t *selector = parse_expression(parser);
                if (!selector) {
                    return NULL;
                }
                
                if (!parser_expect(parser, TOK_RPAREN)) {
                    ast_free(selector);
                    return NULL;
                }
                
                if (!parser_expect(parser, TOK_LBRACE)) {
                    ast_free(selector);
                    return NULL;
                }
                
                ast_node_t *node = ast_new(AST_SWITCH_EXPR, sw_line, sw_col);
                ast_add_child(node, selector);
                
                /* Parse switch rules (arrow-style cases) */
                while (!parser_check(parser, TOK_RBRACE) && !parser_check(parser, TOK_EOF)) {
                    if (parser_check(parser, TOK_CASE) || parser_check(parser, TOK_DEFAULT)) {
                        ast_node_t *rule = ast_new(AST_SWITCH_RULE,
                                                   parser_current_line(parser),
                                                   parser_current_column(parser));
                        
                        if (parser_match(parser, TOK_DEFAULT)) {
                            rule->data.node.name = strdup("default");
                        } else {
                            parser_advance(parser);  /* consume 'case' */
                            /* Parse one or more case patterns/constants separated by commas */
                            do {
                                int pattern_line = parser_current_line(parser);
                                int pattern_col = parser_current_column(parser);
                                
                                /* Check for null pattern */
                                if (parser_check(parser, TOK_NULL)) {
                                    ast_node_t *null_lit = ast_new_literal_from_lexer(parser->lexer);
                                    parser_advance(parser);
                                    ast_add_child(rule, null_lit);
                                }
                                /* Try to parse as a pattern (type, unnamed, record) */
                                else if (parser_check(parser, TOK_IDENTIFIER)) {
                                    ast_node_t *pattern = parse_pattern(parser);
                                    
                                    if (pattern) {
                                        /* Check for when guard */
                                        if (parser_match(parser, TOK_WHEN)) {
                                            ast_node_t *guard = parse_expression(parser);
                                            ast_node_t *guarded = ast_new(AST_GUARDED_PATTERN, pattern_line, pattern_col);
                                            ast_add_child(guarded, pattern);
                                            if (guard) {
                                                ast_add_child(guarded, guard);
                                            }
                                            ast_add_child(rule, guarded);
                                        } else {
                                            ast_add_child(rule, pattern);
                                        }
                                    } else {
                                        /* Not a pattern, parse as expression */
                                        ast_node_t *case_expr = parse_expression(parser);
                                        if (case_expr) {
                                            ast_add_child(rule, case_expr);
                                        }
                                    }
                                } else {
                                    /* Regular expression */
                                    ast_node_t *case_expr = parse_expression(parser);
                                    if (case_expr) {
                                        ast_add_child(rule, case_expr);
                                    }
                                }
                            } while (parser_match(parser, TOK_COMMA));
                        }
                        
                        if (!parser_expect(parser, TOK_ARROW)) {
                            ast_free(rule);
                            ast_free(node);
                            return NULL;
                        }
                        
                        /* After ->, either a block { ... } or an expression followed by ; */
                        if (parser_check(parser, TOK_LBRACE)) {
                            ast_node_t *block = parse_block(parser);
                            if (block) {
                                ast_add_child(rule, block);
                            }
                        } else if (parser_check(parser, TOK_THROW)) {
                            /* throw expression; */
                            ast_node_t *throw_stmt = parse_statement(parser);
                            if (throw_stmt) {
                                ast_add_child(rule, throw_stmt);
                            }
                        } else {
                            /* Expression followed by semicolon */
                            ast_node_t *expr = parse_expression(parser);
                            if (expr) {
                                ast_add_child(rule, expr);
                            }
                            parser_expect(parser, TOK_SEMICOLON);
                        }
                        
                        ast_add_child(node, rule);
                    } else {
                        parser_error(parser, "Expected 'case' or 'default'");
                        ast_free(node);
                        return NULL;
                    }
                }
                
                parser_expect(parser, TOK_RBRACE);
                return node;
            }
            
        /* Primitive types for class literals: boolean.class, int.class, etc. */
        case TOK_BOOLEAN:
        case TOK_BYTE:
        case TOK_CHAR:
        case TOK_SHORT:
        case TOK_INT:
        case TOK_LONG:
        case TOK_FLOAT:
        case TOK_DOUBLE:
        case TOK_VOID:
            {
                int line = parser_current_line(parser);
                int col = parser_current_column(parser);
                
                /* Parse as a type */
                ast_node_t *type_node = ast_new_leaf(AST_PRIMITIVE_TYPE, 
                    token_type_name(tok_type), line, col);
                parser_advance(parser);
                
                /* Handle array dimensions: int[].class */
                while (parser_check(parser, TOK_LBRACKET)) {
                    parser_advance(parser);  /* consume [ */
                    if (!parser_expect(parser, TOK_RBRACKET)) {
                        ast_free(type_node);
                        return NULL;
                    }
                    ast_node_t *array_type = ast_new(AST_ARRAY_TYPE, line, col);
                    ast_add_child(array_type, type_node);
                    type_node = array_type;
                }
                
                /* Must be followed by .class */
                if (!parser_check(parser, TOK_DOT)) {
                    parser_error(parser, "Primitive type must be followed by .class");
                    ast_free(type_node);
                    return NULL;
                }
                parser_advance(parser);  /* consume . */
                
                if (!parser_check(parser, TOK_CLASS)) {
                    parser_error(parser, "Expected 'class' after primitive type.");
                    ast_free(type_node);
                    return NULL;
                }
                parser_advance(parser);  /* consume class */
                
                /* Create class literal node */
                ast_node_t *class_literal = ast_new(AST_CLASS_LITERAL, line, col);
                ast_add_child(class_literal, type_node);
                return class_literal;
            }
            
        default:
            parser_error(parser, "Expected expression");
            return NULL;
    }
}

/* Forward declaration for mutual recursion */
static ast_node_t *parse_postfix_operand(parser_t *parser, ast_node_t *expr);

/**
 * Parse unary expression (prefix operators).
 * Note: Unary operators have lower precedence than postfix operators like method calls.
 * So !name.equals() parses as !(name.equals()), not (!name).equals()
 */
static ast_node_t *parse_unary(parser_t *parser)
{
    token_type_t tok_type = parser_current_type(parser);
    
    switch (tok_type) {
        case TOK_NOT:
        case TOK_TILDE:
        case TOK_PLUS:
        case TOK_MINUS:
        case TOK_INC:
        case TOK_DEC:
            {
                int line = parser_current_line(parser);
                int col = parser_current_column(parser);
                token_type_t op_type = tok_type;
                const char *op = token_type_name(op_type);
                parser_advance(parser);
                
                ast_node_t *operand = parse_unary(parser);
                if (!operand) {
                    return NULL;
                }
                
                ast_node_t *node = ast_new(AST_UNARY_EXPR, line, col);
                node->data.node.name = strdup(op);
                node->data.node.op_token = op_type;  /* Store operator token */
                ast_add_child(node, operand);
                return node;
            }
            
        default:
            {
                /* Parse primary and then handle postfix operations
                 * This ensures !name.method() parses as !(name.method()) */
                ast_node_t *expr = parse_primary(parser);
                if (!expr) {
                    return NULL;
                }
                return parse_postfix_operand(parser, expr);
            }
    }
}

/**
 * Handle postfix operations on an already-parsed expression.
 * This is called from parse_unary for non-unary operands.
 */
static ast_node_t *parse_postfix_operand(parser_t *parser, ast_node_t *expr)
{
    /* Loop to handle chained postfix operations */
    while (1) {
        token_type_t tok_type = parser_current_type(parser);
        if (tok_type == TOK_EOF) {
            break;
        }
        
        switch (tok_type) {
            case TOK_DOT:
                {
                    parser_advance(parser);
                    int line = parser_current_line(parser);
                    int col = parser_current_column(parser);
                    
                    /* Handle qualified new: expr.new InnerClass() */
                    if (parser_check(parser, TOK_NEW)) {
                        parser_advance(parser);
                        ast_node_t *new_expr = parse_new_expression_with_outer(parser, expr);
                        if (!new_expr) {
                            return NULL;
                        }
                        expr = new_expr;
                        break;
                    }
                    
                    /* Handle qualified this: OuterClass.this */
                    if (parser_check(parser, TOK_THIS)) {
                        parser_advance(parser);
                        ast_node_t *this_expr = ast_new(AST_FIELD_ACCESS, line, col);
                        this_expr->data.node.name = strdup("this");
                        ast_add_child(this_expr, expr);
                        expr = this_expr;
                        break;
                    }
                    
                    /* Handle qualified super: OuterClass.super */
                    if (parser_check(parser, TOK_SUPER)) {
                        parser_advance(parser);
                        ast_node_t *super_expr = ast_new(AST_FIELD_ACCESS, line, col);
                        super_expr->data.node.name = strdup("super");
                        ast_add_child(super_expr, expr);
                        expr = super_expr;
                        break;
                    }
                    
                    /* Handle class literal: Type.class */
                    if (parser_check(parser, TOK_CLASS)) {
                        parser_advance(parser);
                        
                        /* Convert expression to type node for class literal */
                        ast_node_t *type_node;
                        if (expr->type == AST_IDENTIFIER) {
                            /* Simple type name - convert identifier to class type */
                            type_node = ast_new(AST_CLASS_TYPE, expr->line, expr->column);
                            type_node->data.node.name = strdup(expr->data.leaf.name);
                            ast_free(expr);
                        } else if (expr->type == AST_ARRAY_TYPE || expr->type == AST_CLASS_TYPE ||
                                   expr->type == AST_FIELD_ACCESS) {
                            /* Already a type node or qualified name */
                            type_node = expr;
                        } else {
                            /* Unexpected expression before .class */
                            parser_error(parser, "Invalid expression before .class");
                            ast_free(expr);
                            return NULL;
                        }
                        
                        ast_node_t *class_literal = ast_new(AST_CLASS_LITERAL, line, col);
                        ast_add_child(class_literal, type_node);
                        expr = class_literal;
                        break;
                    }
                    
                    if (!parser_check(parser, TOK_IDENTIFIER)) {
                        parser_error(parser, "Expected identifier after '.'");
                        ast_free(expr);
                        return NULL;
                    }
                    
                    char *name = strdup(parser_current_text(parser));
                    parser_advance(parser);
                    
                    if (parser_check(parser, TOK_LPAREN)) {
                        /* Method call with explicit receiver (obj.method()) */
                        ast_node_t *call = ast_new(AST_METHOD_CALL, line, col);
                        call->data.node.name = name;
                        call->data.node.flags |= AST_METHOD_CALL_EXPLICIT_RECEIVER;
                        ast_add_child(call, expr);
                        
                        parser_advance(parser);  /* consume ( */
                        
                        /* Parse arguments */
                        if (!parser_check(parser, TOK_RPAREN)) {
                            do {
                                ast_node_t *arg = parse_expression(parser);
                                if (arg) {
                                    ast_add_child(call, arg);
                                }
                            } while (parser_match(parser, TOK_COMMA));
                        }
                        
                        if (!parser_expect(parser, TOK_RPAREN)) {
                            ast_free(call);
                            return NULL;
                        }
                        
                        expr = call;
                    } else {
                        /* Field access */
                        ast_node_t *access = ast_new(AST_FIELD_ACCESS, line, col);
                        access->data.node.name = name;
                        ast_add_child(access, expr);
                        expr = access;
                    }
                }
                break;
                
            case TOK_LBRACKET:
                {
                    int line = parser_current_line(parser);
                    int col = parser_current_column(parser);
                    parser_advance(parser);
                    
                    /* Check if this is an empty [] for array type (e.g., String[]::new) */
                    if (parser_check(parser, TOK_RBRACKET)) {
                        parser_advance(parser);
                        /* This is an array type marker, not an array access */
                        ast_node_t *array_type = ast_new(AST_ARRAY_TYPE, line, col);
                        ast_add_child(array_type, expr);
                        expr = array_type;
                    } else {
                        /* Array access with index expression */
                        ast_node_t *index = parse_expression(parser);
                        if (!index) {
                            ast_free(expr);
                            return NULL;
                        }
                        
                        if (!parser_expect(parser, TOK_RBRACKET)) {
                            ast_free(expr);
                            ast_free(index);
                            return NULL;
                        }
                        
                        ast_node_t *access = ast_new(AST_ARRAY_ACCESS, line, col);
                        ast_add_child(access, expr);
                        ast_add_child(access, index);
                        expr = access;
                    }
                }
                break;
                
            case TOK_LPAREN:
                {
                    /* Method call on identifier or explicit constructor invocation */
                    int line = parser_current_line(parser);
                    int col = parser_current_column(parser);
                    
                    if (expr->type == AST_IDENTIFIER) {
                        ast_node_t *call = ast_new(AST_METHOD_CALL, line, col);
                        call->data.node.name = strdup(expr->data.leaf.name);
                        ast_free(expr);
                        
                        parser_advance(parser);  /* consume ( */
                        
                        /* Parse arguments */
                        if (!parser_check(parser, TOK_RPAREN)) {
                            do {
                                ast_node_t *arg = parse_expression(parser);
                                if (arg) {
                                    ast_add_child(call, arg);
                                }
                            } while (parser_match(parser, TOK_COMMA));
                        }
                        
                        if (!parser_expect(parser, TOK_RPAREN)) {
                            ast_free(call);
                            return NULL;
                        }
                        
                        expr = call;
                    } else if (expr->type == AST_THIS_EXPR || expr->type == AST_SUPER_EXPR) {
                        /* Explicit constructor invocation: this() or super() */
                        ast_node_t *call = ast_new(AST_EXPLICIT_CTOR_CALL, line, col);
                        /* Store whether it's this() or super() */
                        call->data.node.name = strdup(expr->type == AST_THIS_EXPR ? "this" : "super");
                        ast_free(expr);
                        
                        parser_advance(parser);  /* consume ( */
                        
                        /* Parse arguments */
                        if (!parser_check(parser, TOK_RPAREN)) {
                            do {
                                ast_node_t *arg = parse_expression(parser);
                                if (arg) {
                                    ast_add_child(call, arg);
                                }
                            } while (parser_match(parser, TOK_COMMA));
                        }
                        
                        if (!parser_expect(parser, TOK_RPAREN)) {
                            ast_free(call);
                            return NULL;
                        }
                        
                        expr = call;
                    } else {
                        /* Can't make a call on this expression type */
                        goto done;
                    }
                }
                break;
                
            case TOK_INC:
            case TOK_DEC:
                {
                    /* Postfix ++ or -- */
                    int line = parser_current_line(parser);
                    int col = parser_current_column(parser);
                    token_type_t op_type = tok_type;
                    const char *op = token_type_name(op_type);
                    parser_advance(parser);
                    
                    ast_node_t *node = ast_new(AST_UNARY_EXPR, line, col);
                    node->data.node.name = strdup(op);
                    node->data.node.op_token = op_type;
                    node->data.node.flags = 1;  /* Mark as postfix */
                    ast_add_child(node, expr);
                    expr = node;
                }
                break;
            
            case TOK_DOUBLE_COLON:
                {
                    /* Method reference: Type::method or expr::method */
                    int line = parser_current_line(parser);
                    int col = parser_current_column(parser);
                    parser_advance(parser);
                    
                    ast_node_t *ref = ast_new(AST_METHOD_REF, line, col);
                    ast_add_child(ref, expr);
                    
                    if (parser_check(parser, TOK_NEW)) {
                        ref->data.node.name = strdup("new");
                        parser_advance(parser);
                    } else if (parser_check(parser, TOK_IDENTIFIER)) {
                        ref->data.node.name = strdup(parser_current_text(parser));
                        parser_advance(parser);
                    } else {
                        parser_error(parser, "Expected method name or 'new' after '::'");
                        ast_free(ref);
                        return NULL;
                    }
                    
                    expr = ref;
                }
                break;
                
            default:
                goto done;
        }
    }
    
done:
    return expr;
}

/**
 * Parse postfix expressions (method calls, field access, array access, ++/--)
 * parse_unary now handles postfix operations for its operands, so this
 * just delegates to parse_unary.
 */
static ast_node_t *parse_postfix(parser_t *parser)
{
    return parse_unary(parser);
}

/* Forward declaration for internal use */
static ast_node_t *parse_expression_prec(parser_t *parser, precedence_t min_prec);

/**
 * Parse expression using iterative Pratt parser.
 */
ast_node_t *parse_expression(parser_t *parser)
{
    return parse_expression_prec(parser, PREC_ASSIGNMENT);
}

/**
 * Parse expression with minimum precedence (iterative Pratt parser).
 */
static ast_node_t *parse_expression_prec(parser_t *parser, precedence_t min_prec)
{
    /* Parse left-hand side (unary/postfix/primary) */
    ast_node_t *left = parse_postfix(parser);
    if (!left) {
        return NULL;
    }
    
    /* Iteratively handle binary operators */
    while (1) {
        token_type_t tok_type = parser_current_type(parser);
        if (tok_type == TOK_EOF) {
            break;
        }
        
        precedence_t prec = get_binary_precedence(tok_type);
        if (prec == PREC_NONE || prec < min_prec) {
            break;
        }
        
        /* Handle ternary conditional operator */
        if (tok_type == TOK_QUESTION) {
            int line = parser_current_line(parser);
            int col = parser_current_column(parser);
            parser_advance(parser);
            
            ast_node_t *then_expr = parse_expression(parser);
            if (!then_expr) {
                ast_free(left);
                return NULL;
            }
            
            if (!parser_expect(parser, TOK_COLON)) {
                ast_free(left);
                ast_free(then_expr);
                return NULL;
            }
            
            ast_node_t *else_expr = parse_expression_prec(parser, PREC_CONDITIONAL);
            if (!else_expr) {
                ast_free(left);
                ast_free(then_expr);
                return NULL;
            }
            
            ast_node_t *cond = ast_new(AST_CONDITIONAL_EXPR, line, col);
            ast_add_child(cond, left);
            ast_add_child(cond, then_expr);
            ast_add_child(cond, else_expr);
            left = cond;
            continue;
        }
        
        /* Handle instanceof */
        if (tok_type == TOK_INSTANCEOF) {
            int line = parser_current_line(parser);
            int col = parser_current_column(parser);
            parser_advance(parser);
            
            ast_node_t *type_node = parse_type(parser);
            if (!type_node) {
                ast_free(left);
                return NULL;
            }
            
            ast_node_t *inst = ast_new(AST_INSTANCEOF_EXPR, line, col);
            ast_add_child(inst, left);
            ast_add_child(inst, type_node);
            
            /* Pattern matching variable (Java 16+) */
            if (parser_check(parser, TOK_IDENTIFIER)) {
                ast_node_t *var = ast_new_leaf(AST_IDENTIFIER, parser_current_text(parser),
                                               parser_current_line(parser), parser_current_column(parser));
                parser_advance(parser);
                ast_add_child(inst, var);
            }
            
            left = inst;
            continue;
        }
        
        /* Regular binary operator */
        int line = parser_current_line(parser);
        int col = parser_current_column(parser);
        token_type_t op = tok_type;
        const char *op_name = token_type_name(op);
        parser_advance(parser);
        
        /* For right-associative operators, use same precedence */
        /* For left-associative, use prec + 1 */
        precedence_t next_prec = is_right_associative(op) ? prec : prec + 1;
        
        ast_node_t *right = parse_expression_prec(parser, next_prec);
        if (!right) {
            ast_free(left);
            return NULL;
        }
        
        /* Determine node type based on operator */
        ast_node_type_t node_type;
        switch (op) {
            case TOK_ASSIGN:
            case TOK_PLUS_ASSIGN:
            case TOK_MINUS_ASSIGN:
            case TOK_STAR_ASSIGN:
            case TOK_SLASH_ASSIGN:
            case TOK_MOD_ASSIGN:
            case TOK_AND_ASSIGN:
            case TOK_OR_ASSIGN:
            case TOK_XOR_ASSIGN:
            case TOK_LSHIFT_ASSIGN:
            case TOK_RSHIFT_ASSIGN:
            case TOK_URSHIFT_ASSIGN:
                node_type = AST_ASSIGNMENT_EXPR;
                break;
            default:
                node_type = AST_BINARY_EXPR;
                break;
        }
        
        ast_node_t *binary = ast_new(node_type, line, col);
        binary->data.node.name = strdup(op_name);
        binary->data.node.op_token = op;  /* Store operator token for efficient codegen */
        ast_add_child(binary, left);
        ast_add_child(binary, right);
        left = binary;
    }
    
    return left;
}

/* ========================================================================
 * Type Parsing
 * ======================================================================== */

/**
 * Parse type annotations (JSR 308, Java 8).
 * Type annotations can appear before types: @NonNull String
 * Returns list of annotation nodes, or NULL if none.
 */
static slist_t *parse_type_annotations(parser_t *parser)
{
    slist_t *annotations = NULL;
    
    while (parser_check(parser, TOK_AT)) {
        /* Check if this is @interface (annotation declaration), not a type annotation */
        if (parser_peek_type(parser) == TOK_INTERFACE) {
            break;
        }
        ast_node_t *annot = parse_annotation(parser);
        if (annot) {
            if (!annotations) {
                annotations = slist_new(annot);
            } else {
                slist_append(annotations, annot);
            }
        }
    }
    
    return annotations;
}

/**
 * Parse a type (primitive or reference).
 * Supports type annotations (JSR 308): @NonNull String, List<@NonNull String>
 */
ast_node_t *parse_type(parser_t *parser)
{
    /* Parse leading type annotations: @NonNull String */
    slist_t *type_annotations = parse_type_annotations(parser);
    
    token_type_t tok_type = parser_current_type(parser);
    ast_node_t *type_node = NULL;
    
    if (is_primitive_type(tok_type)) {
        type_node = ast_new_leaf(AST_PRIMITIVE_TYPE, token_type_name(tok_type),
                                 parser_current_line(parser), parser_current_column(parser));
        type_node->annotations = type_annotations;
        parser_advance(parser);
    } else if (tok_type == TOK_VOID) {
        type_node = ast_new_leaf(AST_PRIMITIVE_TYPE, "void",
                                 parser_current_line(parser), parser_current_column(parser));
        type_node->annotations = type_annotations;
        parser_advance(parser);
    } else if (tok_type == TOK_VAR) {
        /* var for type inference (Java 10+ local variables, Java 11+ lambda params) */
        type_node = ast_new_leaf(AST_VAR_TYPE, "var",
                                 parser_current_line(parser), parser_current_column(parser));
        type_node->annotations = type_annotations;
        parser_advance(parser);
    } else if (tok_type == TOK_IDENTIFIER) {
        /* Class/interface type, possibly qualified */
        type_node = ast_new(AST_CLASS_TYPE, parser_current_line(parser), parser_current_column(parser));
        type_node->annotations = type_annotations;
        
        string_t *name = string_new(parser_current_text(parser));
        parser_advance(parser);
        
        /* Handle qualified names (a.b.c) */
        while (parser_check(parser, TOK_DOT)) {
            lexer_pos_t save_pos = lexer_save_pos(parser->lexer);
            parser_advance(parser);
            
            if (parser_check(parser, TOK_IDENTIFIER)) {
                string_append_c(name, '.');
                string_append(name, parser_current_text(parser));
                parser_advance(parser);
            } else {
                /* Not part of type, restore */
                lexer_restore_pos(parser->lexer, save_pos);
                break;
            }
        }
        
        type_node->data.node.name = string_free(name, false);
        
        /* Handle type arguments (<...>) or diamond operator (<>) */
        if (parser_check(parser, TOK_LT)) {
            /* Lookahead to verify this is actually a type argument list, not < operator.
             * Type arguments start with: >, ?, identifier, or primitive type.
             * If we see a literal or operator after <, it's not a type argument. */
            lexer_pos_t save_generic = lexer_save_pos(parser->lexer);
            parser_advance(parser);  /* consume < */
            token_type_t after_lt = parser_current_type(parser);
            bool looks_like_type_args = (after_lt == TOK_GT ||  /* diamond <> */
                                         after_lt == TOK_QUESTION ||  /* wildcard ? */
                                         after_lt == TOK_IDENTIFIER ||
                                         is_primitive_type(after_lt));
            
            if (!looks_like_type_args) {
                /* Not type arguments (e.g., i < 7), restore and return without generics */
                lexer_restore_pos(parser->lexer, save_generic);
                return type_node;
            }
            
            /* Check for diamond operator <> */
            if (after_lt == TOK_GT) {
                parser_advance(parser);
                type_node->data.node.flags |= 0x1000;  /* Diamond operator flag */
            } else {
                do {
                    if (parser_check(parser, TOK_QUESTION)) {
                        /* Wildcard */
                        ast_node_t *wildcard = ast_new(AST_WILDCARD_TYPE,
                                                       parser_current_line(parser),
                                                       parser_current_column(parser));
                        parser_advance(parser);
                        
                        if (parser_check(parser, TOK_EXTENDS)) {
                            parser_advance(parser);
                            ast_node_t *bound = parse_type(parser);
                            if (bound) {
                                wildcard->data.node.name = strdup("extends");
                                ast_add_child(wildcard, bound);
                            }
                        } else if (parser_check(parser, TOK_SUPER)) {
                            parser_advance(parser);
                            ast_node_t *bound = parse_type(parser);
                            if (bound) {
                                wildcard->data.node.name = strdup("super");
                                ast_add_child(wildcard, bound);
                            }
                        }
                        
                        ast_add_child(type_node, wildcard);
                    } else {
                        ast_node_t *type_arg = parse_type(parser);
                        if (type_arg) {
                            ast_add_child(type_node, type_arg);
                        }
                    }
                } while (parser_match(parser, TOK_COMMA));
                
                parser_expect_gt(parser);
            }
        }
    } else {
        /* No valid type found - clean up any collected annotations */
        if (type_annotations) {
            for (slist_t *node = type_annotations; node; node = node->next) {
                ast_free((ast_node_t *)node->data);
            }
            slist_free(type_annotations);
        }
        parser_error(parser, "Expected type");
        return NULL;
    }
    
    /* Handle array dimensions with optional type annotations.
     * Array type annotations can appear before each []: String @NonNull [] @Nullable []
     * The annotation applies to the array dimension, not the element type. */
    while (1) {
        /* Check for annotations before array brackets: @NonNull [] */
        slist_t *array_annotations = parse_type_annotations(parser);
        
        if (parser_check(parser, TOK_LBRACKET)) {
            lexer_pos_t save_pos = lexer_save_pos(parser->lexer);
            parser_advance(parser);
            
            if (parser_check(parser, TOK_RBRACKET)) {
                parser_advance(parser);
                ast_node_t *array_type = ast_new(AST_ARRAY_TYPE,
                                                 type_node->line, type_node->column);
                array_type->annotations = array_annotations;  /* Store annotations on array type */
                ast_add_child(array_type, type_node);
                type_node = array_type;
            } else {
                /* Not an array type marker, restore and clean up */
                lexer_restore_pos(parser->lexer, save_pos);
                if (array_annotations) {
                    for (slist_t *node = array_annotations; node; node = node->next) {
                        ast_free((ast_node_t *)node->data);
                    }
                    slist_free(array_annotations);
                }
                break;
            }
        } else {
            /* No brackets - clean up any annotations we collected */
            if (array_annotations) {
                for (slist_t *node = array_annotations; node; node = node->next) {
                    ast_free((ast_node_t *)node->data);
                }
                slist_free(array_annotations);
            }
            break;
        }
    }
    
    return type_node;
}

/* ========================================================================
 * Statement Parsing (Iterative with state machine)
 * ======================================================================== */

/* Forward declarations */
static ast_node_t *parse_statement(parser_t *parser);
ast_node_t *parse_block(parser_t *parser);
ast_node_t *parse_array_initializer(parser_t *parser);
ast_node_t *parse_class_body(parser_t *parser);
static ast_node_t *parse_type_decl(parser_t *parser, uint32_t modifiers);

/**
 * Parse a block { ... }
 */
ast_node_t *parse_block(parser_t *parser)
{
    if (!parser_expect(parser, TOK_LBRACE)) {
        return NULL;
    }
    
    int line = parser_current_line(parser);
    int col = parser_current_column(parser);
    ast_node_t *block = ast_new(AST_BLOCK, line, col);
    
    while (!parser_check(parser, TOK_RBRACE) && !parser_check(parser, TOK_EOF)) {
        ast_node_t *stmt = parse_statement(parser);
        if (stmt) {
            ast_add_child(block, stmt);
        } else if (parser->error_msg) {
            /* Error occurred, skip to recovery point */
            while (!parser_check(parser, TOK_SEMICOLON) &&
                   !parser_check(parser, TOK_RBRACE) &&
                   !parser_check(parser, TOK_EOF)) {
                parser_advance(parser);
            }
            if (parser_check(parser, TOK_SEMICOLON)) {
                parser_advance(parser);
            }
            free(parser->error_msg);
            parser->error_msg = NULL;
        }
    }
    
    parser_expect(parser, TOK_RBRACE);
    return block;
}

/**
 * Parse a statement.
 */
static ast_node_t *parse_statement(parser_t *parser)
{
    token_type_t tok_type = parser_current_type(parser);
    if (tok_type == TOK_EOF) {
        return NULL;
    }
    
    int line = parser_current_line(parser);
    int col = parser_current_column(parser);
    
    switch (tok_type) {
        case TOK_LBRACE:
            return parse_block(parser);
            
        case TOK_SEMICOLON:
            parser_advance(parser);
            return ast_new(AST_EMPTY_STMT, line, col);
            
        case TOK_IF:
            {
                parser_advance(parser);
                ast_node_t *node = ast_new(AST_IF_STMT, line, col);
                
                if (!parser_expect(parser, TOK_LPAREN)) {
                    ast_free(node);
                    return NULL;
                }
                
                ast_node_t *cond = parse_expression(parser);
                if (cond) {
                    ast_add_child(node, cond);
                }
                
                if (!parser_expect(parser, TOK_RPAREN)) {
                    ast_free(node);
                    return NULL;
                }
                
                ast_node_t *then_stmt = parse_statement(parser);
                if (then_stmt) {
                    ast_add_child(node, then_stmt);
                }
                
                if (parser_match(parser, TOK_ELSE)) {
                    ast_node_t *else_stmt = parse_statement(parser);
                    if (else_stmt) {
                        ast_add_child(node, else_stmt);
                    }
                }
                
                return node;
            }
            
        case TOK_FOR:
            {
                parser_advance(parser);
                
                if (!parser_expect(parser, TOK_LPAREN)) {
                    return NULL;
                }
                
                /* Check for enhanced for loop */
                /* TODO: Better detection of enhanced for */
                /* For now, parse as regular for */
                
                ast_node_t *node = ast_new(AST_FOR_STMT, line, col);
                
                /* Init - can be expression or variable declaration */
                if (!parser_check(parser, TOK_SEMICOLON)) {
                    /* Check if this looks like a variable declaration */
                    /* (starts with type keyword or identifier followed by identifier) */
                    token_type_t first_type = parser_current_type(parser);
                    int first_line = parser_current_line(parser);
                    int first_col = parser_current_column(parser);
                    bool is_var_decl = false;
                    
                    /* Primitive type keywords indicate var declaration */
                    if (first_type == TOK_BYTE || first_type == TOK_SHORT ||
                        first_type == TOK_INT || first_type == TOK_LONG ||
                        first_type == TOK_FLOAT || first_type == TOK_DOUBLE ||
                        first_type == TOK_CHAR || first_type == TOK_BOOLEAN ||
                        first_type == TOK_VAR) {
                        is_var_decl = true;
                    } else if (first_type == TOK_IDENTIFIER) {
                        /* Could be a type - peek ahead */
                        lexer_pos_t save_pos = lexer_save_pos(parser->lexer);
                        parser_advance(parser);
                        /* Skip qualified name parts (e.g., Map.Entry) */
                        while (parser_check(parser, TOK_DOT)) {
                            parser_advance(parser);
                            if (parser_check(parser, TOK_IDENTIFIER)) {
                                parser_advance(parser);
                            } else {
                                break;
                            }
                        }
                        /* Skip generic params if present */
                        if (parser_check(parser, TOK_LT)) {
                            int depth = 1;
                            parser_advance(parser);
                            while (depth > 0 && !parser_check(parser, TOK_EOF)) {
                                if (parser_check(parser, TOK_LT)) depth++;
                                else if (parser_check(parser, TOK_GT)) depth--;
                                parser_advance(parser);
                            }
                        }
                        /* Skip array brackets if present */
                        while (parser_check(parser, TOK_LBRACKET)) {
                            parser_advance(parser);
                            if (!parser_expect(parser, TOK_RBRACKET)) {
                                lexer_restore_pos(parser->lexer, save_pos);
                                break;
                            }
                        }
                        /* If followed by identifier, it's a declaration */
                        if (parser_check(parser, TOK_IDENTIFIER)) {
                            is_var_decl = true;
                        }
                        /* Restore position */
                        lexer_restore_pos(parser->lexer, save_pos);
                    }
                    
                    if (is_var_decl) {
                        /* Parse type */
                        ast_node_t *type = parse_type(parser);
                        
                        /* Check for enhanced for loop: for (Type var : iterable) */
                        if (parser_check(parser, TOK_IDENTIFIER)) {
                            char *var_name = strdup(parser_current_text(parser));
                            int var_line = parser_current_line(parser);
                            int var_col = parser_current_column(parser);
                            parser_advance(parser);
                            
                            if (parser_check(parser, TOK_COLON)) {
                                /* Enhanced for loop */
                                parser_advance(parser);
                                
                                ast_free(node);  /* Don't need FOR_STMT */
                                node = ast_new(AST_ENHANCED_FOR_STMT, line, col);
                                
                                /* Add type */
                                if (type) {
                                    ast_add_child(node, type);
                                }
                                
                                /* Add variable name as identifier */
                                ast_node_t *var_node = ast_new_leaf(AST_IDENTIFIER, var_name, var_line, var_col);
                                ast_add_child(node, var_node);
                                
                                /* Parse iterable expression */
                                ast_node_t *iterable = parse_expression(parser);
                                if (iterable) {
                                    ast_add_child(node, iterable);
                                }
                                
                                free(var_name);
                                
                                if (!parser_expect(parser, TOK_RPAREN)) {
                                    ast_free(node);
                                    return NULL;
                                }
                                
                                /* Parse body */
                                ast_node_t *body = parse_statement(parser);
                                if (body) {
                                    ast_add_child(node, body);
                                }
                                
                                return node;
                            }
                            
                            /* Regular for loop - build var declaration */
                            ast_node_t *var_decl = ast_new(AST_VAR_DECL, first_line, first_col);
                            if (type) {
                                ast_add_child(var_decl, type);
                            }
                            
                            ast_node_t *declarator = ast_new(AST_VAR_DECLARATOR, var_line, var_col);
                            declarator->data.node.name = var_name;
                            
                            /* Initializer? */
                            if (parser_check(parser, TOK_ASSIGN)) {
                                parser_advance(parser);
                                ast_node_t *init_expr = parse_expression(parser);
                                if (init_expr) {
                                    ast_add_child(declarator, init_expr);
                                }
                            }
                            
                            ast_add_child(var_decl, declarator);
                            
                            /* More declarators? */
                            while (parser_match(parser, TOK_COMMA)) {
                                if (parser_check(parser, TOK_IDENTIFIER)) {
                                    ast_node_t *decl = ast_new(AST_VAR_DECLARATOR, 
                                        parser_current_line(parser), parser_current_column(parser));
                                    decl->data.node.name = strdup(parser_current_text(parser));
                                    parser_advance(parser);
                                    
                                    if (parser_check(parser, TOK_ASSIGN)) {
                                        parser_advance(parser);
                                        ast_node_t *init = parse_expression(parser);
                                        if (init) {
                                            ast_add_child(decl, init);
                                        }
                                    }
                                    
                                    ast_add_child(var_decl, decl);
                                }
                            }
                            
                            ast_add_child(node, var_decl);
                        } else {
                            /* Type parsed but no identifier - error? */
                            ast_free(type);
                            ast_node_t *init = parse_expression(parser);
                            if (init) {
                                ast_add_child(node, init);
                            }
                        }
                    } else {
                        ast_node_t *init = parse_expression(parser);
                        if (init) {
                            ast_add_child(node, init);
                        }
                    }
                }
                if (!parser_expect(parser, TOK_SEMICOLON)) {
                    ast_free(node);
                    return NULL;
                }
                
                /* Condition */
                if (!parser_check(parser, TOK_SEMICOLON)) {
                    ast_node_t *cond = parse_expression(parser);
                    if (cond) {
                        ast_add_child(node, cond);
                    }
                }
                if (!parser_expect(parser, TOK_SEMICOLON)) {
                    ast_free(node);
                    return NULL;
                }
                
                /* Update */
                if (!parser_check(parser, TOK_RPAREN)) {
                    ast_node_t *update = parse_expression(parser);
                    if (update) {
                        ast_add_child(node, update);
                    }
                }
                if (!parser_expect(parser, TOK_RPAREN)) {
                    ast_free(node);
                    return NULL;
                }
                
                /* Body */
                ast_node_t *body = parse_statement(parser);
                if (body) {
                    ast_add_child(node, body);
                }
                
                return node;
            }
            
        case TOK_WHILE:
            {
                parser_advance(parser);
                ast_node_t *node = ast_new(AST_WHILE_STMT, line, col);
                
                if (!parser_expect(parser, TOK_LPAREN)) {
                    ast_free(node);
                    return NULL;
                }
                
                ast_node_t *cond = parse_expression(parser);
                if (cond) {
                    ast_add_child(node, cond);
                }
                
                if (!parser_expect(parser, TOK_RPAREN)) {
                    ast_free(node);
                    return NULL;
                }
                
                ast_node_t *body = parse_statement(parser);
                if (body) {
                    ast_add_child(node, body);
                }
                
                return node;
            }
            
        case TOK_DO:
            {
                parser_advance(parser);
                ast_node_t *node = ast_new(AST_DO_STMT, line, col);
                
                ast_node_t *body = parse_statement(parser);
                if (body) {
                    ast_add_child(node, body);
                }
                
                if (!parser_expect(parser, TOK_WHILE)) {
                    ast_free(node);
                    return NULL;
                }
                if (!parser_expect(parser, TOK_LPAREN)) {
                    ast_free(node);
                    return NULL;
                }
                
                ast_node_t *cond = parse_expression(parser);
                if (cond) {
                    ast_add_child(node, cond);
                }
                
                if (!parser_expect(parser, TOK_RPAREN)) {
                    ast_free(node);
                    return NULL;
                }
                if (!parser_expect(parser, TOK_SEMICOLON)) {
                    ast_free(node);
                    return NULL;
                }
                
                return node;
            }
            
        case TOK_SWITCH:
            {
                parser_advance(parser);
                ast_node_t *node = ast_new(AST_SWITCH_STMT, line, col);
                
                if (!parser_expect(parser, TOK_LPAREN)) {
                    ast_free(node);
                    return NULL;
                }
                
                ast_node_t *expr = parse_expression(parser);
                if (expr) {
                    ast_add_child(node, expr);
                }
                
                if (!parser_expect(parser, TOK_RPAREN)) {
                    ast_free(node);
                    return NULL;
                }
                if (!parser_expect(parser, TOK_LBRACE)) {
                    ast_free(node);
                    return NULL;
                }
                
                /* Parse cases */
                while (!parser_check(parser, TOK_RBRACE) && !parser_check(parser, TOK_EOF)) {
                    if (parser_check(parser, TOK_CASE) || parser_check(parser, TOK_DEFAULT)) {
                        ast_node_t *case_label = ast_new(AST_CASE_LABEL,
                                                         parser_current_line(parser),
                                                         parser_current_column(parser));
                        
                        if (parser_match(parser, TOK_DEFAULT)) {
                            case_label->data.node.name = strdup("default");
                        } else {
                            parser_advance(parser);  /* consume 'case' */
                            ast_node_t *case_expr = parse_expression(parser);
                            if (case_expr) {
                                ast_add_child(case_label, case_expr);
                            }
                        }
                        
                        if (!parser_expect(parser, TOK_COLON)) {
                            ast_free(case_label);
                            ast_free(node);
                            return NULL;
                        }
                        
                        /* Parse statements until next case or end */
                        while (!parser_check(parser, TOK_CASE) &&
                               !parser_check(parser, TOK_DEFAULT) &&
                               !parser_check(parser, TOK_RBRACE) &&
                               !parser_check(parser, TOK_EOF)) {
                            ast_node_t *stmt = parse_statement(parser);
                            if (stmt) {
                                ast_add_child(case_label, stmt);
                            }
                        }
                        
                        ast_add_child(node, case_label);
                    } else {
                        parser_error(parser, "Expected 'case' or 'default'");
                        ast_free(node);
                        return NULL;
                    }
                }
                
                parser_expect(parser, TOK_RBRACE);
                return node;
            }
            
        case TOK_RETURN:
            {
                parser_advance(parser);
                ast_node_t *node = ast_new(AST_RETURN_STMT, line, col);
                
                if (!parser_check(parser, TOK_SEMICOLON)) {
                    ast_node_t *expr = parse_expression(parser);
                    if (expr) {
                        ast_add_child(node, expr);
                    }
                }
                
                parser_expect(parser, TOK_SEMICOLON);
                return node;
            }
            
        case TOK_YIELD:
            {
                /* yield statement (Java 12+) - for switch expressions */
                parser_advance(parser);
                ast_node_t *node = ast_new(AST_YIELD_STMT, line, col);
                
                ast_node_t *expr = parse_expression(parser);
                if (expr) {
                    ast_add_child(node, expr);
                }
                
                parser_expect(parser, TOK_SEMICOLON);
                return node;
            }
            
        case TOK_THROW:
            {
                parser_advance(parser);
                ast_node_t *node = ast_new(AST_THROW_STMT, line, col);
                
                ast_node_t *expr = parse_expression(parser);
                if (expr) {
                    ast_add_child(node, expr);
                }
                
                parser_expect(parser, TOK_SEMICOLON);
                return node;
            }
            
        case TOK_BREAK:
            {
                parser_advance(parser);
                ast_node_t *node = ast_new(AST_BREAK_STMT, line, col);
                
                if (parser_check(parser, TOK_IDENTIFIER)) {
                    node->data.node.name = strdup(parser_current_text(parser));
                    parser_advance(parser);
                }
                
                parser_expect(parser, TOK_SEMICOLON);
                return node;
            }
            
        case TOK_CONTINUE:
            {
                parser_advance(parser);
                ast_node_t *node = ast_new(AST_CONTINUE_STMT, line, col);
                
                if (parser_check(parser, TOK_IDENTIFIER)) {
                    node->data.node.name = strdup(parser_current_text(parser));
                    parser_advance(parser);
                }
                
                parser_expect(parser, TOK_SEMICOLON);
                return node;
            }
            
        case TOK_TRY:
            {
                parser_advance(parser);
                ast_node_t *node = ast_new(AST_TRY_STMT, line, col);
                
                /* Check for try-with-resources: try (resources) { ... } */
                if (parser_check(parser, TOK_LPAREN)) {
                    parser_advance(parser);  /* consume '(' */
                    
                    /* Parse resource specifications
                     * Two forms:
                     * 1. Declaration: try (Type var = expr) { }
                     * 2. Reference (Java 9+): try (existingVar) { } or try (obj.field) { }
                     *
                     * flags bit 0 (value 1): final modifier
                     * flags bit 1 (value 2): existing variable reference (no declaration)
                     */
                    while (!parser_check(parser, TOK_RPAREN) && !parser_check(parser, TOK_EOF)) {
                        int res_line = parser_current_line(parser);
                        int res_col = parser_current_column(parser);
                        ast_node_t *resource = ast_new(AST_RESOURCE_SPEC, res_line, res_col);
                        
                        /* Check for 'final' modifier - only valid for declarations */
                        bool is_final = false;
                        if (parser_match(parser, TOK_FINAL)) {
                            is_final = true;
                        }
                        resource->data.node.flags = is_final ? 1 : 0;
                        
                        /* Determine if this is a declaration or existing variable reference.
                         * Declaration: Type varName = expr
                         * Reference: existingVar or obj.field
                         *
                         * If we see 'final', it must be a declaration.
                         * Otherwise, parse what could be a type or expression.
                         * If followed by an identifier and '=', it's a declaration.
                         * If followed by ';' or ')', it's a reference.
                         */
                        
                        if (is_final) {
                            /* Must be a declaration */
                            ast_node_t *type_node = parse_type(parser);
                            if (type_node) {
                                ast_add_child(resource, type_node);
                            }
                            
                            if (parser_check(parser, TOK_IDENTIFIER)) {
                                resource->data.node.name = strdup(parser_current_text(parser));
                                parser_advance(parser);
                            }
                            
                            if (!parser_expect(parser, TOK_ASSIGN)) {
                                ast_free(resource);
                                ast_free(node);
                                return NULL;
                            }
                            
                            ast_node_t *init_expr = parse_expression(parser);
                            if (init_expr) {
                                ast_add_child(resource, init_expr);
                            }
                        } else {
                            /* Could be declaration or reference.
                             * Parse as expression first (handles identifiers, field access, etc.)
                             * Then check if we should interpret it differently.
                             */
                            ast_node_t *first = parse_type(parser);
                            
                            if (parser_check(parser, TOK_IDENTIFIER)) {
                                /* Type followed by identifier: this is a declaration */
                                if (first) {
                                    ast_add_child(resource, first);  /* Type node */
                                }
                                
                                resource->data.node.name = strdup(parser_current_text(parser));
                                parser_advance(parser);
                                
                                if (!parser_expect(parser, TOK_ASSIGN)) {
                                    ast_free(resource);
                                    ast_free(node);
                                    return NULL;
                                }
                                
                                ast_node_t *init_expr = parse_expression(parser);
                                if (init_expr) {
                                    ast_add_child(resource, init_expr);
                                }
                            } else if (parser_check(parser, TOK_SEMICOLON) || 
                                       parser_check(parser, TOK_RPAREN)) {
                                /* Just an identifier (or expression): existing variable reference
                                 * Convert type_node to an identifier expression if needed
                                 */
                                resource->data.node.flags |= 2;  /* Mark as existing var reference */
                                
                                /* The 'first' node from parse_type is actually just an identifier.
                                 * We need it as an expression child for codegen to load.
                                 */
                                if (first && first->type == AST_CLASS_TYPE && first->data.node.name) {
                                    /* Convert to identifier expression */
                                    ast_node_t *ident = ast_new(AST_IDENTIFIER, res_line, res_col);
                                    ident->data.leaf.name = strdup(first->data.node.name);
                                    ast_free(first);
                                    ast_add_child(resource, ident);
                                } else if (first) {
                                    /* For qualified names like obj.field, use as is */
                                    ast_add_child(resource, first);
                                }
                            } else {
                                /* Unexpected token after type/expression */
                                parser_error(parser, "Expected identifier, ';', or ')' in resource specification");
                                ast_free(first);
                                ast_free(resource);
                                ast_free(node);
                                return NULL;
                            }
                        }
                        
                        ast_add_child(node, resource);
                        
                        /* Resources separated by ';' (optional trailing semicolon) */
                        if (!parser_match(parser, TOK_SEMICOLON)) {
                            break;
                        }
                    }
                    
                    if (!parser_expect(parser, TOK_RPAREN)) {
                        ast_free(node);
                        return NULL;
                    }
                }
                
                /* Try block */
                ast_node_t *try_block = parse_block(parser);
                if (try_block) {
                    ast_add_child(node, try_block);
                }
                
                /* Catch clauses */
                while (parser_check(parser, TOK_CATCH)) {
                    int catch_line = parser_current_line(parser);
                    int catch_col = parser_current_column(parser);
                    parser_advance(parser);
                    
                    ast_node_t *catch_clause = ast_new(AST_CATCH_CLAUSE, catch_line, catch_col);
                    
                    if (!parser_expect(parser, TOK_LPAREN)) {
                        ast_free(catch_clause);
                        ast_free(node);
                        return NULL;
                    }
                    
                    /* Exception parameter - may have multiple types (multi-catch) */
                    ast_node_t *type_node = parse_type(parser);
                    if (type_node) {
                        ast_add_child(catch_clause, type_node);
                    }
                    
                    /* Multi-catch: check for additional types separated by | */
                    while (parser_check(parser, TOK_BITOR)) {
                        parser_advance(parser);  /* consume '|' */
                        ast_node_t *alt_type = parse_type(parser);
                        if (alt_type) {
                            ast_add_child(catch_clause, alt_type);
                        }
                    }
                    
                    if (parser_check(parser, TOK_IDENTIFIER)) {
                        catch_clause->data.node.name = strdup(parser_current_text(parser));
                        parser_advance(parser);
                    }
                    
                    if (!parser_expect(parser, TOK_RPAREN)) {
                        ast_free(catch_clause);
                        ast_free(node);
                        return NULL;
                    }
                    
                    ast_node_t *catch_block = parse_block(parser);
                    if (catch_block) {
                        ast_add_child(catch_clause, catch_block);
                    }
                    
                    ast_add_child(node, catch_clause);
                }
                
                /* Finally clause */
                if (parser_match(parser, TOK_FINALLY)) {
                    ast_node_t *finally_clause = ast_new(AST_FINALLY_CLAUSE, 
                                                         parser_current_line(parser),
                                                         parser_current_column(parser));
                    ast_node_t *finally_block = parse_block(parser);
                    if (finally_block) {
                        ast_add_child(finally_clause, finally_block);
                    }
                    ast_add_child(node, finally_clause);
                }
                
                return node;
            }
            
        case TOK_SYNCHRONIZED:
            {
                parser_advance(parser);
                ast_node_t *node = ast_new(AST_SYNCHRONIZED_STMT, line, col);
                
                if (!parser_expect(parser, TOK_LPAREN)) {
                    ast_free(node);
                    return NULL;
                }
                
                ast_node_t *expr = parse_expression(parser);
                if (expr) {
                    ast_add_child(node, expr);
                }
                
                if (!parser_expect(parser, TOK_RPAREN)) {
                    ast_free(node);
                    return NULL;
                }
                
                ast_node_t *body = parse_block(parser);
                if (body) {
                    ast_add_child(node, body);
                }
                
                return node;
            }
            
        case TOK_ASSERT:
            {
                parser_advance(parser);
                ast_node_t *node = ast_new(AST_ASSERT_STMT, line, col);
                
                ast_node_t *expr = parse_expression(parser);
                if (expr) {
                    ast_add_child(node, expr);
                }
                
                if (parser_match(parser, TOK_COLON)) {
                    ast_node_t *msg = parse_expression(parser);
                    if (msg) {
                        ast_add_child(node, msg);
                    }
                }
                
                parser_expect(parser, TOK_SEMICOLON);
                return node;
            }
        
        case TOK_CLASS:
        case TOK_INTERFACE:
            {
                /* Local class/interface declaration inside a method */
                uint32_t modifiers = 0;
                /* Note: local classes can be 'final' or 'abstract' but not public/private/etc */
                return parse_type_decl(parser, modifiers);
            }
            
        default:
            {
                /* Could be:
                 * - Variable declaration (type name = ...)
                 * - Expression statement (expr;)
                 * - Labeled statement (label: stmt)
                 * - Local class with modifiers (final class X, abstract class X)
                 */
                
                /* Check for local class with modifiers */
                if (tok_type == TOK_FINAL || tok_type == TOK_ABSTRACT) {
                    lexer_pos_t save_pos = lexer_save_pos(parser->lexer);
                    
                    uint32_t mods = 0;
                    while (parser_check(parser, TOK_FINAL) || parser_check(parser, TOK_ABSTRACT)) {
                        if (parser_match(parser, TOK_FINAL)) mods |= MOD_FINAL;
                        if (parser_match(parser, TOK_ABSTRACT)) mods |= MOD_ABSTRACT;
                    }
                    
                    if (parser_check(parser, TOK_CLASS) || parser_check(parser, TOK_INTERFACE) || parser_check(parser, TOK_RECORD)) {
                        return parse_type_decl(parser, mods);
                    }
                    
                    /* Not a class declaration - restore */
                    lexer_restore_pos(parser->lexer, save_pos);
                }
                
                /* Check for labeled statement */
                if (tok_type == TOK_IDENTIFIER) {
                    lexer_pos_t save_pos = lexer_save_pos(parser->lexer);
                    const char *label_name = strdup(parser_current_text(parser));
                    parser_advance(parser);
                    
                    if (parser_check(parser, TOK_COLON)) {
                        parser_advance(parser);
                        ast_node_t *node = ast_new(AST_LABELED_STMT, line, col);
                        node->data.node.name = (char *)label_name;
                        
                        ast_node_t *stmt = parse_statement(parser);
                        if (stmt) {
                            ast_add_child(node, stmt);
                        }
                        
                        return node;
                    }
                    
                    /* Restore position */
                    free((void *)label_name);
                    lexer_restore_pos(parser->lexer, save_pos);
                }
                
                /* Check for local variable declaration.
                 * This includes:
                 * - Type name = value (e.g., String s = "hello")
                 * - final Type name = value
                 * - @Annotation Type name = value (type annotations) */
                if (is_type_start(tok_type) || tok_type == TOK_FINAL || tok_type == TOK_AT) {
                    /* Could be variable declaration or expression */
                    /* This is tricky - need lookahead */
                    /* For simplicity, try to detect common patterns */
                    
                    uint32_t modifiers = 0;
                    while (parser_check(parser, TOK_FINAL)) {
                        modifiers |= MOD_FINAL;
                        parser_advance(parser);
                    }
                    
                    /* After modifiers, we can have type annotations or type name */
                    if (is_type_start(parser_current_type(parser)) || 
                        parser_check(parser, TOK_AT)) {
                        lexer_pos_t save_pos = lexer_save_pos(parser->lexer);
                        
                        /* Try parsing as type */
                        ast_node_t *type_node = parse_type(parser);
                        
                        /* If followed by identifier, it's a variable declaration */
                        if (type_node && parser_check(parser, TOK_IDENTIFIER)) {
                            ast_node_t *var_decl = ast_new(AST_VAR_DECL, line, col);
                            var_decl->data.node.flags = modifiers;
                            ast_add_child(var_decl, type_node);
                            
                            /* Parse declarators */
                            do {
                                if (!parser_check(parser, TOK_IDENTIFIER)) {
                                    parser_error(parser, "Expected variable name");
                                    ast_free(var_decl);
                                    return NULL;
                                }
                                
                                ast_node_t *declarator = ast_new(AST_VAR_DECLARATOR,
                                                                 parser_current_line(parser),
                                                                 parser_current_column(parser));
                                declarator->data.node.name = strdup(parser_current_text(parser));
                                parser_advance(parser);
                                
                                /* Array dimensions on variable */
                                while (parser_match(parser, TOK_LBRACKET)) {
                                    parser_expect(parser, TOK_RBRACKET);
                                    declarator->data.node.flags++;  /* Count dimensions */
                                }
                                
                                /* Initializer */
                                if (parser_match(parser, TOK_ASSIGN)) {
                                    ast_node_t *init;
                                    if (parser_check(parser, TOK_LBRACE)) {
                                        init = parse_array_initializer(parser);
                                    } else {
                                        init = parse_expression(parser);
                                    }
                                    if (init) {
                                        ast_add_child(declarator, init);
                                    }
                                }
                                
                                ast_add_child(var_decl, declarator);
                            } while (parser_match(parser, TOK_COMMA));
                            
                            parser_expect(parser, TOK_SEMICOLON);
                            return var_decl;
                        }
                        
                        /* Not a variable declaration, restore and parse as expression */
                        ast_free(type_node);
                        lexer_restore_pos(parser->lexer, save_pos);
                    }
                }
                
                /* Expression statement */
                ast_node_t *expr = parse_expression(parser);
                if (!expr) {
                    return NULL;
                }
                
                parser_expect(parser, TOK_SEMICOLON);
                
                ast_node_t *node = ast_new(AST_EXPR_STMT, line, col);
                ast_add_child(node, expr);
                return node;
            }
    }
}

/**
 * Parse array initializer { ... }
 */
ast_node_t *parse_array_initializer(parser_t *parser)
{
    if (!parser_expect(parser, TOK_LBRACE)) {
        return NULL;
    }
    
    int line = true ? parser_current_line(parser) : 0;
    int col = true ? parser_current_column(parser) : 0;
    ast_node_t *init = ast_new(AST_ARRAY_INIT, line, col);
    
    /* Parse elements, handling trailing comma and empty initializer {,} */
    while (!parser_check(parser, TOK_RBRACE)) {
        /* Skip if we're at a comma (handles {,} and trailing commas) */
        if (parser_check(parser, TOK_COMMA)) {
            parser_advance(parser);  /* consume comma */
            continue;
        }
        
        ast_node_t *elem;
        if (parser_check(parser, TOK_LBRACE)) {
            elem = parse_array_initializer(parser);
        } else {
            elem = parse_expression(parser);
        }
        if (elem) {
            ast_add_child(init, elem);
        }
        
        /* Skip comma if present */
        parser_match(parser, TOK_COMMA);
    }
    
    parser_expect(parser, TOK_RBRACE);
    return init;
}

/* ========================================================================
 * Declaration Parsing
 * ======================================================================== */

/**
 * Parse a single annotation: @Name or @Name(value) or @Name(key=value, ...)
 */
static ast_node_t *parse_annotation(parser_t *parser)
{
    if (!parser_check(parser, TOK_AT)) {
        return NULL;
    }
    
    int line = parser_current_line(parser);
    int col = parser_current_column(parser);
    parser_advance(parser);  /* consume @ */
    
    if (!parser_check(parser, TOK_IDENTIFIER)) {
        parser_error(parser, "Expected annotation name after @");
        return NULL;
    }
    
    /* Build qualified annotation name (e.g., java.lang.Override) */
    string_t *name = string_new(parser_current_text(parser));
    parser_advance(parser);
    
    while (parser_match(parser, TOK_DOT)) {
        if (!parser_check(parser, TOK_IDENTIFIER)) {
            parser_error(parser, "Expected identifier after '.'");
            string_free(name, true);
            return NULL;
        }
        string_append(name, ".");
        string_append(name, parser_current_text(parser));
        parser_advance(parser);
    }
    
    ast_node_t *annot = ast_new(AST_ANNOTATION, line, col);
    annot->data.node.name = strdup(name->str);
    string_free(name, true);
    
    /* Parse annotation arguments if present */
    if (parser_match(parser, TOK_LPAREN)) {
        if (!parser_check(parser, TOK_RPAREN)) {
            do {
                /* Check for name=value pair or just value */
                if (parser_check(parser, TOK_IDENTIFIER)) {
                    const char *first_text = strdup(parser_current_text(parser));
                    int first_line = parser_current_line(parser);
                    int first_col = parser_current_column(parser);
                    parser_advance(parser);
                    
                    if (parser_match(parser, TOK_ASSIGN)) {
                        /* name=value pair */
                        ast_node_t *pair = ast_new(AST_ANNOTATION_VALUE, first_line, first_col);
                        pair->data.node.name = (char *)first_text;
                        
                        /* Parse value (string, number, annotation, enum, array) */
                        if (parser_check(parser, TOK_STRING_LITERAL)) {
                            ast_node_t *val = ast_new_literal_from_lexer(parser->lexer);
                            ast_add_child(pair, val);
                            parser_advance(parser);
                        } else if (parser_check(parser, TOK_INTEGER_LITERAL) ||
                                   parser_check(parser, TOK_TRUE) ||
                                   parser_check(parser, TOK_FALSE)) {
                            ast_node_t *val = ast_new_literal_from_lexer(parser->lexer);
                            ast_add_child(pair, val);
                            parser_advance(parser);
                        } else if (parser_check(parser, TOK_AT)) {
                            ast_node_t *nested = parse_annotation(parser);
                            if (nested) ast_add_child(pair, nested);
                        } else if (parser_check(parser, TOK_LBRACE)) {
                            /* Array value - skip for now */
                            int depth = 1;
                            parser_advance(parser);
                            while (depth > 0 && !parser_check(parser, TOK_EOF)) {
                                if (parser_check(parser, TOK_LBRACE)) depth++;
                                else if (parser_check(parser, TOK_RBRACE)) depth--;
                                parser_advance(parser);
                            }
                        } else {
                            /* Could be enum constant or class literal */
                            ast_node_t *val = parse_expression(parser);
                            if (val) ast_add_child(pair, val);
                        }
                        
                        ast_add_child(annot, pair);
                    } else {
                        /* Single value - first token was the value, need to handle as identifier */
                        /* Actually this is tricky - we already consumed it. For now, create identifier */
                        ast_node_t *pair = ast_new(AST_ANNOTATION_VALUE, first_line, first_col);
                        pair->data.node.name = strdup("value");  /* Default element name */
                        ast_node_t *val = ast_new_leaf(AST_IDENTIFIER, first_text, first_line, first_col);
                        free((void *)first_text);
                        ast_add_child(pair, val);
                        ast_add_child(annot, pair);
                    }
                } else if (parser_check(parser, TOK_STRING_LITERAL)) {
                    /* Single value annotation like @SuppressWarnings("unchecked") */
                    ast_node_t *pair = ast_new(AST_ANNOTATION_VALUE, parser_current_line(parser), parser_current_column(parser));
                    pair->data.node.name = strdup("value");
                    ast_node_t *val = ast_new_literal_from_lexer(parser->lexer);
                    ast_add_child(pair, val);
                    parser_advance(parser);
                    ast_add_child(annot, pair);
                } else {
                    /* Skip unknown content */
                    parser_advance(parser);
                }
            } while (parser_match(parser, TOK_COMMA));
        }
        
        parser_expect(parser, TOK_RPAREN);
    }
    
    return annot;
}

/**
 * Parse modifiers and annotations.
 * Returns modifiers as return value, stores annotations in the provided list.
 */
static uint32_t parse_modifiers_with_annotations(parser_t *parser, slist_t **annotations)
{
    uint32_t flags = 0;
    *annotations = NULL;
    
    while (1) {
        if (parser_check(parser, TOK_AT)) {
            /* Check if this is @interface (annotation declaration), not an annotation */
            if (parser_peek_type(parser) == TOK_INTERFACE) {
                /* This is @interface, not an annotation - stop parsing modifiers */
                break;
            }
            ast_node_t *annot = parse_annotation(parser);
            if (annot) {
                if (!*annotations) {
                    *annotations = slist_new(annot);
                } else {
                    slist_append(*annotations, annot);
                }
            }
            continue;
        }
        
        if (is_modifier(parser_current_type(parser))) {
            uint32_t new_flag = modifier_flag(parser_current_type(parser));
            if (flags & new_flag) {
                parser_error(parser, "repeated modifier");
            }
            flags |= new_flag;
            parser_advance(parser);
            continue;
        }
        
        break;
    }
    
    return flags;
}

/**
 * Parse modifiers and annotations (legacy wrapper - skips annotations).
 */
static uint32_t parse_modifiers(parser_t *parser)
{
    slist_t *annotations = NULL;
    uint32_t flags = parse_modifiers_with_annotations(parser, &annotations);
    /* Free annotations list (don't need them in this legacy path) */
    if (annotations) {
        for (slist_t *node = annotations; node; node = node->next) {
            ast_free((ast_node_t *)node->data);
        }
        slist_free(annotations);
    }
    return flags;
}

/**
 * Parse a method or constructor declaration.
 */
static ast_node_t *parse_method_decl(parser_t *parser, uint32_t modifiers,
                                      ast_node_t *return_type, const char *name,
                                      int line, int col)
{
    ast_node_type_t type = return_type ? AST_METHOD_DECL : AST_CONSTRUCTOR_DECL;
    ast_node_t *method = ast_new(type, line, col);
    method->data.node.flags = modifiers;
    method->data.node.name = strdup(name);
    method->data.node.extra = return_type;
    
    /* Parse parameters */
    parser_expect(parser, TOK_LPAREN);
    
    if (!parser_check(parser, TOK_RPAREN)) {
        do {
            /* Parse modifiers and annotations for the parameter */
            slist_t *param_annotations = NULL;
            uint32_t param_mods = parse_modifiers_with_annotations(parser, &param_annotations);
            ast_node_t *param_type = parse_type(parser);
            
            /* Varargs */
            bool varargs = parser_match(parser, TOK_ELLIPSIS);
            
            if (!parser_check(parser, TOK_IDENTIFIER)) {
                parser_error(parser, "Expected parameter name");
                ast_free(method);
                return NULL;
            }
            
            ast_node_t *param = ast_new(AST_PARAMETER, parser_current_line(parser),
                                        parser_current_column(parser));
            param->data.node.name = strdup(parser_current_text(parser));
            param->data.node.flags = param_mods | (varargs ? MOD_VARARGS : 0);
            param->annotations = param_annotations;  /* Store parameter annotations */
            parser_advance(parser);
            
            /* Handle C-style array brackets after parameter name (e.g., String args[]) */
            while (parser_check(parser, TOK_LBRACKET)) {
                int arr_line = parser_current_line(parser);
                int arr_col = parser_current_column(parser);
                parser_advance(parser);
                if (!parser_expect(parser, TOK_RBRACKET)) {
                    ast_free(method);
                    return NULL;
                }
                /* Wrap the type in an array type */
                ast_node_t *array_type = ast_new(AST_ARRAY_TYPE, arr_line, arr_col);
                ast_add_child(array_type, param_type);
                param_type = array_type;
            }
            
            if (param_type) {
                ast_add_child(param, param_type);
            }
            
            ast_add_child(method, param);
        } while (parser_match(parser, TOK_COMMA));
    }
    
    parser_expect(parser, TOK_RPAREN);
    
    /* Throws clause */
    if (parser_match(parser, TOK_THROWS)) {
        do {
            ast_node_t *exc_type = parse_type(parser);
            if (exc_type) {
                /* Store in a special way - add as child with marker */
                exc_type->data.node.flags = 0xFFFF;  /* Mark as throws */
                ast_add_child(method, exc_type);
            }
        } while (parser_match(parser, TOK_COMMA));
    }
    
    /* Method body, default value (for annotation elements), or semicolon */
    if (parser_check(parser, TOK_LBRACE)) {
        ast_node_t *body = parse_block(parser);
        if (body) {
            ast_add_child(method, body);
        }
    } else if (parser_match(parser, TOK_DEFAULT)) {
        /* Annotation element default value: Type name() default value; */
        ast_node_t *default_val;
        if (parser_check(parser, TOK_LBRACE)) {
            /* Array value */
            default_val = parse_array_initializer(parser);
        } else if (parser_check(parser, TOK_AT)) {
            /* Nested annotation */
            default_val = parse_annotation(parser);
        } else {
            /* Expression (string, number, enum constant, class literal) */
            default_val = parse_expression(parser);
        }
        method->annotation_default = default_val;
        parser_expect(parser, TOK_SEMICOLON);
    } else {
        parser_expect(parser, TOK_SEMICOLON);
    }
    
    return method;
}

/**
 * Parse a field declaration.
 */
static ast_node_t *parse_field_decl(parser_t *parser, uint32_t modifiers,
                                     ast_node_t *type_node, const char *name,
                                     int line, int col)
{
    ast_node_t *field = ast_new(AST_FIELD_DECL, line, col);
    field->data.node.flags = modifiers;
    if (type_node) {
        ast_add_child(field, type_node);
    }
    
    /* Parse declarators (starting with the first one we already have) */
    ast_node_t *declarator = ast_new(AST_VAR_DECLARATOR, line, col);
    declarator->data.node.name = strdup(name);
    
    /* Array dimensions on variable */
    while (parser_match(parser, TOK_LBRACKET)) {
        parser_expect(parser, TOK_RBRACKET);
        declarator->data.node.flags++;
    }
    
    /* Initializer */
    if (parser_match(parser, TOK_ASSIGN)) {
        ast_node_t *init;
        if (parser_check(parser, TOK_LBRACE)) {
            init = parse_array_initializer(parser);
        } else {
            init = parse_expression(parser);
        }
        if (init) {
            ast_add_child(declarator, init);
        }
    }
    
    ast_add_child(field, declarator);
    
    /* Additional declarators */
    while (parser_match(parser, TOK_COMMA)) {
        if (!parser_check(parser, TOK_IDENTIFIER)) {
            parser_error(parser, "Expected field name");
            ast_free(field);
            return NULL;
        }
        
        declarator = ast_new(AST_VAR_DECLARATOR, parser_current_line(parser),
                             parser_current_column(parser));
        declarator->data.node.name = strdup(parser_current_text(parser));
        parser_advance(parser);
        
        while (parser_match(parser, TOK_LBRACKET)) {
            parser_expect(parser, TOK_RBRACKET);
            declarator->data.node.flags++;
        }
        
        if (parser_match(parser, TOK_ASSIGN)) {
            ast_node_t *init;
            if (parser_check(parser, TOK_LBRACE)) {
                init = parse_array_initializer(parser);
            } else {
                init = parse_expression(parser);
            }
            if (init) {
                ast_add_child(declarator, init);
            }
        }
        
        ast_add_child(field, declarator);
    }
    
    parser_expect(parser, TOK_SEMICOLON);
    return field;
}

/* Forward declarations for nested type parsing */
static ast_node_t *parse_type_decl(parser_t *parser, uint32_t modifiers);
static ast_node_t *parse_type_decl_with_annotations(parser_t *parser, uint32_t modifiers, slist_t *annotations);

/**
 * Parse a class body member.
 */
static ast_node_t *parse_class_member(parser_t *parser, const char *class_name)
{
    slist_t *annotations = NULL;
    uint32_t modifiers = parse_modifiers_with_annotations(parser, &annotations);
    
    /* Nested class/interface/enum/record declaration */
    if (parser_check(parser, TOK_CLASS) ||
        parser_check(parser, TOK_INTERFACE) ||
        parser_check(parser, TOK_ENUM) ||
        parser_check(parser, TOK_RECORD) ||
        parser_check(parser, TOK_AT)) {  /* @interface */
        return parse_type_decl_with_annotations(parser, modifiers, annotations);
    }
    
    /* Static initializer */
    if (parser_check(parser, TOK_LBRACE)) {
        ast_node_t *init = ast_new(AST_INITIALIZER_BLOCK, parser_current_line(parser),
                                   parser_current_column(parser));
        init->data.node.flags = modifiers;
        init->annotations = annotations;
        ast_node_t *block = parse_block(parser);
        if (block) {
            ast_add_child(init, block);
        }
        return init;
    }
    
    /* Constructor (name matches class name) */
    if (parser_check(parser, TOK_IDENTIFIER) && class_name &&
        strcmp(parser_current_text(parser), class_name) == 0) {
        /* Check if followed by ( - then it's a constructor */
        lexer_pos_t save_pos = lexer_save_pos(parser->lexer);
        int ctor_line = parser_current_line(parser);
        int ctor_col = parser_current_column(parser);
        parser_advance(parser);
        
        if (parser_check(parser, TOK_LPAREN)) {
            ast_node_t *ctor = parse_method_decl(parser, modifiers, NULL, class_name,
                                     ctor_line, ctor_col);
            if (ctor) ctor->annotations = annotations;
            return ctor;
        }
        
        /* Restore - it's something else */
        lexer_restore_pos(parser->lexer, save_pos);
    }
    
    /* Generic method type parameters (e.g., public <T> T foo(...)) */
    slist_t *method_type_params = NULL;
    if (parser_check(parser, TOK_LT)) {
        parser_advance(parser);
        do {
            if (parser_check(parser, TOK_IDENTIFIER)) {
                ast_node_t *type_param = ast_new(AST_TYPE_PARAMETER,
                                                  parser_current_line(parser),
                                                  parser_current_column(parser));
                type_param->data.node.name = strdup(parser_current_text(parser));
                parser_advance(parser);
                
                /* Bounds: T extends A & B & C */
                if (parser_match(parser, TOK_EXTENDS)) {
                    ast_node_t *bound = parse_type(parser);
                    if (bound) {
                        ast_add_child(type_param, bound);
                    }
                    /* Additional interface bounds after & (TOK_BITAND) */
                    while (parser_match(parser, TOK_BITAND)) {
                        bound = parse_type(parser);
                        if (bound) {
                            ast_add_child(type_param, bound);
                        }
                    }
                }
                
                if (!method_type_params) {
                    method_type_params = slist_new(type_param);
                } else {
                    slist_append(method_type_params, type_param);
                }
            }
        } while (parser_match(parser, TOK_COMMA));
        parser_expect_gt(parser);
    }
    
    /* Method or field - need to parse type and name first */
    if (!is_type_start(parser_current_type(parser))) {
        parser_error(parser, "Expected type or constructor");
        /* Free annotations on error */
        if (annotations) {
            for (slist_t *node = annotations; node; node = node->next) {
                ast_free((ast_node_t *)node->data);
            }
            slist_free(annotations);
        }
        if (method_type_params) {
            for (slist_t *node = method_type_params; node; node = node->next) {
                ast_free((ast_node_t *)node->data);
            }
            slist_free(method_type_params);
        }
        return NULL;
    }
    
    int line = parser_current_line(parser);
    int col = parser_current_column(parser);
    
    ast_node_t *type_node = parse_type(parser);
    if (!type_node) {
        if (annotations) {
            for (slist_t *node = annotations; node; node = node->next) {
                ast_free((ast_node_t *)node->data);
            }
            slist_free(annotations);
        }
        return NULL;
    }
    
    if (!parser_check(parser, TOK_IDENTIFIER)) {
        parser_error(parser, "Expected method or field name");
        ast_free(type_node);
        if (annotations) {
            for (slist_t *node = annotations; node; node = node->next) {
                ast_free((ast_node_t *)node->data);
            }
            slist_free(annotations);
        }
        if (method_type_params) {
            for (slist_t *node = method_type_params; node; node = node->next) {
                ast_free((ast_node_t *)node->data);
            }
            slist_free(method_type_params);
        }
        return NULL;
    }
    
    char *name = strdup(parser_current_text(parser));
    parser_advance(parser);
    
    if (parser_check(parser, TOK_LPAREN)) {
        /* Method */
        ast_node_t *method = parse_method_decl(parser, modifiers, type_node, name,
                                               line, col);
        if (method) {
            method->annotations = annotations;
            /* Add method type parameters as children */
            if (method_type_params) {
                for (slist_t *node = method_type_params; node; node = node->next) {
                    ast_add_child(method, (ast_node_t *)node->data);
                }
                slist_free(method_type_params);
            }
        }
        free(name);
        return method;
    } else {
        /* Field - type parameters are not allowed */
        if (method_type_params) {
            parser_error(parser, "Type parameters not allowed on fields");
            for (slist_t *node = method_type_params; node; node = node->next) {
                ast_free((ast_node_t *)node->data);
            }
            slist_free(method_type_params);
        }
        ast_node_t *field = parse_field_decl(parser, modifiers, type_node, name,
                                             line, col);
        if (field) field->annotations = annotations;
        free(name);
        return field;
    }
}

/**
 * Parse a class body.
 */
ast_node_t *parse_class_body(parser_t *parser)
{
    /* Get class name from parent context if available */
    const char *class_name = NULL;  /* TODO: Pass from caller */
    
    if (!parser_expect(parser, TOK_LBRACE)) {
        return NULL;
    }
    
    ast_node_t *body = ast_new(AST_BLOCK, true ? parser_current_line(parser) : 0,
                               true ? parser_current_column(parser) : 0);
    
    while (!parser_check(parser, TOK_RBRACE) && !parser_check(parser, TOK_EOF)) {
        ast_node_t *member = parse_class_member(parser, class_name);
        if (member) {
            ast_add_child(body, member);
        } else if (parser->error_msg) {
            /* Error recovery */
            while (!parser_check(parser, TOK_SEMICOLON) &&
                   !parser_check(parser, TOK_RBRACE) &&
                   !parser_check(parser, TOK_EOF)) {
                parser_advance(parser);
            }
            if (parser_check(parser, TOK_SEMICOLON)) {
                parser_advance(parser);
            }
            free(parser->error_msg);
            parser->error_msg = NULL;
        }
    }
    
    parser_expect(parser, TOK_RBRACE);
    return body;
}

/**
 * Parse a type declaration (class, interface, enum, @interface).
 */
static ast_node_t *parse_type_decl_with_annotations(parser_t *parser, uint32_t modifiers, slist_t *annotations);

static ast_node_t *parse_type_decl(parser_t *parser, uint32_t modifiers)
{
    return parse_type_decl_with_annotations(parser, modifiers, NULL);
}

static ast_node_t *parse_type_decl_with_annotations(parser_t *parser, uint32_t modifiers, slist_t *annotations)
{
    int line = parser_current_line(parser);
    int col = parser_current_column(parser);
    
    ast_node_type_t type;
    if (parser_match(parser, TOK_CLASS)) {
        type = AST_CLASS_DECL;
    } else if (parser_match(parser, TOK_INTERFACE)) {
        type = AST_INTERFACE_DECL;
    } else if (parser_match(parser, TOK_ENUM)) {
        type = AST_ENUM_DECL;
    } else if (parser_match(parser, TOK_RECORD)) {
        type = AST_RECORD_DECL;
    } else if (parser_check(parser, TOK_AT) && parser_match(parser, TOK_AT)) {
        /* @interface annotation declaration */
        if (!parser_match(parser, TOK_INTERFACE)) {
            parser_error(parser, "Expected 'interface' after '@'");
            return NULL;
        }
        type = AST_ANNOTATION_DECL;
    } else {
        parser_error(parser, "Expected class, interface, enum, or record");
        return NULL;
    }
    
    /* Name */
    if (!parser_check(parser, TOK_IDENTIFIER)) {
        parser_error(parser, "Expected type name");
        return NULL;
    }
    
    ast_node_t *decl = ast_new(type, line, col);
    decl->data.node.flags = modifiers;
    decl->data.node.name = strdup(parser_current_text(parser));
    decl->annotations = annotations;  /* Store annotations on the declaration */
    parser_advance(parser);
    
    /* Type parameters (before record components for records) */
    if (parser_check(parser, TOK_LT)) {
        parser_advance(parser);
        do {
            if (parser_check(parser, TOK_IDENTIFIER)) {
                ast_node_t *type_param = ast_new(AST_TYPE_PARAMETER,
                                                  parser_current_line(parser),
                                                  parser_current_column(parser));
                type_param->data.node.name = strdup(parser_current_text(parser));
                parser_advance(parser);
                
                /* Bounds: T extends A & B & C */
                if (parser_match(parser, TOK_EXTENDS)) {
                    ast_node_t *bound = parse_type(parser);
                    if (bound) {
                        ast_add_child(type_param, bound);
                    }
                    /* Additional interface bounds after & (TOK_BITAND) */
                    while (parser_match(parser, TOK_BITAND)) {
                        bound = parse_type(parser);
                        if (bound) {
                            ast_add_child(type_param, bound);
                        }
                    }
                }
                
                ast_add_child(decl, type_param);
            }
        } while (parser_match(parser, TOK_COMMA));
        parser_expect_gt(parser);
    }
    
    /* Record components (for records only) */
    if (type == AST_RECORD_DECL) {
        if (!parser_expect(parser, TOK_LPAREN)) {
            ast_free(decl);
            return NULL;
        }
        
        if (!parser_check(parser, TOK_RPAREN)) {
            do {
                /* Parse optional annotations on record component */
                slist_t *comp_annotations = NULL;
                while (parser_check(parser, TOK_AT)) {
                    ast_node_t *annot = parse_annotation(parser);
                    if (annot) {
                        if (!comp_annotations) {
                            comp_annotations = slist_new(annot);
                        } else {
                            slist_append(comp_annotations, annot);
                        }
                    }
                }
                
                ast_node_t *comp_type = parse_type(parser);
                
                if (!parser_check(parser, TOK_IDENTIFIER)) {
                    parser_error(parser, "Expected record component name");
                    ast_free(decl);
                    return NULL;
                }
                
                /* Use AST_PARAMETER with MOD_RECORD_COMPONENT flag */
                ast_node_t *component = ast_new(AST_PARAMETER,
                                               parser_current_line(parser),
                                               parser_current_column(parser));
                component->data.node.name = strdup(parser_current_text(parser));
                component->data.node.flags = MOD_FINAL | MOD_RECORD_COMPONENT;
                component->annotations = comp_annotations;
                if (comp_type) {
                    ast_add_child(component, comp_type);
                }
                parser_advance(parser);
                
                ast_add_child(decl, component);
            } while (parser_match(parser, TOK_COMMA));
        }
        
        parser_expect(parser, TOK_RPAREN);
    }
    
    /* Extends (records cannot extend) */
    if (parser_match(parser, TOK_EXTENDS)) {
        if (type == AST_RECORD_DECL) {
            parser_error(parser, "Records cannot extend a class");
            ast_free(decl);
            return NULL;
        } else if (type == AST_INTERFACE_DECL) {
            /* Interface can extend multiple interfaces */
            do {
                ast_node_t *super_type = parse_type(parser);
                if (super_type) {
                    super_type->data.node.flags = 1;  /* Mark as extends */
                    ast_add_child(decl, super_type);
                }
            } while (parser_match(parser, TOK_COMMA));
        } else {
            /* Class extends single class */
            ast_node_t *super_type = parse_type(parser);
            if (super_type) {
                super_type->data.node.flags = 1;  /* Mark as extends */
                ast_add_child(decl, super_type);
            }
        }
    }
    
    /* Implements */
    if (parser_match(parser, TOK_IMPLEMENTS)) {
        do {
            ast_node_t *impl_type = parse_type(parser);
            if (impl_type) {
                impl_type->data.node.flags = 2;  /* Mark as implements */
                ast_add_child(decl, impl_type);
            }
        } while (parser_match(parser, TOK_COMMA));
    }
    
    /* Permits (Java 17+ sealed classes) */
    if (parser_match(parser, TOK_PERMITS)) {
        do {
            ast_node_t *permits_type = parse_type(parser);
            if (permits_type) {
                permits_type->data.node.flags = 3;  /* Mark as permits */
                ast_add_child(decl, permits_type);
            }
        } while (parser_match(parser, TOK_COMMA));
    }
    
    /* Body - pass class name for constructor detection */
    if (!parser_expect(parser, TOK_LBRACE)) {
        ast_free(decl);
        return NULL;
    }
    
    /* Enum bodies have a special format: constants first, then members */
    if (type == AST_ENUM_DECL) {
        /* Parse enum constants (comma-separated, end with semicolon or }) */
        while (!parser_check(parser, TOK_SEMICOLON) && 
               !parser_check(parser, TOK_RBRACE) && 
               !parser_check(parser, TOK_EOF)) {
            
            /* Parse optional annotations on enum constant */
            slist_t *const_annotations = NULL;
            while (parser_check(parser, TOK_AT)) {
                ast_node_t *annot = parse_annotation(parser);
                if (annot) {
                    if (!const_annotations) {
                        const_annotations = slist_new(annot);
                    } else {
                        slist_append(const_annotations, annot);
                    }
                }
            }
            
            if (!parser_check(parser, TOK_IDENTIFIER)) {
                if (parser_check(parser, TOK_SEMICOLON) || parser_check(parser, TOK_RBRACE)) {
                    break;  /* Empty enum or trailing comma */
                }
                parser_error(parser, "Expected enum constant name");
                ast_free(decl);
                return NULL;
            }
            
            ast_node_t *constant = ast_new(AST_ENUM_CONSTANT, 
                                           parser_current_line(parser),
                                           parser_current_column(parser));
            constant->data.node.name = strdup(parser_current_text(parser));
            constant->annotations = const_annotations;
            parser_advance(parser);
            
            /* Optional constructor arguments */
            if (parser_match(parser, TOK_LPAREN)) {
                while (!parser_check(parser, TOK_RPAREN) && !parser_check(parser, TOK_EOF)) {
                    ast_node_t *arg = parse_expression(parser);
                    if (arg) {
                        ast_add_child(constant, arg);
                    }
                    if (!parser_match(parser, TOK_COMMA)) {
                        break;
                    }
                }
                parser_expect(parser, TOK_RPAREN);
            }
            
            /* Optional anonymous class body for constant */
            if (parser_check(parser, TOK_LBRACE)) {
                /* Parse class body for this enum constant */
                parser_advance(parser);  /* consume { */
                while (!parser_check(parser, TOK_RBRACE) && !parser_check(parser, TOK_EOF)) {
                    ast_node_t *member = parse_class_member(parser, decl->data.node.name);
                    if (member) {
                        ast_add_child(constant, member);
                    }
                }
                parser_expect(parser, TOK_RBRACE);
            }
            
            ast_add_child(decl, constant);
            
            if (!parser_match(parser, TOK_COMMA)) {
                break;  /* No more constants */
            }
        }
        
        /* Consume optional semicolon after constants */
        parser_match(parser, TOK_SEMICOLON);
    }
    
    /* Parse class members (methods, fields, constructors) */
    while (!parser_check(parser, TOK_RBRACE) && !parser_check(parser, TOK_EOF)) {
        ast_node_t *member = parse_class_member(parser, decl->data.node.name);
        if (member) {
            ast_add_child(decl, member);
        } else if (parser->error_msg) {
            /* Error recovery */
            while (!parser_check(parser, TOK_SEMICOLON) &&
                   !parser_check(parser, TOK_RBRACE) &&
                   !parser_check(parser, TOK_EOF)) {
                parser_advance(parser);
            }
            if (parser_check(parser, TOK_SEMICOLON)) {
                parser_advance(parser);
            }
            free(parser->error_msg);
            parser->error_msg = NULL;
        }
    }
    
    parser_expect(parser, TOK_RBRACE);
    return decl;
}

/* ========================================================================
 * Compilation Unit Parsing
 * ======================================================================== */

/**
 * Parse a module name (e.g., java.base, com.example.mymodule).
 * Returns a newly allocated string.
 */
static char *parse_module_name(parser_t *parser)
{
    string_t *name = string_new("");
    
    /* First component can be identifier or certain keywords used as identifiers */
    if (parser_check(parser, TOK_IDENTIFIER) || parser_check(parser, TOK_OPEN) ||
        parser_check(parser, TOK_MODULE) || parser_check(parser, TOK_TRANSITIVE) ||
        parser_check(parser, TOK_EXPORTS) || parser_check(parser, TOK_OPENS) ||
        parser_check(parser, TOK_PROVIDES) || parser_check(parser, TOK_USES) ||
        parser_check(parser, TOK_REQUIRES) || parser_check(parser, TOK_TO) ||
        parser_check(parser, TOK_WITH)) {
        string_append(name, parser_current_text(parser));
        parser_advance(parser);
        
        while (parser_match(parser, TOK_DOT)) {
            string_append_c(name, '.');
            if (parser_check(parser, TOK_IDENTIFIER) || parser_check(parser, TOK_OPEN) ||
                parser_check(parser, TOK_MODULE) || parser_check(parser, TOK_TRANSITIVE) ||
                parser_check(parser, TOK_EXPORTS) || parser_check(parser, TOK_OPENS) ||
                parser_check(parser, TOK_PROVIDES) || parser_check(parser, TOK_USES) ||
                parser_check(parser, TOK_REQUIRES) || parser_check(parser, TOK_TO) ||
                parser_check(parser, TOK_WITH)) {
                string_append(name, parser_current_text(parser));
                parser_advance(parser);
            }
        }
    }
    
    return string_free(name, false);
}

/**
 * Parse a module declaration:
 *   [open] module module.name {
 *       requires [transitive] [static] module.name;
 *       exports package.name [to module.name, ...];
 *       opens package.name [to module.name, ...];
 *       uses service.class.name;
 *       provides service.class.name with impl.class.name, ...;
 *   }
 */
static ast_node_t *parse_module_decl(parser_t *parser, bool is_open)
{
    int line = parser_current_line(parser);
    int col = parser_current_column(parser);
    
    if (!parser_expect(parser, TOK_MODULE)) {
        return NULL;
    }
    
    ast_node_t *module = ast_new(AST_MODULE_DECL, line, col);
    if (is_open) {
        module->data.node.flags |= MOD_PUBLIC;  /* Use MOD_PUBLIC to indicate 'open' */
    }
    
    /* Module name */
    module->data.node.name = parse_module_name(parser);
    
    if (!parser_expect(parser, TOK_LBRACE)) {
        ast_free(module);
        return NULL;
    }
    
    /* Module directives */
    while (!parser_check(parser, TOK_RBRACE) && !parser_check(parser, TOK_EOF)) {
        ast_node_t *directive = NULL;
        int dir_line = parser_current_line(parser);
        int dir_col = parser_current_column(parser);
        
        if (parser_match(parser, TOK_REQUIRES)) {
            /* requires [transitive] [static] module.name; */
            directive = ast_new(AST_REQUIRES_DIRECTIVE, dir_line, dir_col);
            
            if (parser_match(parser, TOK_TRANSITIVE)) {
                directive->data.node.flags |= MOD_PUBLIC;  /* transitive */
            }
            if (parser_match(parser, TOK_STATIC)) {
                directive->data.node.flags |= MOD_STATIC;
            }
            
            directive->data.node.name = parse_module_name(parser);
            parser_expect(parser, TOK_SEMICOLON);
            
        } else if (parser_match(parser, TOK_EXPORTS)) {
            /* exports package.name [to module.name, ...]; */
            directive = ast_new(AST_EXPORTS_DIRECTIVE, dir_line, dir_col);
            directive->data.node.name = parse_module_name(parser);  /* package name */
            
            /* Optional 'to' clause */
            if (parser_match(parser, TOK_TO)) {
                do {
                    char *target = parse_module_name(parser);
                    ast_node_t *target_node = ast_new(AST_IDENTIFIER, dir_line, dir_col);
                    target_node->data.node.name = target;
                    ast_add_child(directive, target_node);
                } while (parser_match(parser, TOK_COMMA));
            }
            parser_expect(parser, TOK_SEMICOLON);
            
        } else if (parser_match(parser, TOK_OPENS)) {
            /* opens package.name [to module.name, ...]; */
            directive = ast_new(AST_OPENS_DIRECTIVE, dir_line, dir_col);
            directive->data.node.name = parse_module_name(parser);  /* package name */
            
            /* Optional 'to' clause */
            if (parser_match(parser, TOK_TO)) {
                do {
                    char *target = parse_module_name(parser);
                    ast_node_t *target_node = ast_new(AST_IDENTIFIER, dir_line, dir_col);
                    target_node->data.node.name = target;
                    ast_add_child(directive, target_node);
                } while (parser_match(parser, TOK_COMMA));
            }
            parser_expect(parser, TOK_SEMICOLON);
            
        } else if (parser_match(parser, TOK_USES)) {
            /* uses service.class.name; */
            directive = ast_new(AST_USES_DIRECTIVE, dir_line, dir_col);
            directive->data.node.name = parse_module_name(parser);  /* service class */
            parser_expect(parser, TOK_SEMICOLON);
            
        } else if (parser_match(parser, TOK_PROVIDES)) {
            /* provides service.class.name with impl.class.name, ...; */
            directive = ast_new(AST_PROVIDES_DIRECTIVE, dir_line, dir_col);
            directive->data.node.name = parse_module_name(parser);  /* service class */
            
            if (!parser_expect(parser, TOK_WITH)) {
                ast_free(directive);
                break;
            }
            
            /* Implementation classes */
            do {
                char *impl = parse_module_name(parser);
                ast_node_t *impl_node = ast_new(AST_IDENTIFIER, dir_line, dir_col);
                impl_node->data.node.name = impl;
                ast_add_child(directive, impl_node);
            } while (parser_match(parser, TOK_COMMA));
            
            parser_expect(parser, TOK_SEMICOLON);
            
        } else {
            parser_error(parser, "Expected module directive (requires, exports, opens, uses, provides)");
            /* Skip to semicolon for error recovery */
            while (!parser_check(parser, TOK_SEMICOLON) &&
                   !parser_check(parser, TOK_RBRACE) &&
                   !parser_check(parser, TOK_EOF)) {
                parser_advance(parser);
            }
            if (parser_check(parser, TOK_SEMICOLON)) {
                parser_advance(parser);
            }
            free(parser->error_msg);
            parser->error_msg = NULL;
            continue;
        }
        
        if (directive) {
            ast_add_child(module, directive);
        }
    }
    
    parser_expect(parser, TOK_RBRACE);
    return module;
}

/**
 * Parse a compilation unit (entire Java file).
 */
ast_node_t *parser_parse(parser_t *parser)
{
    ast_node_t *unit = ast_new(AST_COMPILATION_UNIT, 1, 1);
    
    /* Check for module declaration: [open] module ... */
    if (parser_check(parser, TOK_OPEN) || parser_check(parser, TOK_MODULE)) {
        bool is_open = parser_match(parser, TOK_OPEN);
        ast_node_t *module = parse_module_decl(parser, is_open);
        if (module) {
            ast_add_child(unit, module);
        }
        return unit;
    }
    
    /* Package declaration (may have annotations before it, e.g. @Deprecated package foo;) */
    slist_t *pkg_annotations = NULL;
    if (parser_check(parser, TOK_AT)) {
        /* Parse annotations - these may be package annotations or type annotations */
        uint32_t pkg_mods = parse_modifiers_with_annotations(parser, &pkg_annotations);
        (void)pkg_mods;  /* Package declarations don't have modifiers */
    }
    
    if (parser_check(parser, TOK_PACKAGE)) {
        int line = parser_current_line(parser);
        int col = parser_current_column(parser);
        parser_advance(parser);
        
        ast_node_t *pkg = ast_new(AST_PACKAGE_DECL, line, col);
        
        /* Attach annotations to package declaration */
        if (pkg_annotations) {
            for (slist_t *node = pkg_annotations; node; node = node->next) {
                ast_add_child(pkg, (ast_node_t *)node->data);
            }
            slist_free(pkg_annotations);  /* Don't free the nodes, they're now children */
            pkg_annotations = NULL;
        }
        
        /* Parse package name - contextual keywords like 'requires', 'exports' etc. 
         * can be used as package name components */
        string_t *name = string_new("");
        token_type_t tok = parser_current_type(parser);
        if (tok == TOK_IDENTIFIER || is_contextual_keyword(tok)) {
            string_append(name, parser_current_text(parser));
            parser_advance(parser);
            
            while (parser_match(parser, TOK_DOT)) {
                string_append_c(name, '.');
                tok = parser_current_type(parser);
                if (tok == TOK_IDENTIFIER || is_contextual_keyword(tok)) {
                    string_append(name, parser_current_text(parser));
                    parser_advance(parser);
                }
            }
        }
        
        pkg->data.node.name = string_free(name, false);
        parser_expect(parser, TOK_SEMICOLON);
        ast_add_child(unit, pkg);
    } else if (pkg_annotations) {
        /* Annotations were parsed but no package follows - these are type annotations */
        /* They'll be handled in the type declaration parsing below, but we need to 
         * put them back somehow. For now, we just skip them as this is unusual. */
        for (slist_t *node = pkg_annotations; node; node = node->next) {
            ast_free((ast_node_t *)node->data);
        }
        slist_free(pkg_annotations);
        pkg_annotations = NULL;
    }
    
    /* Import declarations */
    while (parser_check(parser, TOK_IMPORT)) {
        int line = parser_current_line(parser);
        int col = parser_current_column(parser);
        parser_advance(parser);
        
        ast_node_t *import = ast_new(AST_IMPORT_DECL, line, col);
        
        /* Static import */
        if (parser_match(parser, TOK_STATIC)) {
            import->data.node.flags = MOD_STATIC;
        }
        
        /* Parse import name */
        string_t *name = string_new("");
        if (parser_check(parser, TOK_IDENTIFIER)) {
            string_append(name, parser_current_text(parser));
            parser_advance(parser);
            
            while (parser_match(parser, TOK_DOT)) {
                string_append_c(name, '.');
                if (parser_check(parser, TOK_STAR)) {
                    string_append_c(name, '*');
                    parser_advance(parser);
                    break;
                } else if (parser_check(parser, TOK_IDENTIFIER)) {
                    string_append(name, parser_current_text(parser));
                    parser_advance(parser);
                }
            }
        }
        
        import->data.node.name = string_free(name, false);
        parser_expect(parser, TOK_SEMICOLON);
        ast_add_child(unit, import);
    }
    
    /* Type declarations */
    while (!parser_check(parser, TOK_EOF)) {
        slist_t *annotations = NULL;
        uint32_t modifiers = parse_modifiers_with_annotations(parser, &annotations);
        
        if (parser_check(parser, TOK_CLASS) ||
            parser_check(parser, TOK_INTERFACE) ||
            parser_check(parser, TOK_ENUM) ||
            parser_check(parser, TOK_RECORD) ||
            parser_check(parser, TOK_AT)) {  /* @interface */
            ast_node_t *type_decl = parse_type_decl_with_annotations(parser, modifiers, annotations);
            if (type_decl) {
                ast_add_child(unit, type_decl);
            }
        } else if (parser_check(parser, TOK_EOF)) {
            break;
        } else {
            parser_error(parser, "Expected type declaration");
            /* Skip to next likely type declaration */
            while (!parser_check(parser, TOK_CLASS) &&
                   !parser_check(parser, TOK_INTERFACE) &&
                   !parser_check(parser, TOK_ENUM) &&
                   !parser_check(parser, TOK_RECORD) &&
                   !parser_check(parser, TOK_AT) &&
                   !parser_check(parser, TOK_EOF)) {
                parser_advance(parser);
            }
            free(parser->error_msg);
            parser->error_msg = NULL;
            /* Free annotations on error path */
            if (annotations) {
                for (slist_t *node = annotations; node; node = node->next) {
                    ast_free((ast_node_t *)node->data);
                }
                slist_free(annotations);
            }
        }
    }
    
    return unit;
}

