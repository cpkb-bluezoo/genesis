/*
 * flow.c - definite assignment analysis (JLS 16)
 *
 * Copyright (C) 2020, 2026 Chris Burdess <dog@gnu.org>
 *
 * This file is part of genesis; see COPYING for the license terms.
 *
 * A local variable declared without an initializer ("int x;") may only be read where it is
 * definitely assigned: on every path that reaches the read, something has assigned it. This is
 * the dataflow analysis of JLS chapter 16, over the syntax tree after semantic analysis. Only
 * such variables are tracked; everything else is assigned wherever it is in scope.
 *
 * The state at a point is the set of tracked variables assigned there, or "dead" for a point no
 * path reaches (where, vacuously, every variable is assigned). A statement takes the state before
 * it and leaves the state after it. A boolean condition has two states after it, one for each
 * value it can have ("a && b" assigns what b assigns only when true). A jump (break, continue,
 * yield) records its state against its target, which takes the intersection of them when it
 * ends; a try statement with a finally block adds what the block assigns to the jumps that leave
 * it.
 *
 * Variables are found by name: a local variable may not hide another of its method (JLS 6.4), so
 * the names in scope at a point name different variables, and a nested class or lambda
 * parameter is no complication. A class declared in a method, and an anonymous class, is
 * analysed on its own: what it reads of its enclosing method is not checked.
 */

#include <config.h>

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "genesis.h"
#include "flow.h"
#include "util.h"

/* ========================================================================
 * State
 * ======================================================================== */

typedef struct {
    uint64_t *bits;     /* one bit for each tracked variable: assigned */
    bool dead;          /* no path reaches this point: every variable is assigned */
} da_state_t;

typedef enum {
    JUMP_BREAK,
    JUMP_CONTINUE,
    JUMP_YIELD
} jump_kind_t;

typedef struct target {
    ast_node_t *node;
    const char *label;      /* the label of a labelled statement */
    bool is_loop;
    bool is_switch;         /* a switch statement */
    bool is_yield;          /* a switch expression */
    bool is_labelled;       /* stands for a labelled statement */
    struct target *loop;    /* a labelled statement's loop, for "continue label" */
    struct target *next;
} target_t;

typedef struct jump {
    jump_kind_t kind;
    target_t *target;
    da_state_t state;
    struct jump *next;
} jump_t;

typedef struct {
    const char *name;
    int id;
} var_name_t;

typedef struct {
    semantic_t *sem;
    size_t words;               /* of state */
    int capacity;               /* tracked variables the state has room for */
    int count;                  /* tracked variables declared so far */
    var_name_t *names;          /* the tracked variables in scope */
    size_t name_count;
    size_t name_capacity;
    jump_t *jumps;              /* newest first */
    target_t *targets;          /* innermost first */
    target_t *pending_labelled; /* the labelled statement the next loop is the body of */
    ast_node_t *group_stmt;     /* the statement being run that is directly in a switch group */
    void **blocks;              /* everything allocated, freed together */
    size_t block_count;
    size_t block_capacity;
} flow_t;

static void *flow_alloc(flow_t *f, size_t size)
{
    if (f->block_count == f->block_capacity) {
        f->block_capacity = f->block_capacity ? f->block_capacity * 2 : 64;
        f->blocks = realloc(f->blocks, f->block_capacity * sizeof(void *));
    }
    void *p = calloc(1, size ? size : 1);
    f->blocks[f->block_count++] = p;
    return p;
}

static da_state_t st_new(flow_t *f)
{
    da_state_t s;
    s.bits = flow_alloc(f, f->words * sizeof(uint64_t));
    s.dead = false;
    return s;
}

static da_state_t st_dead(flow_t *f)
{
    da_state_t s = st_new(f);
    s.dead = true;
    return s;
}

static da_state_t st_copy(flow_t *f, const da_state_t *src)
{
    da_state_t s = st_new(f);
    memcpy(s.bits, src->bits, f->words * sizeof(uint64_t));
    s.dead = src->dead;
    return s;
}

/* dst = dst and src: assigned on both paths */
static void st_and(flow_t *f, da_state_t *dst, const da_state_t *src)
{
    if (src->dead) {
        return;
    }
    if (dst->dead) {
        memcpy(dst->bits, src->bits, f->words * sizeof(uint64_t));
        dst->dead = false;
        return;
    }
    for (size_t i = 0; i < f->words; i++) {
        dst->bits[i] &= src->bits[i];
    }
}

/* dst = dst or src: assigned on either path */
static void st_or(flow_t *f, da_state_t *dst, const da_state_t *src)
{
    if (dst->dead) {
        return;
    }
    if (src->dead) {
        dst->dead = true;
        return;
    }
    for (size_t i = 0; i < f->words; i++) {
        dst->bits[i] |= src->bits[i];
    }
}

static bool st_has(const da_state_t *s, int id)
{
    return s->dead || ((s->bits[id / 64] >> (id % 64)) & 1);
}

static void st_set(da_state_t *s, int id)
{
    if (!s->dead) {
        s->bits[id / 64] |= (uint64_t)1 << (id % 64);
    }
}

static void st_clear(da_state_t *s, int id)
{
    if (!s->dead) {
        s->bits[id / 64] &= ~((uint64_t)1 << (id % 64));
    }
}

/* ========================================================================
 * Helpers
 * ======================================================================== */

/* The children of a node, none for the kinds whose storage holds a name instead */
static slist_t *kids(const ast_node_t *n)
{
    switch (n->type) {
        case AST_IDENTIFIER:
        case AST_LITERAL:
        case AST_THIS_EXPR:
        case AST_SUPER_EXPR:
        case AST_PRIMITIVE_TYPE:
        case AST_VAR_TYPE:
            return NULL;
        default:
            return n->data.node.children;
    }
}

static ast_node_t *kid(const ast_node_t *n, int index)
{
    slist_t *c = kids(n);
    while (c && index > 0) {
        c = c->next;
        index--;
    }
    return c ? (ast_node_t *)c->data : NULL;
}

static ast_node_t *last_kid(const ast_node_t *n)
{
    slist_t *c = kids(n);
    ast_node_t *last = NULL;
    for (; c; c = c->next) {
        last = (ast_node_t *)c->data;
    }
    return last;
}

static ast_node_t *unwrap(ast_node_t *e)
{
    while (e && e->type == AST_PARENTHESIZED && kids(e)) {
        e = kid(e, 0);
    }
    return e;
}

static bool is_type_node(const ast_node_t *n)
{
    switch (n->type) {
        case AST_PRIMITIVE_TYPE:
        case AST_CLASS_TYPE:
        case AST_ARRAY_TYPE:
        case AST_TYPE_PARAMETER:
        case AST_WILDCARD_TYPE:
        case AST_VAR_TYPE:
        case AST_INTERSECTION_TYPE:
        case AST_TYPE_ARGS:
            return true;
        default:
            return false;
    }
}

static bool is_literal_of(ast_node_t *e, token_type_t token)
{
    e = unwrap(e);
    return e && e->type == AST_LITERAL && e->data.leaf.token_type == token;
}

/* How many variables with no initializer a subtree declares, anywhere in it */
static int count_blank_declarators(const ast_node_t *n)
{
    int count = 0;
    if (n->type == AST_VAR_DECLARATOR && !kids(n)) {
        count++;
    }
    if (n->type == AST_CASE_LABEL) {
        /* a declaration in a switch group is in scope in the groups after it too, and unassigned
         * there however it was initialised: tracked, with an initializer or not */
        for (slist_t *c = kids(n); c; c = c->next) {
            ast_node_t *child = (ast_node_t *)c->data;
            if (child && child->type == AST_VAR_DECL) {
                for (slist_t *d = kids(child); d; d = d->next) {
                    ast_node_t *decl = (ast_node_t *)d->data;
                    if (decl && decl->type == AST_VAR_DECLARATOR && kids(decl)) {
                        count++;
                    }
                }
            }
        }
    }
    for (slist_t *c = kids(n); c; c = c->next) {
        if (c->data) {
            count += count_blank_declarators((const ast_node_t *)c->data);
        }
    }
    return count;
}

/* ========================================================================
 * Scopes, variables, jumps
 * ======================================================================== */

static int declare(flow_t *f, const char *name)
{
    if (!name || f->count >= f->capacity) {
        return -1;
    }
    if (f->name_count == f->name_capacity) {
        f->name_capacity = f->name_capacity ? f->name_capacity * 2 : 16;
        f->names = realloc(f->names, f->name_capacity * sizeof(var_name_t));
    }
    int id = f->count++;
    f->names[f->name_count].name = name;
    f->names[f->name_count].id = id;
    f->name_count++;
    return id;
}

static int lookup(flow_t *f, const char *name)
{
    for (size_t i = f->name_count; i > 0; i--) {
        if (strcmp(f->names[i - 1].name, name) == 0) {
            return f->names[i - 1].id;
        }
    }
    return -1;
}

static void record_jump(flow_t *f, jump_kind_t kind, target_t *target, const da_state_t *st)
{
    if (!target) {
        return;
    }
    jump_t *j = flow_alloc(f, sizeof(jump_t));
    j->kind = kind;
    j->target = target;
    j->state = st_copy(f, st);
    j->next = f->jumps;
    f->jumps = j;
}

/* Merge into acc the states of the jumps of a kind to a target, removing them */
static void take_jumps(flow_t *f, target_t *target, jump_kind_t kind, da_state_t *acc)
{
    jump_t **link = &f->jumps;
    while (*link) {
        jump_t *j = *link;
        if (j->target == target && j->kind == kind) {
            st_and(f, acc, &j->state);
            *link = j->next;
        } else {
            link = &j->next;
        }
    }
}

static target_t *push_target(flow_t *f, ast_node_t *node)
{
    target_t *t = flow_alloc(f, sizeof(target_t));
    t->node = node;
    t->next = f->targets;
    f->targets = t;
    return t;
}

static void pop_target(flow_t *f, target_t *t)
{
    if (f->targets == t) {
        f->targets = t->next;
    }
}

/* ========================================================================
 * Declarations that hold bodies
 * ======================================================================== */

static void analyze_body(semantic_t *sem, ast_node_t *body);
static void walk_declarations(semantic_t *sem, ast_node_t *node);

/* ========================================================================
 * Expressions and statements
 * ======================================================================== */

static void expr(flow_t *f, ast_node_t *e, da_state_t *st);
static void cond(flow_t *f, ast_node_t *e, const da_state_t *in, da_state_t *t, da_state_t *fl);
static void stmt(flow_t *f, ast_node_t *s, da_state_t *st);

static void use(flow_t *f, ast_node_t *ident, da_state_t *st)
{
    if (!ident->data.leaf.name) {
        return;
    }
    int id = lookup(f, ident->data.leaf.name);
    if (id >= 0 && !st_has(st, id)) {
        semantic_error(f->sem, ident->line, ident->column,
                       "variable %s might not have been initialized", ident->data.leaf.name);
        st_set(st, id);     /* once is enough */
    }
}

static void exprs(flow_t *f, slist_t *list, da_state_t *st)
{
    for (; list; list = list->next) {
        if (list->data) {
            expr(f, (ast_node_t *)list->data, st);
        }
    }
}

static void lambda(flow_t *f, ast_node_t *e, da_state_t *st)
{
    ast_node_t *body = kid(e, 1);
    if (!body) {
        return;
    }
    target_t *saved_targets = f->targets;
    jump_t *saved_jumps = f->jumps;
    size_t name_mark = f->name_count;
    f->targets = NULL;
    da_state_t in = st_copy(f, st);
    if (body->type == AST_BLOCK) {
        stmt(f, body, &in);
    } else {
        expr(f, body, &in);
    }
    f->targets = saved_targets;
    f->jumps = saved_jumps;
    f->name_count = name_mark;
}

static void switch_expression(flow_t *f, ast_node_t *e, da_state_t *st)
{
    ast_node_t *selector = kid(e, 0);
    if (selector) {
        expr(f, selector, st);
    }
    da_state_t sel = st_copy(f, st);
    target_t *t = push_target(f, e);
    t->is_yield = true;
    size_t name_mark = f->name_count;
    bool any_rule = false;

    slist_t *rules = kids(e);
    for (slist_t *r = rules ? rules->next : NULL; r; r = r->next) {
        ast_node_t *rule = (ast_node_t *)r->data;
        if (!rule) {
            continue;
        }
        any_rule = true;
        da_state_t in = st_copy(f, &sel);
        ast_node_t *body = last_kid(rule);
        for (slist_t *l = kids(rule); l; l = l->next) {
            ast_node_t *label = (ast_node_t *)l->data;
            if (label && label != body && label->type == AST_GUARDED_PATTERN) {
                da_state_t guard_in = st_copy(f, &in);
                expr(f, kid(label, 1), &guard_in);
            }
        }
        if (body) {
            if (body->type == AST_BLOCK || body->type == AST_THROW_STMT) {
                stmt(f, body, &in);
            } else {
                expr(f, body, &in);
                record_jump(f, JUMP_YIELD, t, &in);
            }
        }
    }

    da_state_t out = st_dead(f);
    take_jumps(f, t, JUMP_YIELD, &out);
    if (!any_rule) {
        out = st_copy(f, &sel);
    }
    pop_target(f, t);
    f->name_count = name_mark;
    *st = out;
}

static void expr(flow_t *f, ast_node_t *e, da_state_t *st)
{
    if (!e) {
        return;
    }
    switch (e->type) {
        case AST_IDENTIFIER:
            use(f, e, st);
            return;

        case AST_LITERAL:
        case AST_THIS_EXPR:
        case AST_SUPER_EXPR:
        case AST_CLASS_LITERAL:
            return;

        case AST_PARENTHESIZED:
            expr(f, kid(e, 0), st);
            return;

        case AST_ASSIGNMENT_EXPR: {
            ast_node_t *target = unwrap(kid(e, 0));
            ast_node_t *value = kid(e, 1);
            bool plain = e->data.node.op_token == TOK_ASSIGN;
            if (target && target->type == AST_IDENTIFIER) {
                int id = target->data.leaf.name ? lookup(f, target->data.leaf.name) : -1;
                if (!plain) {
                    use(f, target, st);
                }
                expr(f, value, st);
                if (id >= 0) {
                    st_set(st, id);
                }
            } else {
                expr(f, target, st);
                expr(f, value, st);
            }
            return;
        }

        case AST_UNARY_EXPR: {
            token_type_t op = e->data.node.op_token;
            if (op == TOK_NOT) {
                da_state_t t, fl;
                cond(f, e, st, &t, &fl);
                *st = t;
                st_and(f, st, &fl);
                return;
            }
            expr(f, kid(e, 0), st);     /* ++ and -- read the variable first */
            return;
        }

        case AST_BINARY_EXPR: {
            token_type_t op = e->data.node.op_token;
            if (op == TOK_AND || op == TOK_OR) {
                da_state_t t, fl;
                cond(f, e, st, &t, &fl);
                *st = t;
                st_and(f, st, &fl);
                return;
            }
            exprs(f, kids(e), st);
            return;
        }

        case AST_CONDITIONAL_EXPR: {
            da_state_t t, fl;
            cond(f, kid(e, 0), st, &t, &fl);
            expr(f, kid(e, 1), &t);
            expr(f, kid(e, 2), &fl);
            *st = t;
            st_and(f, st, &fl);
            return;
        }

        case AST_LAMBDA_EXPR:
            lambda(f, e, st);
            return;

        case AST_SWITCH_EXPR:
            switch_expression(f, e, st);
            return;

        case AST_NEW_OBJECT: {
            slist_t *c = kids(e);
            for (; c; c = c->next) {
                ast_node_t *child = (ast_node_t *)c->data;
                if (!child || is_type_node(child)) {
                    continue;
                }
                if (child->type == AST_BLOCK && !c->next) {
                    walk_declarations(f->sem, child);   /* an anonymous class body, on its own */
                } else {
                    expr(f, child, st);
                }
            }
            return;
        }

        default:
            if (is_type_node(e)) {
                return;
            }
            for (slist_t *c = kids(e); c; c = c->next) {
                if (c->data) {
                    expr(f, (ast_node_t *)c->data, st);
                }
            }
            return;
    }
}

/*
 * The states after a boolean expression, for each of its values: "t" where it is true and "fl"
 * where it is false. "in" is the state before it and is left alone.
 */
static void cond(flow_t *f, ast_node_t *e, const da_state_t *in, da_state_t *t, da_state_t *fl)
{
    e = unwrap(e);
    if (!e) {
        *t = st_copy(f, in);
        *fl = st_copy(f, in);
        return;
    }
    if (is_literal_of(e, TOK_TRUE)) {
        *t = st_copy(f, in);
        *fl = st_dead(f);
        return;
    }
    if (is_literal_of(e, TOK_FALSE)) {
        *t = st_dead(f);
        *fl = st_copy(f, in);
        return;
    }
    if (e->type == AST_UNARY_EXPR && e->data.node.op_token == TOK_NOT) {
        cond(f, kid(e, 0), in, fl, t);
        return;
    }
    if (e->type == AST_BINARY_EXPR && e->data.node.op_token == TOK_AND) {
        da_state_t a_true, a_false, b_true, b_false;
        cond(f, kid(e, 0), in, &a_true, &a_false);
        cond(f, kid(e, 1), &a_true, &b_true, &b_false);
        *t = b_true;
        *fl = a_false;
        st_and(f, fl, &b_false);
        return;
    }
    if (e->type == AST_BINARY_EXPR && e->data.node.op_token == TOK_OR) {
        da_state_t a_true, a_false, b_true, b_false;
        cond(f, kid(e, 0), in, &a_true, &a_false);
        cond(f, kid(e, 1), &a_false, &b_true, &b_false);
        *t = a_true;
        st_and(f, t, &b_true);
        *fl = b_false;
        return;
    }
    if (e->type == AST_CONDITIONAL_EXPR) {
        da_state_t c_true, c_false, a_true, a_false, b_true, b_false;
        cond(f, kid(e, 0), in, &c_true, &c_false);
        cond(f, kid(e, 1), &c_true, &a_true, &a_false);
        cond(f, kid(e, 2), &c_false, &b_true, &b_false);
        *t = a_true;
        st_and(f, t, &b_true);
        *fl = a_false;
        st_and(f, fl, &b_false);
        return;
    }
    da_state_t out = st_copy(f, in);
    expr(f, e, &out);
    *t = out;
    *fl = st_copy(f, &out);
}

static target_t *find_break_target(flow_t *f, const char *label)
{
    for (target_t *t = f->targets; t; t = t->next) {
        if (label) {
            if (t->is_labelled && t->label && strcmp(t->label, label) == 0) {
                return t;
            }
        } else if (t->is_loop || t->is_switch) {
            return t;
        }
    }
    return NULL;
}

static target_t *find_continue_target(flow_t *f, const char *label)
{
    for (target_t *t = f->targets; t; t = t->next) {
        if (label) {
            if (t->is_labelled && t->label && strcmp(t->label, label) == 0) {
                return t->loop;
            }
        } else if (t->is_loop) {
            return t;
        }
    }
    return NULL;
}

static target_t *find_yield_target(flow_t *f)
{
    for (target_t *t = f->targets; t; t = t->next) {
        if (t->is_yield) {
            return t;
        }
    }
    return NULL;
}

/* A loop's target; a labelled statement it is the body of learns it for "continue label" */
static target_t *push_loop(flow_t *f, ast_node_t *node)
{
    target_t *t = push_target(f, node);
    t->is_loop = true;
    if (f->pending_labelled) {
        f->pending_labelled->loop = t;
        f->pending_labelled = NULL;
    }
    return t;
}

static void switch_statement(flow_t *f, ast_node_t *s, da_state_t *st)
{
    ast_node_t *selector = kid(s, 0);
    if (selector) {
        expr(f, selector, st);
    }
    da_state_t sel = st_copy(f, st);
    target_t *t = push_target(f, s);
    t->is_switch = true;
    f->pending_labelled = NULL;
    size_t name_mark = f->name_count;     /* the whole switch block is one scope */

    da_state_t cur = st_dead(f);         /* where the previous group's end falls into the next */
    bool has_default = false;
    bool exhaustive = false;
    slist_t *labels = kids(s);
    for (slist_t *l = labels ? labels->next : NULL; l; l = l->next) {
        ast_node_t *label = (ast_node_t *)l->data;
        if (!label) {
            continue;
        }
        da_state_t in = st_copy(f, &sel);
        st_and(f, &in, &cur);

        slist_t *body = kids(label);
        bool is_default = label->data.node.name && strcmp(label->data.node.name, "default") == 0;
        if (is_default) {
            has_default = true;
        } else if (body) {
            ast_node_t *case_expr = (ast_node_t *)body->data;
            body = body->next;
            if (case_expr) {
                switch (case_expr->type) {
                    case AST_TYPE_PATTERN:
                    case AST_RECORD_PATTERN:
                    case AST_UNNAMED_PATTERN:
                        exhaustive = true;
                        break;
                    case AST_GUARDED_PATTERN: {
                        exhaustive = true;
                        da_state_t guard_in = st_copy(f, &in);
                        expr(f, kid(case_expr, 1), &guard_in);
                        break;
                    }
                    default:
                        if (is_literal_of(case_expr, TOK_NULL)) {
                            exhaustive = true;
                        }
                        break;
                }
            }
        }
        for (; body; body = body->next) {
            f->group_stmt = (ast_node_t *)body->data;
            stmt(f, (ast_node_t *)body->data, &in);
            f->group_stmt = NULL;
        }
        cur = in;
    }

    da_state_t out = cur;
    if (!has_default && !exhaustive) {
        st_and(f, &out, &sel);
    }
    take_jumps(f, t, JUMP_BREAK, &out);
    pop_target(f, t);
    f->name_count = name_mark;
    *st = out;
}

static void try_statement(flow_t *f, ast_node_t *s, da_state_t *st)
{
    da_state_t try_in = st_copy(f, st);
    jump_t *mark = f->jumps;
    size_t name_mark = f->name_count;

    ast_node_t *block = NULL;
    ast_node_t *finally_clause = NULL;
    da_state_t body = st_copy(f, st);
    slist_t *c = kids(s);
    for (; c; c = c->next) {
        ast_node_t *child = (ast_node_t *)c->data;
        if (child && child->type == AST_RESOURCE_SPEC) {
            expr(f, last_kid(child), &body);
        } else {
            break;
        }
    }
    if (c && c->data && ((ast_node_t *)c->data)->type == AST_BLOCK) {
        block = (ast_node_t *)c->data;
        c = c->next;
    }
    stmt(f, block, &body);
    da_state_t result = body;

    for (; c; c = c->next) {
        ast_node_t *child = (ast_node_t *)c->data;
        if (!child) {
            continue;
        }
        if (child->type == AST_CATCH_CLAUSE) {
            da_state_t in = st_copy(f, &try_in);
            stmt(f, last_kid(child), &in);
            st_and(f, &result, &in);
        } else if (child->type == AST_FINALLY_CLAUSE) {
            finally_clause = child;
        }
    }

    if (finally_clause) {
        da_state_t fin = st_copy(f, &try_in);
        stmt(f, kid(finally_clause, 0), &fin);
        for (jump_t *j = f->jumps; j && j != mark; j = j->next) {
            st_or(f, &j->state, &fin);      /* leaving the try statement runs the finally block */
        }
        st_or(f, &result, &fin);
    }
    f->name_count = name_mark;
    *st = result;
}

static void stmt(flow_t *f, ast_node_t *s, da_state_t *st)
{
    if (!s) {
        return;
    }
    switch (s->type) {
        case AST_BLOCK: {
            size_t name_mark = f->name_count;
            for (slist_t *c = kids(s); c; c = c->next) {
                stmt(f, (ast_node_t *)c->data, st);
            }
            f->name_count = name_mark;
            return;
        }

        case AST_EMPTY_STMT:
            return;

        case AST_EXPR_STMT:
            expr(f, kid(s, 0), st);
            return;

        case AST_VAR_DECL:
            for (slist_t *c = kids(s); c; c = c->next) {
                ast_node_t *decl = (ast_node_t *)c->data;
                if (!decl || decl->type != AST_VAR_DECLARATOR) {
                    continue;
                }
                ast_node_t *init = kid(decl, 0);
                if (init) {
                    expr(f, init, st);
                    if (f->group_stmt == s) {
                        int id = declare(f, decl->data.node.name);
                        if (id >= 0) {
                            st_set(st, id);
                        }
                    }
                } else {
                    int id = declare(f, decl->data.node.name);
                    if (id >= 0) {
                        st_clear(st, id);
                    }
                }
            }
            return;

        case AST_IF_STMT: {
            da_state_t t, fl;
            cond(f, kid(s, 0), st, &t, &fl);
            stmt(f, kid(s, 1), &t);
            ast_node_t *else_stmt = kid(s, 2);
            if (else_stmt) {
                stmt(f, else_stmt, &fl);
            }
            *st = t;
            st_and(f, st, &fl);
            return;
        }

        case AST_WHILE_STMT: {
            target_t *t = push_loop(f, s);
            da_state_t body, out;
            cond(f, kid(s, 0), st, &body, &out);
            stmt(f, kid(s, 1), &body);
            take_jumps(f, t, JUMP_BREAK, &out);
            pop_target(f, t);
            *st = out;
            return;
        }

        case AST_DO_STMT: {
            target_t *t = push_loop(f, s);
            da_state_t body = st_copy(f, st);
            stmt(f, kid(s, 0), &body);
            take_jumps(f, t, JUMP_CONTINUE, &body);
            da_state_t again, out;
            cond(f, kid(s, 1), &body, &again, &out);
            take_jumps(f, t, JUMP_BREAK, &out);
            pop_target(f, t);
            *st = out;
            return;
        }

        case AST_FOR_STMT: {
            size_t name_mark = f->name_count;
            ast_node_t *init = kid(s, 0);
            ast_node_t *condition = kid(s, 1);
            ast_node_t *update = kid(s, 2);
            ast_node_t *body_stmt = kid(s, 3);
            if (init && init->type != AST_EMPTY_STMT) {
                if (init->type == AST_VAR_DECL) {
                    stmt(f, init, st);
                } else {
                    expr(f, init, st);
                }
            }
            target_t *t = push_loop(f, s);
            da_state_t body, out;
            if (!condition || condition->type == AST_EMPTY_STMT) {
                body = st_copy(f, st);
                out = st_dead(f);
            } else {
                cond(f, condition, st, &body, &out);
            }
            stmt(f, body_stmt, &body);
            take_jumps(f, t, JUMP_CONTINUE, &body);
            if (update && update->type != AST_EMPTY_STMT) {
                expr(f, update, &body);
            }
            take_jumps(f, t, JUMP_BREAK, &out);
            pop_target(f, t);
            f->name_count = name_mark;
            *st = out;
            return;
        }

        case AST_ENHANCED_FOR_STMT: {
            expr(f, kid(s, 2), st);
            target_t *t = push_loop(f, s);
            da_state_t body = st_copy(f, st);
            stmt(f, kid(s, 3), &body);
            da_state_t out = st_copy(f, st);
            take_jumps(f, t, JUMP_BREAK, &out);
            pop_target(f, t);
            *st = out;
            return;
        }

        case AST_SWITCH_STMT:
            switch_statement(f, s, st);
            return;

        case AST_RETURN_STMT:
        case AST_THROW_STMT:
            expr(f, kid(s, 0), st);
            st->dead = true;
            return;

        case AST_BREAK_STMT:
            record_jump(f, JUMP_BREAK, find_break_target(f, s->data.node.name), st);
            st->dead = true;
            return;

        case AST_CONTINUE_STMT:
            record_jump(f, JUMP_CONTINUE, find_continue_target(f, s->data.node.name), st);
            st->dead = true;
            return;

        case AST_YIELD_STMT:
            expr(f, kid(s, 0), st);
            record_jump(f, JUMP_YIELD, find_yield_target(f), st);
            st->dead = true;
            return;

        case AST_TRY_STMT:
            try_statement(f, s, st);
            return;

        case AST_SYNCHRONIZED_STMT:
            expr(f, kid(s, 0), st);
            stmt(f, kid(s, 1), st);
            return;

        case AST_ASSERT_STMT: {
            /* what the assertion assigns does not count: it may not run */
            da_state_t t, fl;
            cond(f, kid(s, 0), st, &t, &fl);
            da_state_t message = st_copy(f, &fl);
            expr(f, kid(s, 1), &message);
            return;
        }

        case AST_LABELED_STMT: {
            target_t *t = push_target(f, s);
            t->is_labelled = true;
            t->label = s->data.node.name;
            ast_node_t *inner = kid(s, 0);
            if (inner && (inner->type == AST_WHILE_STMT || inner->type == AST_DO_STMT ||
                          inner->type == AST_FOR_STMT || inner->type == AST_ENHANCED_FOR_STMT)) {
                f->pending_labelled = t;    /* "continue label" means that loop */
            }
            stmt(f, inner, st);
            f->pending_labelled = NULL;
            take_jumps(f, t, JUMP_BREAK, st);
            pop_target(f, t);
            return;
        }

        case AST_CLASS_DECL:
        case AST_INTERFACE_DECL:
        case AST_ENUM_DECL:
        case AST_RECORD_DECL:
        case AST_ANNOTATION_DECL:
            walk_declarations(f->sem, s);
            return;

        default:
            expr(f, s, st);
            return;
    }
}

/* ========================================================================
 * Driver
 * ======================================================================== */

static void flow_free(flow_t *f)
{
    for (size_t i = 0; i < f->block_count; i++) {
        free(f->blocks[i]);
    }
    free(f->blocks);
    free(f->names);
}

/* Analyse a method body, an initializer or the initializer of a field */
static void analyze_body(semantic_t *sem, ast_node_t *body)
{
    int blanks = count_blank_declarators(body);
    if (blanks == 0) {
        /* nothing to track, but a class declared in it may declare variables of its own */
        for (slist_t *c = kids(body); c; c = c->next) {
            if (c->data) {
                walk_declarations(sem, (ast_node_t *)c->data);
            }
        }
        return;
    }
    flow_t f;
    memset(&f, 0, sizeof(f));
    f.sem = sem;
    f.capacity = blanks;
    f.words = ((size_t)blanks + 63) / 64;
    da_state_t st = st_new(&f);
    if (body->type == AST_BLOCK) {
        stmt(&f, body, &st);
    } else {
        expr(&f, body, &st);
    }
    flow_free(&f);
}

static void walk_declarations(semantic_t *sem, ast_node_t *node)
{
    if (!node) {
        return;
    }
    switch (node->type) {
        case AST_METHOD_DECL:
        case AST_CONSTRUCTOR_DECL:
        case AST_INITIALIZER_BLOCK:
            for (slist_t *c = kids(node); c; c = c->next) {
                ast_node_t *child = (ast_node_t *)c->data;
                if (child && child->type == AST_BLOCK) {
                    analyze_body(sem, child);
                }
            }
            return;

        case AST_FIELD_DECL:
            for (slist_t *c = kids(node); c; c = c->next) {
                ast_node_t *decl = (ast_node_t *)c->data;
                if (decl && decl->type == AST_VAR_DECLARATOR && kid(decl, 0)) {
                    analyze_body(sem, kid(decl, 0));
                }
            }
            return;

        default:
            if (is_type_node(node)) {
                return;
            }
            for (slist_t *c = kids(node); c; c = c->next) {
                if (c->data) {
                    walk_declarations(sem, (ast_node_t *)c->data);
                }
            }
            return;
    }
}

void flow_check_definite_assignment(semantic_t *sem, ast_node_t *unit)
{
    walk_declarations(sem, unit);
}
