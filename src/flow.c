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
    uint64_t *bits;     /* a bit for each tracked variable: assigned on every path (words of
                         * them), then possibly assigned on some path (words more) */
    bool dead;          /* no path reaches this point: every variable is assigned, and none
                         * is possibly assigned */
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

/* A tracked variable of an enclosing method as a nested class sees it: whether it was assigned
 * where the class is declared, which is all that matters inside the class body */
typedef struct {
    ast_node_t *decl;
    bool assigned;
} captured_var_t;

typedef struct {
    captured_var_t *items;
    size_t count;
} captures_t;

typedef struct {
    semantic_t *sem;
    const captures_t *captures; /* of the method this class body is in, if it is in one */
    ast_node_t **decls;         /* the declarator of each tracked variable, by id */
    bool *is_final;             /* by id: declared final */
    bool *has_init;             /* by id: declared with an initializer */
    bool *is_field;             /* by id: a blank final field of the class being checked, whose
                                 * assignment is all that is followed */
    bool no_nested;             /* leave the classes declared in the code to the analysis of it */
    bool record_fields;         /* the fields are a record's components, whose symbols are not
                                 * told from other fields by identity */
    bool record_returns;        /* a constructor: where it returns, its fields must be assigned */
    jump_t *returns;
    uint64_t *induced;          /* variables a loop's earlier iteration may have assigned */
    int quiet;                  /* above zero: errors are not reported (a loop's first pass) */
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
    s.bits = flow_alloc(f, 2 * f->words * sizeof(uint64_t));
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
    memcpy(s.bits, src->bits, 2 * f->words * sizeof(uint64_t));
    s.dead = src->dead;
    return s;
}

/* The paths join: assigned only if on both, possibly assigned if on either */
static void st_and(flow_t *f, da_state_t *dst, const da_state_t *src)
{
    if (src->dead) {
        return;
    }
    if (dst->dead) {
        memcpy(dst->bits, src->bits, 2 * f->words * sizeof(uint64_t));
        dst->dead = false;
        return;
    }
    for (size_t i = 0; i < f->words; i++) {
        dst->bits[i] &= src->bits[i];
        dst->bits[f->words + i] |= src->bits[f->words + i];
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
        dst->bits[f->words + i] |= src->bits[f->words + i];
    }
}

static bool st_has(const da_state_t *s, int id)
{
    return s->dead || ((s->bits[id / 64] >> (id % 64)) & 1);
}

static bool st_maybe(const flow_t *f, const da_state_t *s, int id)
{
    return !s->dead && ((s->bits[f->words + id / 64] >> (id % 64)) & 1);
}

static void st_set(flow_t *f, da_state_t *s, int id)
{
    if (!s->dead) {
        s->bits[id / 64] |= (uint64_t)1 << (id % 64);
        s->bits[f->words + id / 64] |= (uint64_t)1 << (id % 64);
    }
}

static void st_set_maybe(flow_t *f, da_state_t *s, int id)
{
    if (!s->dead) {
        s->bits[f->words + id / 64] |= (uint64_t)1 << (id % 64);
    }
}

static void st_clear(flow_t *f, da_state_t *s, int id)
{
    if (!s->dead) {
        s->bits[id / 64] &= ~((uint64_t)1 << (id % 64));
        s->bits[f->words + id / 64] &= ~((uint64_t)1 << (id % 64));
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
    if (n->type == AST_VAR_DECL && (n->data.node.flags & MOD_FINAL)) {
        /* a final variable is tracked even with an initializer, to refuse assigning it */
        for (slist_t *d = kids(n); d; d = d->next) {
            ast_node_t *decl = (ast_node_t *)d->data;
            if (decl && decl->type == AST_VAR_DECLARATOR && kids(decl)) {
                count++;
            }
        }
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

static int declare(flow_t *f, ast_node_t *decl, bool is_final, bool has_init)
{
    const char *name = decl ? decl->data.node.name : NULL;
    if (!name || f->count >= f->capacity) {
        return -1;
    }
    if (f->name_count == f->name_capacity) {
        f->name_capacity = f->name_capacity ? f->name_capacity * 2 : 16;
        f->names = realloc(f->names, f->name_capacity * sizeof(var_name_t));
    }
    int id = f->count++;
    f->decls[id] = decl;
    f->is_final[id] = is_final;
    f->has_init[id] = has_init;
    f->induced[id / 64] &= ~((uint64_t)1 << (id % 64));
    f->names[f->name_count].name = name;
    f->names[f->name_count].id = id;
    f->name_count++;
    return id;
}

static int declare_field(flow_t *f, ast_node_t *decl)
{
    int id = declare(f, decl, true, false);
    if (id >= 0) {
        f->is_field[id] = true;
    }
    return id;
}

/* The blank final field of a name, for "this.name = value" */
static int lookup_field(flow_t *f, const char *name)
{
    for (size_t i = 0; i < f->name_count; i++) {
        int id = f->names[i].id;
        if (f->is_field[id] && strcmp(f->names[i].name, name) == 0) {
            return id;
        }
    }
    return -1;
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

static void flow_free(flow_t *f);
static void check_blank_finals(semantic_t *sem, ast_node_t *cls, bool want_static);
static void analyze_body(semantic_t *sem, ast_node_t *body, const captures_t *captures);
static void walk_declarations(semantic_t *sem, ast_node_t *node, const captures_t *captures);

/* What the code of a class declared here sees of the tracked variables in scope */
static captures_t *snapshot(flow_t *f, const da_state_t *st)
{
    size_t outer = f->captures ? f->captures->count : 0;
    captures_t *c = flow_alloc(f, sizeof(captures_t));
    c->items = flow_alloc(f, (outer + f->name_count) * sizeof(captured_var_t));
    for (size_t i = 0; i < outer; i++) {
        c->items[c->count++] = f->captures->items[i];
    }
    for (size_t i = 0; i < f->name_count; i++) {
        int id = f->names[i].id;
        c->items[c->count].decl = f->decls[id];
        c->items[c->count].assigned = st_has(st, id);
        c->count++;
    }
    return c;
}

/* ========================================================================
 * Expressions and statements
 * ======================================================================== */

static void expr(flow_t *f, ast_node_t *e, da_state_t *st);
static void cond(flow_t *f, ast_node_t *e, const da_state_t *in, da_state_t *t, da_state_t *fl);
static void stmt(flow_t *f, ast_node_t *s, da_state_t *st);

static void report(flow_t *f, const ast_node_t *at, const char *format, const char *name)
{
    if (f->quiet == 0) {
        semantic_error(f->sem, at->line, at->column, format, name);
    }
}

/* An error about a field, reported by the analysis of the fields (whose base level is quiet: the
 * analysis of each body has reported what is its own) but not in a first pass over a loop */
static void report_field(flow_t *f, const ast_node_t *at, const char *format, const char *name)
{
    if (f->quiet <= 1) {
        semantic_error(f->sem, at->line, at->column, format, name);
    }
}

/* Is this symbol the field the tracked variable stands for */
static bool is_our_field(const flow_t *f, int id, const symbol_t *sym)
{
    if (f->record_fields) {
        return sym && sym->kind == SYM_FIELD && sym->name && f->decls[id]->data.node.name &&
               strcmp(sym->name, f->decls[id]->data.node.name) == 0;
    }
    if (!sym || sym->kind != SYM_FIELD || !sym->ast) {
        return false;
    }
    const ast_node_t *decl = f->decls[id];
    if (sym->ast == decl) {
        return true;
    }
    if (sym->ast->type == AST_FIELD_DECL) {
        for (slist_t *c = kids(sym->ast); c; c = c->next) {
            if (c->data == decl) {
                return true;
            }
        }
    }
    return false;
}

/* DA only: assigned as far as reads are concerned, not known to be this field being assigned */
static void st_set_assigned(da_state_t *s, int id)
{
    if (!s->dead) {
        s->bits[id / 64] |= (uint64_t)1 << (id % 64);
    }
}

static void use(flow_t *f, ast_node_t *ident, da_state_t *st)
{
    if (!ident->data.leaf.name) {
        return;
    }
    int id = lookup(f, ident->data.leaf.name);
    if (id >= 0 && f->is_field[id]) {
        /* a field of the class: a read of it is one only if the name was resolved to it, a local
         * or parameter of the name being what is read otherwise */
        if (is_our_field(f, id, ident->sem_symbol) && !st_has(st, id)) {
            report_field(f, ident, "variable %s might not have been initialized", ident->data.leaf.name);
            st_set_assigned(st, id);
        }
        return;
    }
    if (id >= 0) {
        if (!st_has(st, id)) {
            report(f, ident, "variable %s might not have been initialized", ident->data.leaf.name);
            st_set(f, st, id);     /* once is enough */
        }
        return;
    }
    /* In a class declared in a method: a local of that method, which is what the symbol the
     * name was resolved to says (a field or parameter of the class of the same name is not) */
    symbol_t *sym = ident->sem_symbol;
    if (f->captures && sym && sym->kind == SYM_LOCAL_VAR && sym->ast) {
        for (size_t i = 0; i < f->captures->count; i++) {
            if (f->captures->items[i].decl == sym->ast && !f->captures->items[i].assigned) {
                report(f, ident, "variable %s might not have been initialized", ident->data.leaf.name);
                f->captures->items[i].assigned = true;
                return;
            }
        }
    }
}

/* An assignment to a tracked variable: a final one may be assigned once, and only if it has no
 * initializer */
static void check_assign(flow_t *f, ast_node_t *ident, int id, da_state_t *st)
{
    if (!f->is_final[id] || st->dead || f->is_field[id]) {
        return;
    }
    if (f->has_init[id]) {
        report(f, ident, "cannot assign a value to final variable %s", ident->data.leaf.name);
    } else if (st_maybe(f, st, id)) {
        bool in_loop = (f->induced[id / 64] >> (id % 64)) & 1;
        report(f, ident, in_loop ? "variable %s might be assigned in loop" :
                                   "variable %s might already have been assigned", ident->data.leaf.name);
    }
}

/* An assignment to a blank final field that may follow another */
static void check_field_assign(flow_t *f, const ast_node_t *at, int id, const da_state_t *st)
{
    if (st->dead || !st_maybe(f, st, id)) {
        return;
    }
    bool in_loop = (f->induced[id / 64] >> (id % 64)) & 1;
    report_field(f, at, in_loop ? "variable %s might be assigned in loop" :
                                  "variable %s might already have been assigned",
                 f->decls[id]->data.node.name);
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
    bool saved_returns = f->record_returns;
    size_t name_mark = f->name_count;
    f->targets = NULL;
    f->record_returns = false;
    da_state_t in = st_copy(f, st);
    if (body->type == AST_BLOCK) {
        stmt(f, body, &in);
    } else {
        expr(f, body, &in);
    }
    f->targets = saved_targets;
    f->jumps = saved_jumps;
    f->record_returns = saved_returns;
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
                if (id >= 0 && f->is_field[id]) {
                    symbol_t *sym = target->sem_symbol;
                    if (is_our_field(f, id, sym)) {
                        check_field_assign(f, target, id, st);
                        st_set(f, st, id);
                    } else if (!sym || (sym->kind != SYM_LOCAL_VAR && sym->kind != SYM_PARAMETER)) {
                        st_set_assigned(st, id);    /* the name is not known to be anything else */
                    }
                } else if (id >= 0) {
                    check_assign(f, target, id, st);
                    st_set(f, st, id);
                }
            } else if (target && target->type == AST_FIELD_ACCESS && target->data.node.name &&
                       kid(target, 0) && unwrap(kid(target, 0))->type == AST_THIS_EXPR) {
                expr(f, value, st);
                int id = lookup_field(f, target->data.node.name);
                if (id >= 0) {
                    check_field_assign(f, target, id, st);
                    st_set(f, st, id);
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
            if (op == TOK_INC || op == TOK_DEC) {
                ast_node_t *operand = unwrap(kid(e, 0));
                if (operand && operand->type == AST_IDENTIFIER && operand->data.leaf.name) {
                    int id = lookup(f, operand->data.leaf.name);
                    if (id >= 0) {
                        check_assign(f, operand, id, st);
                    }
                }
            }
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

        case AST_FIELD_ACCESS: {
            ast_node_t *receiver = kid(e, 0);
            if (receiver && unwrap(receiver)->type == AST_THIS_EXPR && e->data.node.name) {
                int id = lookup_field(f, e->data.node.name);
                if (id >= 0 && !st_has(st, id)) {
                    report_field(f, e, "variable %s might not have been initialized", e->data.node.name);
                    st_set_assigned(st, id);
                }
                return;
            }
            expr(f, receiver, st);
            return;
        }

        case AST_NEW_OBJECT: {
            slist_t *c = kids(e);
            for (; c; c = c->next) {
                ast_node_t *child = (ast_node_t *)c->data;
                if (!child || is_type_node(child)) {
                    continue;
                }
                if (child->type == AST_BLOCK && !c->next) {
                    if (!f->no_nested) {
                        if (f->quiet == 0) {
                            check_blank_finals(f->sem, child, false);
                        }
                        walk_declarations(f->sem, child, snapshot(f, st));   /* an anonymous class body */
                    }
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

/* ========================================================================
 * Constant expressions (JLS 15.29)
 * ======================================================================== */

typedef enum {
    CONST_NONE,
    CONST_INT,      /* an int, and a char, short or byte */
    CONST_LONG,
    CONST_REAL,     /* a double, or a float (single) */
    CONST_BOOL
} const_kind_t;

typedef struct {
    const_kind_t kind;
    long long i;        /* INT, LONG, BOOL */
    double d;           /* REAL */
    bool single;        /* REAL: a float, whose results are rounded to one */
} const_value_t;

static const_value_t const_none(void)
{
    const_value_t v;
    memset(&v, 0, sizeof(v));
    return v;
}

static const_value_t const_int(long long i)
{
    const_value_t v = const_none();
    v.kind = CONST_INT;
    v.i = (int32_t)i;
    return v;
}

static const_value_t const_long(long long i)
{
    const_value_t v = const_none();
    v.kind = CONST_LONG;
    v.i = i;
    return v;
}

static const_value_t const_real(double d, bool single)
{
    const_value_t v = const_none();
    v.kind = CONST_REAL;
    v.single = single;
    v.d = single ? (double)(float)d : d;
    return v;
}

static const_value_t const_bool(bool b)
{
    const_value_t v = const_none();
    v.kind = CONST_BOOL;
    v.i = b;
    return v;
}

static bool const_numeric(const_value_t v)
{
    return v.kind == CONST_INT || v.kind == CONST_LONG || v.kind == CONST_REAL;
}

static double const_as_double(const_value_t v)
{
    return v.kind == CONST_REAL ? v.d : (double)v.i;
}

/* The value converted to the type of a variable that holds it (JLS 5.1.2, 5.2) */
static const_value_t const_convert(const_value_t v, type_kind_t to)
{
    switch (to) {
        case TYPE_BOOLEAN:
            return v.kind == CONST_BOOL ? v : const_none();
        case TYPE_INT:
        case TYPE_SHORT:
        case TYPE_BYTE:
        case TYPE_CHAR:
            return v.kind == CONST_INT ? v : const_none();
        case TYPE_LONG:
            return v.kind == CONST_INT || v.kind == CONST_LONG ? const_long(v.i) : const_none();
        case TYPE_FLOAT:
            return const_numeric(v) ? const_real(const_as_double(v), true) : const_none();
        case TYPE_DOUBLE:
            return const_numeric(v) ? const_real(const_as_double(v), false) : const_none();
        default:
            return const_none();
    }
}

static bool const_primitive_kind(const type_t *t)
{
    return t && (t->kind == TYPE_BOOLEAN || t->kind == TYPE_INT || t->kind == TYPE_SHORT ||
                 t->kind == TYPE_BYTE || t->kind == TYPE_CHAR || t->kind == TYPE_LONG ||
                 t->kind == TYPE_FLOAT || t->kind == TYPE_DOUBLE);
}

static const_value_t const_expr(flow_t *f, ast_node_t *e, int depth, bool foreign);

/*
 * The value of a final field, from its initializer or a class file's constant. "foreign" is set
 * where the expression belongs to the declaration of something else: its names are known by the
 * symbols they resolved to and not by the variables of the code being analysed.
 */
static const_value_t const_of_symbol(flow_t *f, const symbol_t *sym, int depth)
{
    if (!sym || depth > 8 || !const_primitive_kind(sym->type) || sym->kind != SYM_FIELD ||
        !(sym->modifiers & MOD_FINAL)) {
        return const_none();
    }
    type_kind_t kind = sym->type->kind;
    if (sym->data.var_data.has_const_fp) {
        return const_convert(const_real(sym->data.var_data.const_fp, kind == TYPE_FLOAT), kind);
    }
    if (sym->data.var_data.has_const_value) {
        long long raw = sym->data.var_data.const_value;
        const_value_t v = kind == TYPE_BOOLEAN ? const_bool(raw != 0)
                        : kind == TYPE_LONG ? const_long(raw)
                        : kind == TYPE_FLOAT || kind == TYPE_DOUBLE ? const_real((double)raw, kind == TYPE_FLOAT)
                        : const_int(raw);
        return v;
    }
    const ast_node_t *decl = sym->ast;
    if (decl && decl->type == AST_FIELD_DECL) {
        const ast_node_t *found = NULL;
        for (slist_t *c = kids(decl); c; c = c->next) {
            const ast_node_t *d = (const ast_node_t *)c->data;
            if (d && d->type == AST_VAR_DECLARATOR && d->data.node.name && sym->name &&
                strcmp(d->data.node.name, sym->name) == 0) {
                found = d;
            }
        }
        decl = found;
    }
    if (!decl || decl->type != AST_VAR_DECLARATOR || !kids(decl)) {
        return const_none();
    }
    return const_convert(const_expr(f, kid(decl, 0), depth + 1, true), kind);
}

static const_value_t const_binary(token_type_t op, const_value_t a, const_value_t b)
{
    if (a.kind == CONST_BOOL && b.kind == CONST_BOOL) {
        switch (op) {
            case TOK_AND:
            case TOK_BITAND: return const_bool(a.i && b.i);
            case TOK_OR:
            case TOK_BITOR: return const_bool(a.i || b.i);
            case TOK_CARET:
            case TOK_NE: return const_bool(a.i != b.i);
            case TOK_EQ: return const_bool(a.i == b.i);
            default: return const_none();
        }
    }
    if (!const_numeric(a) || !const_numeric(b)) {
        return const_none();
    }
    bool shift = op == TOK_LSHIFT || op == TOK_RSHIFT || op == TOK_URSHIFT;
    if (shift) {
        if (a.kind == CONST_REAL || b.kind == CONST_REAL) {
            return const_none();
        }
        if (a.kind == CONST_LONG) {
            int n = (int)(b.i & 63);
            switch (op) {
                case TOK_LSHIFT: return const_long((long long)((uint64_t)a.i << n));
                case TOK_RSHIFT: return const_long(a.i >> n);
                default: return const_long((long long)((uint64_t)a.i >> n));
            }
        }
        int32_t x = (int32_t)a.i;
        int n = (int)(b.i & 31);
        switch (op) {
            case TOK_LSHIFT: return const_int((int32_t)((uint32_t)x << n));
            case TOK_RSHIFT: return const_int(x >> n);
            default: return const_int((int32_t)((uint32_t)x >> n));
        }
    }
    if (a.kind == CONST_REAL || b.kind == CONST_REAL) {
        bool single = !((a.kind == CONST_REAL && !a.single) || (b.kind == CONST_REAL && !b.single));
        double x = const_as_double(a);
        double y = const_as_double(b);
        switch (op) {
            case TOK_PLUS: return const_real(x + y, single);
            case TOK_MINUS: return const_real(x - y, single);
            case TOK_STAR: return const_real(x * y, single);
            case TOK_SLASH: return const_real(x / y, single);
            case TOK_LT: return const_bool(x < y);
            case TOK_GT: return const_bool(x > y);
            case TOK_LE: return const_bool(x <= y);
            case TOK_GE: return const_bool(x >= y);
            case TOK_EQ: return const_bool(x == y);
            case TOK_NE: return const_bool(x != y);
            default: return const_none();
        }
    }
    if (a.kind == CONST_LONG || b.kind == CONST_LONG) {
        long long x = a.i;
        long long y = b.i;
        switch (op) {
            case TOK_PLUS: return const_long((long long)((uint64_t)x + (uint64_t)y));
            case TOK_MINUS: return const_long((long long)((uint64_t)x - (uint64_t)y));
            case TOK_STAR: return const_long((long long)((uint64_t)x * (uint64_t)y));
            case TOK_SLASH:
                return y == 0 || (x == INT64_MIN && y == -1) ? const_none() : const_long(x / y);
            case TOK_MOD:
                return y == 0 || (x == INT64_MIN && y == -1) ? const_none() : const_long(x % y);
            case TOK_BITAND: return const_long(x & y);
            case TOK_BITOR: return const_long(x | y);
            case TOK_CARET: return const_long(x ^ y);
            case TOK_LT: return const_bool(x < y);
            case TOK_GT: return const_bool(x > y);
            case TOK_LE: return const_bool(x <= y);
            case TOK_GE: return const_bool(x >= y);
            case TOK_EQ: return const_bool(x == y);
            case TOK_NE: return const_bool(x != y);
            default: return const_none();
        }
    }
    int32_t x = (int32_t)a.i;
    int32_t y = (int32_t)b.i;
    switch (op) {
        case TOK_PLUS: return const_int((int32_t)((uint32_t)x + (uint32_t)y));
        case TOK_MINUS: return const_int((int32_t)((uint32_t)x - (uint32_t)y));
        case TOK_STAR: return const_int((int32_t)((uint32_t)x * (uint32_t)y));
        case TOK_SLASH:
            return y == 0 || (x == INT32_MIN && y == -1) ? const_none() : const_int(x / y);
        case TOK_MOD:
            return y == 0 || (x == INT32_MIN && y == -1) ? const_none() : const_int(x % y);
        case TOK_BITAND: return const_int(x & y);
        case TOK_BITOR: return const_int(x | y);
        case TOK_CARET: return const_int(x ^ y);
        case TOK_LT: return const_bool(x < y);
        case TOK_GT: return const_bool(x > y);
        case TOK_LE: return const_bool(x <= y);
        case TOK_GE: return const_bool(x >= y);
        case TOK_EQ: return const_bool(x == y);
        case TOK_NE: return const_bool(x != y);
        default: return const_none();
    }
}

static const_value_t const_expr(flow_t *f, ast_node_t *e, int depth, bool foreign)
{
    if (!e || depth > 8) {
        return const_none();
    }
    switch (e->type) {
        case AST_PARENTHESIZED:
            return const_expr(f, kid(e, 0), depth, foreign);

        case AST_LITERAL:
            switch (e->data.leaf.token_type) {
                case TOK_TRUE:
                case TOK_FALSE:
                    return const_bool(e->data.leaf.token_type == TOK_TRUE);
                case TOK_INTEGER_LITERAL:
                    return const_int(e->data.leaf.value.int_val);
                case TOK_LONG_LITERAL:
                    return const_long(e->data.leaf.value.int_val);
                case TOK_FLOAT_LITERAL:
                    return const_real(e->data.leaf.value.float_val, true);
                case TOK_DOUBLE_LITERAL:
                    return const_real(e->data.leaf.value.float_val, false);
                case TOK_CHAR_LITERAL:
                    return e->data.leaf.value.str_val && e->data.leaf.value.str_val[0] ?
                        const_int((long long)char_literal_value(e->data.leaf.value.str_val)) : const_none();
                default:
                    return const_none();
            }

        case AST_IDENTIFIER: {
            if (!foreign && e->data.leaf.name) {
                int id = lookup(f, e->data.leaf.name);
                if (id >= 0) {
                    /* a final local with a constant initializer; a field is no tracked local */
                    if (f->is_field[id] || !f->is_final[id] || !f->has_init[id] ||
                        !e->sem_symbol || !const_primitive_kind(e->sem_symbol->type)) {
                        return const_none();
                    }
                    return const_convert(const_expr(f, kid(f->decls[id], 0), depth + 1, false),
                                         e->sem_symbol->type->kind);
                }
            }
            return const_of_symbol(f, e->sem_symbol, depth);
        }

        case AST_FIELD_ACCESS:
            return const_of_symbol(f, e->sem_symbol, depth);

        case AST_UNARY_EXPR: {
            const_value_t a = const_expr(f, kid(e, 0), depth + 1, foreign);
            token_type_t op = e->data.node.op_token;
            if (op == TOK_NOT) {
                return a.kind == CONST_BOOL ? const_bool(!a.i) : const_none();
            }
            if (!const_numeric(a)) {
                return const_none();
            }
            if (op == TOK_PLUS) {
                return a;
            }
            if (op == TOK_MINUS) {
                return a.kind == CONST_REAL ? const_real(-a.d, a.single)
                     : a.kind == CONST_LONG ? const_long((long long)(0 - (uint64_t)a.i))
                     : const_int((int32_t)(0 - (uint32_t)a.i));
            }
            if (op == TOK_TILDE && a.kind != CONST_REAL) {
                return a.kind == CONST_LONG ? const_long(~a.i) : const_int(~(int32_t)a.i);
            }
            return const_none();
        }

        case AST_BINARY_EXPR:
            return const_binary(e->data.node.op_token, const_expr(f, kid(e, 0), depth + 1, foreign),
                                const_expr(f, kid(e, 1), depth + 1, foreign));

        case AST_CONDITIONAL_EXPR: {
            const_value_t c = const_expr(f, kid(e, 0), depth + 1, foreign);
            const_value_t a = const_expr(f, kid(e, 1), depth + 1, foreign);
            const_value_t b = const_expr(f, kid(e, 2), depth + 1, foreign);
            if (c.kind != CONST_BOOL || a.kind == CONST_NONE || a.kind != b.kind) {
                return const_none();
            }
            return c.i ? a : b;
        }

        default:
            return const_none();
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
    const_value_t constant = const_expr(f, e, 0, false);
    if (constant.kind == CONST_BOOL) {
        /* a constant condition has the one value, whatever is assigned on the way to the other */
        *t = constant.i ? st_copy(f, in) : st_dead(f);
        *fl = constant.i ? st_dead(f) : st_copy(f, in);
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

/* Mark possibly assigned every tracked variable a subtree assigns anywhere in it: what a catch or
 * finally block may find already assigned, wherever in the try block the exception came from */
static void mark_assigned_in(flow_t *f, const ast_node_t *n, da_state_t *st)
{
    if (!n) {
        return;
    }
    ast_node_t *target = NULL;
    if (n->type == AST_ASSIGNMENT_EXPR) {
        target = unwrap(kid(n, 0));
    } else if (n->type == AST_UNARY_EXPR &&
               (n->data.node.op_token == TOK_INC || n->data.node.op_token == TOK_DEC)) {
        target = unwrap(kid(n, 0));
    }
    if (target && target->type == AST_IDENTIFIER && target->data.leaf.name) {
        int id = lookup(f, target->data.leaf.name);
        if (id >= 0 && (!f->is_field[id] || is_our_field(f, id, target->sem_symbol))) {
            st_set_maybe(f, st, id);
        }
    } else if (target && target->type == AST_FIELD_ACCESS && target->data.node.name && kid(target, 0) &&
               unwrap(kid(target, 0))->type == AST_THIS_EXPR) {
        int id = lookup_field(f, target->data.node.name);
        if (id >= 0) {
            st_set_maybe(f, st, id);
        }
    }
    for (slist_t *c = kids(n); c; c = c->next) {
        mark_assigned_in(f, (const ast_node_t *)c->data, st);
    }
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
            mark_assigned_in(f, block, &in);
            stmt(f, last_kid(child), &in);
            st_and(f, &result, &in);
        } else if (child->type == AST_FINALLY_CLAUSE) {
            finally_clause = child;
        }
    }

    if (finally_clause) {
        da_state_t fin = st_copy(f, &try_in);
        mark_assigned_in(f, block, &fin);
        for (slist_t *k = kids(s); k; k = k->next) {
            ast_node_t *catch_clause = (ast_node_t *)k->data;
            if (catch_clause && catch_clause->type == AST_CATCH_CLAUSE) {
                mark_assigned_in(f, last_kid(catch_clause), &fin);
            }
        }
        stmt(f, kid(finally_clause, 0), &fin);
        for (jump_t *j = f->jumps; j && j != mark; j = j->next) {
            st_or(f, &j->state, &fin);      /* leaving the try statement runs the finally block */
        }
        st_or(f, &result, &fin);
    }
    f->name_count = name_mark;
    *st = result;
}

/*
 * One pass over a loop: "in" is the state where the loop is entered (after a for statement's
 * initialization), "out" gets the state after it, and "end" the state at the end of an iteration,
 * from which the next one starts.
 */
static void loop_pass(flow_t *f, ast_node_t *s, const da_state_t *in, da_state_t *out, da_state_t *end)
{
    switch (s->type) {
        case AST_WHILE_STMT: {
            target_t *t = push_loop(f, s);
            da_state_t body, o;
            cond(f, kid(s, 0), in, &body, &o);
            stmt(f, kid(s, 1), &body);
            take_jumps(f, t, JUMP_CONTINUE, &body);
            take_jumps(f, t, JUMP_BREAK, &o);
            pop_target(f, t);
            *end = body;
            *out = o;
            return;
        }

        case AST_DO_STMT: {
            target_t *t = push_loop(f, s);
            da_state_t body = st_copy(f, in);
            stmt(f, kid(s, 0), &body);
            take_jumps(f, t, JUMP_CONTINUE, &body);
            da_state_t again, o;
            cond(f, kid(s, 1), &body, &again, &o);
            take_jumps(f, t, JUMP_BREAK, &o);
            pop_target(f, t);
            *end = again;
            *out = o;
            return;
        }

        case AST_FOR_STMT: {
            ast_node_t *condition = kid(s, 1);
            ast_node_t *update = kid(s, 2);
            target_t *t = push_loop(f, s);
            da_state_t body, o;
            if (!condition || condition->type == AST_EMPTY_STMT) {
                body = st_copy(f, in);
                o = st_dead(f);
            } else {
                cond(f, condition, in, &body, &o);
            }
            stmt(f, kid(s, 3), &body);
            take_jumps(f, t, JUMP_CONTINUE, &body);
            if (update && update->type != AST_EMPTY_STMT) {
                expr(f, update, &body);
            }
            take_jumps(f, t, JUMP_BREAK, &o);
            pop_target(f, t);
            *end = body;
            *out = o;
            return;
        }

        default: {      /* the enhanced for statement; its iterable is evaluated before */
            target_t *t = push_loop(f, s);
            da_state_t body = st_copy(f, in);
            stmt(f, kid(s, 3), &body);
            take_jumps(f, t, JUMP_CONTINUE, &body);
            da_state_t o = st_copy(f, in);
            take_jumps(f, t, JUMP_BREAK, &o);
            pop_target(f, t);
            *end = body;
            *out = o;
            return;
        }
    }
}

/* Is there a blank final variable in scope: one a loop may assign more than once */
static bool blank_final_in_scope(const flow_t *f)
{
    for (size_t i = 0; i < f->name_count; i++) {
        int id = f->names[i].id;
        if (f->is_final[id] && !f->has_init[id]) {
            return true;
        }
    }
    return false;
}

/*
 * A loop (JLS 16.2.10 to 16.2.12). What an iteration assigns may have been assigned already when
 * the next one starts, which matters to a blank final variable: it is analysed a first time, to
 * find what an iteration assigns (reporting nothing), and again with that taken as assigned
 * possibly before the loop.
 */
static void loop_statement(flow_t *f, ast_node_t *s, da_state_t *st)
{
    size_t name_mark = f->name_count;
    target_t *labelled = f->pending_labelled;
    if (s->type == AST_FOR_STMT) {
        ast_node_t *init = kid(s, 0);
        if (init && init->type != AST_EMPTY_STMT) {
            if (init->type == AST_VAR_DECL) {
                stmt(f, init, st);
            } else {
                expr(f, init, st);
            }
        }
    } else if (s->type == AST_ENHANCED_FOR_STMT) {
        expr(f, kid(s, 2), st);
    }
    f->pending_labelled = labelled;

    da_state_t entry = st_copy(f, st);
    da_state_t out, end;
    if (blank_final_in_scope(f)) {
        jump_t *jump_mark = f->jumps;
        size_t scope_mark = f->name_count;
        int count_mark = f->count;
        f->quiet++;
        loop_pass(f, s, &entry, &out, &end);
        f->quiet--;
        f->jumps = jump_mark;
        f->name_count = scope_mark;
        f->count = count_mark;
        f->pending_labelled = labelled;

        uint64_t *outer_induced = f->induced;
        f->induced = flow_alloc(f, f->words * sizeof(uint64_t));
        memcpy(f->induced, outer_induced, f->words * sizeof(uint64_t));
        if (!end.dead && !entry.dead) {
            for (size_t i = 0; i < f->words; i++) {
                uint64_t later = end.bits[f->words + i];
                f->induced[i] |= later & ~entry.bits[f->words + i];
                entry.bits[f->words + i] |= later;
            }
        }
        loop_pass(f, s, &entry, &out, &end);
        f->induced = outer_induced;
    } else {
        loop_pass(f, s, &entry, &out, &end);
    }
    f->name_count = name_mark;
    *st = out;
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
                bool is_final = (s->data.node.flags & MOD_FINAL) != 0;
                if (init) {
                    expr(f, init, st);
                    if (f->group_stmt == s || is_final) {
                        int id = declare(f, decl, is_final, true);
                        if (id >= 0) {
                            st_set(f, st, id);
                        }
                    }
                } else {
                    int id = declare(f, decl, is_final, false);
                    if (id >= 0) {
                        st_clear(f, st, id);
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

        case AST_WHILE_STMT:
        case AST_DO_STMT:
        case AST_FOR_STMT:
        case AST_ENHANCED_FOR_STMT:
            loop_statement(f, s, st);
            return;

        case AST_SWITCH_STMT:
            switch_statement(f, s, st);
            return;

        case AST_RETURN_STMT:
        case AST_THROW_STMT:
            expr(f, kid(s, 0), st);
            if (s->type == AST_RETURN_STMT && f->record_returns && !st->dead) {
                jump_t *r = flow_alloc(f, sizeof(jump_t));
                r->state = st_copy(f, st);
                r->next = f->returns;
                f->returns = r;
            }
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
            if (!f->no_nested) {
                walk_declarations(f->sem, s, snapshot(f, st));
            }
            return;

        default:
            expr(f, s, st);
            return;
    }
}

/* ========================================================================
 * Blank final fields (JLS 16.8, 16.9)
 * ======================================================================== */

/*
 * The blank final fields of a class ("final int x;") must be assigned by its initializers and
 * constructors: every constructor that does not begin with this(...) must leave each of them
 * assigned, wherever it returns; a class with no constructor must have them assigned by its
 * initializers. "x = v" and "this.x = v" are taken to assign the field whatever else may be named
 * x, so a missed error is possible but never a wrong one. Static blank finals likewise, by the
 * static initializers. The code is followed again here, which reports nothing of its own: the
 * analysis of each body has done that.
 */
static void check_blank_finals(semantic_t *sem, ast_node_t *cls, bool want_static)
{
    bool is_record = cls->type == AST_RECORD_DECL;
    int nfields = 0;
    for (slist_t *c = kids(cls); c; c = c->next) {
        ast_node_t *member = (ast_node_t *)c->data;
        if (is_record && !want_static && member && member->type == AST_PARAMETER &&
            (member->data.node.flags & MOD_RECORD_COMPONENT)) {
            nfields++;
        }
        if (member && member->type == AST_FIELD_DECL && (member->data.node.flags & MOD_FINAL) &&
            ((member->data.node.flags & MOD_STATIC) != 0) == want_static) {
            for (slist_t *d = kids(member); d; d = d->next) {
                ast_node_t *decl = (ast_node_t *)d->data;
                if (decl && decl->type == AST_VAR_DECLARATOR && !kids(decl)) {
                    nfields++;
                }
            }
        }
    }
    if (nfields == 0) {
        return;
    }

    flow_t f;
    memset(&f, 0, sizeof(f));
    f.sem = sem;
    f.quiet = 1;
    f.no_nested = true;
    f.record_fields = is_record && !want_static;
    int capacity = count_blank_declarators(cls) + nfields + 1;
    f.capacity = capacity;
    f.words = ((size_t)capacity + 63) / 64;
    f.decls = flow_alloc(&f, (size_t)capacity * sizeof(ast_node_t *));
    f.is_final = flow_alloc(&f, (size_t)capacity * sizeof(bool));
    f.has_init = flow_alloc(&f, (size_t)capacity * sizeof(bool));
    f.is_field = flow_alloc(&f, (size_t)capacity * sizeof(bool));
    f.induced = flow_alloc(&f, f.words * sizeof(uint64_t));

    int *ids = flow_alloc(&f, (size_t)nfields * sizeof(int));
    int n = 0;
    for (slist_t *c = kids(cls); c; c = c->next) {
        ast_node_t *member = (ast_node_t *)c->data;
        if (f.record_fields && member && member->type == AST_PARAMETER &&
            (member->data.node.flags & MOD_RECORD_COMPONENT)) {
            ids[n++] = declare_field(&f, member);
        }
        if (member && member->type == AST_FIELD_DECL && (member->data.node.flags & MOD_FINAL) &&
            ((member->data.node.flags & MOD_STATIC) != 0) == want_static) {
            for (slist_t *d = kids(member); d; d = d->next) {
                ast_node_t *decl = (ast_node_t *)d->data;
                if (decl && decl->type == AST_VAR_DECLARATOR && !kids(decl)) {
                    ids[n++] = declare_field(&f, decl);
                }
            }
        }
    }

    /* the initializers, in the order they run */
    da_state_t st = st_new(&f);
    for (slist_t *c = kids(cls); c; c = c->next) {
        ast_node_t *member = (ast_node_t *)c->data;
        if (!member) {
            continue;
        }
        bool is_static = (member->data.node.flags & MOD_STATIC) != 0;
        if (member->type == AST_FIELD_DECL && is_static == want_static) {
            for (slist_t *d = kids(member); d; d = d->next) {
                ast_node_t *decl = (ast_node_t *)d->data;
                if (decl && decl->type == AST_VAR_DECLARATOR && kid(decl, 0)) {
                    expr(&f, kid(decl, 0), &st);
                }
            }
        } else if (member->type == AST_INITIALIZER_BLOCK && is_static == want_static) {
            stmt(&f, kid(member, 0), &st);
        }
    }

    if (want_static) {
        for (int i = 0; i < n; i++) {
            if (ids[i] >= 0 && !st_has(&st, ids[i])) {
                semantic_error(sem, f.decls[ids[i]]->line, f.decls[ids[i]]->column,
                               "variable %s might not have been initialized", f.decls[ids[i]]->data.node.name);
            }
        }
        flow_free(&f);
        return;
    }

    bool any_constructor = false;
    for (slist_t *c = kids(cls); c; c = c->next) {
        ast_node_t *ctor = (ast_node_t *)c->data;
        if (!ctor || ctor->type != AST_CONSTRUCTOR_DECL) {
            continue;
        }
        any_constructor = true;
        ast_node_t *body = NULL;
        for (slist_t *k = kids(ctor); k; k = k->next) {
            if (k->data && ((ast_node_t *)k->data)->type == AST_BLOCK) {
                body = (ast_node_t *)k->data;
            }
        }
        if (!body) {
            continue;
        }
        /* this(...), which since Java 25 need not come first */
        bool delegates = false;
        for (slist_t *k = kids(body); k; k = k->next) {
            ast_node_t *statement = (ast_node_t *)k->data;
            if (statement && statement->type == AST_EXPR_STMT) {
                statement = kid(statement, 0);
            }
            if (statement && statement->type == AST_EXPLICIT_CTOR_CALL && statement->data.node.name &&
                strcmp(statement->data.node.name, "this") == 0) {
                delegates = true;
            }
        }
        if (delegates) {
            continue;       /* another constructor of the class assigns them */
        }

        da_state_t end = st_copy(&f, &st);
        f.returns = NULL;
        f.record_returns = true;
        size_t name_mark = f.name_count;
        int count_mark = f.count;
        stmt(&f, body, &end);
        f.record_returns = false;
        f.name_count = name_mark;
        f.count = count_mark;
        for (int i = 0; i < n; i++) {
            if (ids[i] < 0) {
                continue;
            }
            bool assigned = st_has(&end, ids[i]);
            for (jump_t *r = f.returns; assigned && r; r = r->next) {
                assigned = st_has(&r->state, ids[i]);
            }
            if (!assigned) {
                semantic_error(sem, ctor->line, ctor->column,
                               "variable %s might not have been initialized", f.decls[ids[i]]->data.node.name);
            }
        }
    }
    if (!any_constructor && !is_record) {   /* a record's implicit constructor assigns them */
        for (int i = 0; i < n; i++) {
            if (ids[i] >= 0 && !st_has(&st, ids[i])) {
                semantic_error(sem, cls->line, cls->column,
                               "variable %s not initialized in the default constructor",
                               f.decls[ids[i]]->data.node.name);
            }
        }
    }
    flow_free(&f);
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
static void analyze_body(semantic_t *sem, ast_node_t *body, const captures_t *captures)
{
    int blanks = count_blank_declarators(body);
    if (blanks == 0 && !captures) {
        /* nothing to track, but a class declared in it may declare variables of its own */
        for (slist_t *c = kids(body); c; c = c->next) {
            if (c->data) {
                walk_declarations(sem, (ast_node_t *)c->data, NULL);
            }
        }
        return;
    }
    flow_t f;
    memset(&f, 0, sizeof(f));
    f.sem = sem;
    f.captures = captures;
    f.capacity = blanks;
    f.decls = flow_alloc(&f, ((size_t)blanks + 1) * sizeof(ast_node_t *));
    f.is_final = flow_alloc(&f, ((size_t)blanks + 1) * sizeof(bool));
    f.has_init = flow_alloc(&f, ((size_t)blanks + 1) * sizeof(bool));
    f.is_field = flow_alloc(&f, ((size_t)blanks + 1) * sizeof(bool));
    f.words = ((size_t)blanks + 63) / 64;
    if (f.words == 0) {
        f.words = 1;
    }
    f.induced = flow_alloc(&f, f.words * sizeof(uint64_t));
    da_state_t st = st_new(&f);
    if (body->type == AST_BLOCK) {
        stmt(&f, body, &st);
    } else {
        expr(&f, body, &st);
    }
    flow_free(&f);
}

static void walk_declarations(semantic_t *sem, ast_node_t *node, const captures_t *captures)
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
                    analyze_body(sem, child, captures);
                }
            }
            return;

        case AST_FIELD_DECL:
            for (slist_t *c = kids(node); c; c = c->next) {
                ast_node_t *decl = (ast_node_t *)c->data;
                if (decl && decl->type == AST_VAR_DECLARATOR && kid(decl, 0)) {
                    analyze_body(sem, kid(decl, 0), captures);
                }
            }
            return;

        case AST_ENUM_CONSTANT:
            /* with a class body, whose members are the constant's own children */
            check_blank_finals(sem, node, false);
            /* fall through to the members */
        default:
            if (is_type_node(node)) {
                return;
            }
            for (slist_t *c = kids(node); c; c = c->next) {
                if (c->data) {
                    walk_declarations(sem, (ast_node_t *)c->data, captures);
                }
            }
            return;

        case AST_CLASS_DECL:
        case AST_ENUM_DECL:
        case AST_RECORD_DECL:
            check_blank_finals(sem, node, false);
            check_blank_finals(sem, node, true);
            for (slist_t *c = kids(node); c; c = c->next) {
                if (c->data) {
                    walk_declarations(sem, (ast_node_t *)c->data, captures);
                }
            }
            return;
    }
}

void flow_check_definite_assignment(semantic_t *sem, ast_node_t *unit)
{
    walk_declarations(sem, unit, NULL);
}
