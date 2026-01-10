/*
 * indy.c
 * Invokedynamic infrastructure for the genesis Java compiler
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include <stdlib.h>
#include <string.h>
#include "indy.h"

/* ========================================================================
 * Constant Pool Extensions for invokedynamic
 * ======================================================================== */

/**
 * Add a raw entry to the constant pool.
 * Internal helper for adding entries that need manual setup.
 */
static uint16_t cp_add_raw_entry(const_pool_t *cp)
{
    if (!cp) {
        return 0;
    }
    
    /* Grow if needed */
    if (cp->count >= cp->capacity) {
        uint16_t new_cap = cp->capacity * 2;
        const_pool_entry_t *new_entries = realloc(cp->entries,
                                                   new_cap * sizeof(const_pool_entry_t));
        if (!new_entries) {
            return 0;
        }
        cp->entries = new_entries;
        cp->capacity = new_cap;
    }
    
    return cp->count++;
}

uint16_t cp_add_method_handle(const_pool_t *cp, method_handle_kind_t reference_kind,
                               uint16_t reference_index)
{
    if (!cp || reference_kind < REF_getField || reference_kind > REF_invokeInterface) {
        return 0;
    }
    
    uint16_t index = cp_add_raw_entry(cp);
    if (index == 0) {
        return 0;
    }
    
    const_pool_entry_t *entry = &cp->entries[index];
    entry->type = CONST_METHOD_HANDLE;
    /* Store reference_kind in high byte, reference_index uses ref.class_index */
    entry->data.ref.class_index = (uint16_t)reference_kind;
    entry->data.ref.name_type_index = reference_index;
    
    return index;
}

uint16_t cp_add_method_type(const_pool_t *cp, const char *descriptor)
{
    if (!cp || !descriptor) {
        return 0;
    }
    
    /* Add UTF8 entry for descriptor */
    uint16_t desc_index = cp_add_utf8(cp, descriptor);
    if (desc_index == 0) {
        return 0;
    }
    
    uint16_t index = cp_add_raw_entry(cp);
    if (index == 0) {
        return 0;
    }
    
    const_pool_entry_t *entry = &cp->entries[index];
    entry->type = CONST_METHOD_TYPE;
    entry->data.class_index = desc_index;  /* descriptor_index */
    
    return index;
}

uint16_t cp_add_invoke_dynamic(const_pool_t *cp, uint16_t bootstrap_method_idx,
                                uint16_t name_and_type_idx)
{
    if (!cp) {
        return 0;
    }
    
    uint16_t index = cp_add_raw_entry(cp);
    if (index == 0) {
        return 0;
    }
    
    const_pool_entry_t *entry = &cp->entries[index];
    entry->type = CONST_INVOKE_DYNAMIC;
    entry->data.ref.class_index = bootstrap_method_idx;
    entry->data.ref.name_type_index = name_and_type_idx;
    
    return index;
}

/* ========================================================================
 * Bootstrap Methods Management
 * ======================================================================== */

#define BSM_INITIAL_CAPACITY 8

bootstrap_methods_t *bootstrap_methods_new(void)
{
    bootstrap_methods_t *bsm = calloc(1, sizeof(bootstrap_methods_t));
    if (!bsm) {
        return NULL;
    }
    
    bsm->methods = calloc(BSM_INITIAL_CAPACITY, sizeof(bootstrap_method_t));
    if (!bsm->methods) {
        free(bsm);
        return NULL;
    }
    
    bsm->capacity = BSM_INITIAL_CAPACITY;
    bsm->count = 0;
    
    return bsm;
}

void bootstrap_methods_free(bootstrap_methods_t *bsm)
{
    if (!bsm) {
        return;
    }
    
    for (uint16_t i = 0; i < bsm->count; i++) {
        free(bsm->methods[i].arguments);
    }
    free(bsm->methods);
    free(bsm);
}

int bootstrap_methods_add(bootstrap_methods_t *bsm, uint16_t method_handle,
                          uint16_t *arguments, uint16_t num_arguments)
{
    if (!bsm) {
        return -1;
    }
    
    /* Grow if needed */
    if (bsm->count >= bsm->capacity) {
        uint16_t new_cap = bsm->capacity * 2;
        bootstrap_method_t *new_methods = realloc(bsm->methods,
                                                   new_cap * sizeof(bootstrap_method_t));
        if (!new_methods) {
            return -1;
        }
        bsm->methods = new_methods;
        bsm->capacity = new_cap;
    }
    
    bootstrap_method_t *entry = &bsm->methods[bsm->count];
    entry->method_handle_index = method_handle;
    entry->num_arguments = num_arguments;
    
    if (num_arguments > 0 && arguments) {
        entry->arguments = malloc(num_arguments * sizeof(uint16_t));
        if (!entry->arguments) {
            return -1;
        }
        memcpy(entry->arguments, arguments, num_arguments * sizeof(uint16_t));
    } else {
        entry->arguments = NULL;
    }
    
    return bsm->count++;
}

/* ========================================================================
 * Lambda Metafactory Support
 * ======================================================================== */

/* LambdaMetafactory class and method names */
#define LAMBDA_METAFACTORY_CLASS "java/lang/invoke/LambdaMetafactory"
#define LAMBDA_METAFACTORY_METHOD "metafactory"
#define LAMBDA_METAFACTORY_DESC \
    "(Ljava/lang/invoke/MethodHandles$Lookup;" \
    "Ljava/lang/String;" \
    "Ljava/lang/invoke/MethodType;" \
    "Ljava/lang/invoke/MethodType;" \
    "Ljava/lang/invoke/MethodHandle;" \
    "Ljava/lang/invoke/MethodType;)" \
    "Ljava/lang/invoke/CallSite;"

#define ALT_METAFACTORY_METHOD "altMetafactory"
#define ALT_METAFACTORY_DESC \
    "(Ljava/lang/invoke/MethodHandles$Lookup;" \
    "Ljava/lang/String;" \
    "Ljava/lang/invoke/MethodType;" \
    "[Ljava/lang/Object;)" \
    "Ljava/lang/invoke/CallSite;"

int indy_add_lambda_metafactory(const_pool_t *cp, bootstrap_methods_t *bsm)
{
    if (!cp || !bsm) {
        return -1;
    }
    
    /* Add method reference for LambdaMetafactory.metafactory */
    uint16_t method_ref = cp_add_methodref(cp, LAMBDA_METAFACTORY_CLASS,
                                            LAMBDA_METAFACTORY_METHOD,
                                            LAMBDA_METAFACTORY_DESC);
    if (method_ref == 0) {
        return -1;
    }
    
    /* Create method handle pointing to the metafactory method */
    uint16_t method_handle = cp_add_method_handle(cp, REF_invokeStatic, method_ref);
    if (method_handle == 0) {
        return -1;
    }
    
    /* Add bootstrap method entry (no static arguments for the BSM itself) */
    return bootstrap_methods_add(bsm, method_handle, NULL, 0);
}

int indy_add_alt_metafactory(const_pool_t *cp, bootstrap_methods_t *bsm)
{
    if (!cp || !bsm) {
        return -1;
    }
    
    /* Add method reference for LambdaMetafactory.altMetafactory */
    uint16_t method_ref = cp_add_methodref(cp, LAMBDA_METAFACTORY_CLASS,
                                            ALT_METAFACTORY_METHOD,
                                            ALT_METAFACTORY_DESC);
    if (method_ref == 0) {
        return -1;
    }
    
    /* Create method handle pointing to the altMetafactory method */
    uint16_t method_handle = cp_add_method_handle(cp, REF_invokeStatic, method_ref);
    if (method_handle == 0) {
        return -1;
    }
    
    /* Add bootstrap method entry */
    return bootstrap_methods_add(bsm, method_handle, NULL, 0);
}

/* ========================================================================
 * Lambda Compilation
 * ======================================================================== */

lambda_info_t *lambda_info_new(const char *func_interface, const char *sam_name,
                                const char *sam_descriptor)
{
    if (!func_interface || !sam_name || !sam_descriptor) {
        return NULL;
    }
    
    lambda_info_t *info = calloc(1, sizeof(lambda_info_t));
    if (!info) {
        return NULL;
    }
    
    info->functional_interface = strdup(func_interface);
    info->sam_name = strdup(sam_name);
    info->sam_descriptor = strdup(sam_descriptor);
    
    if (!info->functional_interface || !info->sam_name || !info->sam_descriptor) {
        lambda_info_free(info);
        return NULL;
    }
    
    return info;
}

void lambda_info_free(lambda_info_t *info)
{
    if (!info) {
        return;
    }
    
    free(info->functional_interface);
    free(info->sam_name);
    free(info->sam_descriptor);
    free(info->impl_method_name);
    free(info->impl_method_descriptor);
    
    /* Free captures */
    slist_t *node = info->captures;
    while (node) {
        lambda_capture_t *cap = (lambda_capture_t *)node->data;
        if (cap) {
            free(cap->name);
            free(cap->descriptor);
            free(cap);
        }
        node = node->next;
    }
    slist_free(info->captures);
    
    free(info);
}

void lambda_info_add_capture(lambda_info_t *info, const char *name,
                              const char *descriptor, uint16_t slot, bool is_this)
{
    if (!info || !name || !descriptor) {
        return;
    }
    
    lambda_capture_t *cap = calloc(1, sizeof(lambda_capture_t));
    if (!cap) {
        return;
    }
    
    cap->name = strdup(name);
    cap->descriptor = strdup(descriptor);
    cap->local_slot = slot;
    cap->is_this = is_this;
    
    if (!cap->name || !cap->descriptor) {
        free(cap->name);
        free(cap->descriptor);
        free(cap);
        return;
    }
    
    if (is_this) {
        info->is_instance_capture = true;
    }
    
    if (!info->captures) {
        info->captures = slist_new(cap);
    } else {
        slist_append(info->captures, cap);
    }
}

uint16_t indy_generate_lambda_callsite(const_pool_t *cp, bootstrap_methods_t *bsm,
                                        lambda_info_t *info, int metafactory_idx)
{
    if (!cp || !bsm || !info || metafactory_idx < 0) {
        return 0;
    }
    
    /* Build the invokedynamic name_and_type entry
     * Name: the SAM method name (e.g., "apply", "accept", "run")
     * Descriptor: captures -> functional interface type
     *   e.g., "(Ljava/lang/String;)Ljava/util/function/Function;"
     */
    
    /* Build invocation type descriptor: (captures)L<functional_interface>; */
    size_t desc_len = 1024;  /* Should be enough */
    char *invoke_desc = malloc(desc_len);
    if (!invoke_desc) {
        return 0;
    }
    
    char *p = invoke_desc;
    *p++ = '(';
    
    /* Add capture types */
    for (slist_t *node = info->captures; node; node = node->next) {
        lambda_capture_t *cap = (lambda_capture_t *)node->data;
        size_t cap_len = strlen(cap->descriptor);
        memcpy(p, cap->descriptor, cap_len);
        p += cap_len;
    }
    
    *p++ = ')';
    *p++ = 'L';
    
    /* Add functional interface return type */
    size_t fi_len = strlen(info->functional_interface);
    memcpy(p, info->functional_interface, fi_len);
    p += fi_len;
    *p++ = ';';
    *p = '\0';
    
    /* Create name_and_type entry */
    uint16_t nat_index = cp_add_name_and_type(cp, info->sam_name, invoke_desc);
    free(invoke_desc);
    if (nat_index == 0) {
        return 0;
    }
    
    /* Bootstrap method arguments for LambdaMetafactory.metafactory:
     * 1. MethodType - erased SAM descriptor (e.g., "(Ljava/lang/Object;)Ljava/lang/Object;")
     * 2. MethodHandle - implementation method handle
     * 3. MethodType - specialized SAM descriptor
     */
    
    /* For now, create placeholder entries - actual implementation will compute these */
    uint16_t erased_type = cp_add_method_type(cp, info->sam_descriptor);
    if (erased_type == 0) {
        return 0;
    }
    
    /* Implementation method handle - points to lambda$N method
     * For instance lambdas: REF_invokeSpecial
     * For static lambdas: REF_invokeStatic */
    /* TODO: compute impl_method_ref from info->impl_method_name etc. */
    /* For stub, use placeholder */
    uint16_t impl_handle = 0;  /* Placeholder - needs actual implementation */
    (void)impl_handle;  /* Suppress unused warning until fully implemented */
    
    uint16_t specialized_type = cp_add_method_type(cp, info->sam_descriptor);
    if (specialized_type == 0) {
        return 0;
    }
    
    /* Add bootstrap method with arguments */
    uint16_t bsm_args[3] = { erased_type, impl_handle, specialized_type };
    int bsm_idx = bootstrap_methods_add(bsm, 
                                         bsm->methods[metafactory_idx].method_handle_index,
                                         bsm_args, 3);
    if (bsm_idx < 0) {
        return 0;
    }
    
    /* Create InvokeDynamic entry */
    return cp_add_invoke_dynamic(cp, (uint16_t)bsm_idx, nat_index);
}

/* ========================================================================
 * Method Reference Compilation
 * ======================================================================== */

method_ref_info_t *method_ref_info_new(method_ref_kind_t kind,
                                        const char *target_class,
                                        const char *method_name,
                                        const char *method_desc,
                                        const char *func_interface,
                                        const char *sam_name,
                                        const char *sam_descriptor)
{
    if (!target_class || !method_name || !method_desc ||
        !func_interface || !sam_name || !sam_descriptor) {
        return NULL;
    }
    
    method_ref_info_t *info = calloc(1, sizeof(method_ref_info_t));
    if (!info) {
        return NULL;
    }
    
    info->kind = kind;
    info->target_class = strdup(target_class);
    info->method_name = strdup(method_name);
    info->method_descriptor = strdup(method_desc);
    info->functional_interface = strdup(func_interface);
    info->sam_name = strdup(sam_name);
    info->sam_descriptor = strdup(sam_descriptor);
    
    if (!info->target_class || !info->method_name || !info->method_descriptor ||
        !info->functional_interface || !info->sam_name || !info->sam_descriptor) {
        method_ref_info_free(info);
        return NULL;
    }
    
    return info;
}

void method_ref_info_free(method_ref_info_t *info)
{
    if (!info) {
        return;
    }
    
    free(info->target_class);
    free(info->method_name);
    free(info->method_descriptor);
    free(info->functional_interface);
    free(info->sam_name);
    free(info->sam_descriptor);
    free(info);
}

uint16_t indy_generate_method_ref_callsite(const_pool_t *cp, bootstrap_methods_t *bsm,
                                            method_ref_info_t *info, int metafactory_idx)
{
    if (!cp || !bsm || !info || metafactory_idx < 0) {
        return 0;
    }
    
    /* Determine the method handle kind based on reference type */
    method_handle_kind_t handle_kind;
    switch (info->kind) {
        case MREF_STATIC:
            handle_kind = REF_invokeStatic;
            break;
        case MREF_BOUND:
        case MREF_UNBOUND:
            handle_kind = REF_invokeVirtual;
            break;
        case MREF_CONSTRUCTOR:
        case MREF_ARRAY_CONSTRUCTOR:
            handle_kind = REF_newInvokeSpecial;
            break;
        default:
            return 0;
    }
    
    /* Create method reference for target method */
    uint16_t method_ref = cp_add_methodref(cp, info->target_class,
                                            info->method_name,
                                            info->method_descriptor);
    if (method_ref == 0) {
        return 0;
    }
    
    /* Create method handle for implementation */
    uint16_t impl_handle = cp_add_method_handle(cp, handle_kind, method_ref);
    if (impl_handle == 0) {
        return 0;
    }
    
    /* Build invocation type descriptor */
    char invoke_desc[512];
    if (info->kind == MREF_BOUND) {
        /* Bound method reference: receiver is captured
         * Descriptor: (LTargetClass;)LFunctionalInterface; */
        snprintf(invoke_desc, sizeof(invoke_desc), "(L%s;)L%s;",
                 info->target_class, info->functional_interface);
    } else {
        /* Static or unbound: no captures
         * Descriptor: ()LFunctionalInterface; */
        snprintf(invoke_desc, sizeof(invoke_desc), "()L%s;",
                 info->functional_interface);
    }
    
    /* Create name_and_type entry */
    uint16_t nat_index = cp_add_name_and_type(cp, info->sam_name, invoke_desc);
    if (nat_index == 0) {
        return 0;
    }
    
    /* Bootstrap arguments */
    uint16_t erased_type = cp_add_method_type(cp, info->sam_descriptor);
    uint16_t specialized_type = cp_add_method_type(cp, info->sam_descriptor);
    if (erased_type == 0 || specialized_type == 0) {
        return 0;
    }
    
    uint16_t bsm_args[3] = { erased_type, impl_handle, specialized_type };
    int bsm_idx = bootstrap_methods_add(bsm,
                                         bsm->methods[metafactory_idx].method_handle_index,
                                         bsm_args, 3);
    if (bsm_idx < 0) {
        return 0;
    }
    
    /* Create InvokeDynamic entry */
    return cp_add_invoke_dynamic(cp, (uint16_t)bsm_idx, nat_index);
}

