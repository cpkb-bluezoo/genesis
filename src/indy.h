/*
 * indy.h
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

#ifndef INDY_H
#define INDY_H

#include <stdint.h>
#include <stdbool.h>
#include "util.h"
#include "constpool.h"

/* ========================================================================
 * Method Handle Reference Kinds (JVM Spec §5.4.3.5)
 * ======================================================================== */

typedef enum method_handle_kind
{
    REF_getField         = 1,   /* getfield C.f:T */
    REF_getStatic        = 2,   /* getstatic C.f:T */
    REF_putField         = 3,   /* putfield C.f:T */
    REF_putStatic        = 4,   /* putstatic C.f:T */
    REF_invokeVirtual    = 5,   /* invokevirtual C.m:(A*)T */
    REF_invokeStatic     = 6,   /* invokestatic C.m:(A*)T */
    REF_invokeSpecial    = 7,   /* invokespecial C.m:(A*)T */
    REF_newInvokeSpecial = 8,   /* new C; dup; invokespecial C.<init>:(A*)V */
    REF_invokeInterface  = 9    /* invokeinterface C.m:(A*)T */
} method_handle_kind_t;

/* ========================================================================
 * Bootstrap Method Entry (for BootstrapMethods attribute)
 * ======================================================================== */

/**
 * Bootstrap method entry for the BootstrapMethods class attribute.
 * Each invokedynamic call site references one of these.
 */
typedef struct bootstrap_method
{
    uint16_t method_handle_index;   /* CP index of CONSTANT_MethodHandle */
    uint16_t *arguments;            /* Array of CP indices for bootstrap args */
    uint16_t num_arguments;         /* Number of bootstrap arguments */
} bootstrap_method_t;

/**
 * Bootstrap methods table (class-level).
 * Stored as a class attribute in the class file.
 */
typedef struct bootstrap_methods
{
    bootstrap_method_t *methods;    /* Array of bootstrap method entries */
    uint16_t count;                 /* Number of entries */
    uint16_t capacity;              /* Allocated capacity */
} bootstrap_methods_t;

/* ========================================================================
 * Lambda Capture Info
 * ======================================================================== */

/**
 * Represents a captured variable for lambda/method reference desugaring.
 */
typedef struct lambda_capture
{
    char *name;                     /* Variable name */
    char *descriptor;               /* Type descriptor */
    uint16_t local_slot;            /* Local variable slot (for local captures) */
    bool is_this;                   /* True if capturing 'this' */
} lambda_capture_t;

/**
 * Lambda expression compilation info.
 * Tracks all information needed to generate invokedynamic call site.
 */
typedef struct lambda_info
{
    char *functional_interface;     /* Target functional interface name */
    char *sam_name;                 /* Single abstract method name */
    char *sam_descriptor;           /* SAM method descriptor */
    char *impl_method_name;         /* Generated lambda$N method name */
    char *impl_method_descriptor;   /* Implementation method descriptor */
    bool is_instance_capture;       /* True if captures 'this' */
    slist_t *captures;              /* List of lambda_capture_t* */
} lambda_info_t;

/* ========================================================================
 * Constant Pool Extensions for invokedynamic
 * ======================================================================== */

/**
 * Add a MethodHandle entry to the constant pool.
 *
 * @param cp             Constant pool
 * @param reference_kind Method handle kind (REF_*)
 * @param reference_idx  CP index of referenced field/method
 * @return               Constant pool index, or 0 on error
 */
uint16_t cp_add_method_handle(const_pool_t *cp, method_handle_kind_t reference_kind,
                               uint16_t reference_index);

/**
 * Add a MethodType entry to the constant pool.
 *
 * @param cp         Constant pool
 * @param descriptor Method type descriptor (e.g., "(II)I")
 * @return           Constant pool index, or 0 on error
 */
uint16_t cp_add_method_type(const_pool_t *cp, const char *descriptor);

/**
 * Add an InvokeDynamic entry to the constant pool.
 *
 * @param cp                    Constant pool
 * @param bootstrap_method_idx  Index into BootstrapMethods attribute
 * @param name_and_type_idx     CP index of CONSTANT_NameAndType
 * @return                      Constant pool index, or 0 on error
 */
uint16_t cp_add_invoke_dynamic(const_pool_t *cp, uint16_t bootstrap_method_idx,
                                uint16_t name_and_type_idx);

/* ========================================================================
 * Bootstrap Methods Management
 * ======================================================================== */

/**
 * Create a new bootstrap methods table.
 *
 * @return New bootstrap methods table, or NULL on error
 */
bootstrap_methods_t *bootstrap_methods_new(void);

/**
 * Free a bootstrap methods table.
 *
 * @param bsm Bootstrap methods table to free
 */
void bootstrap_methods_free(bootstrap_methods_t *bsm);

/**
 * Add a bootstrap method entry.
 *
 * @param bsm           Bootstrap methods table
 * @param method_handle CP index of the bootstrap method handle
 * @param arguments     Array of CP indices for bootstrap arguments
 * @param num_arguments Number of bootstrap arguments
 * @return              Index of the new entry, or -1 on error
 */
int bootstrap_methods_add(bootstrap_methods_t *bsm, uint16_t method_handle,
                          uint16_t *arguments, uint16_t num_arguments);

/* ========================================================================
 * Lambda Metafactory Support
 * ======================================================================== */

/**
 * Standard bootstrap method for lambda expressions.
 * Corresponds to java.lang.invoke.LambdaMetafactory.metafactory().
 *
 * Arguments:
 *   1. MethodType - erased functional interface method type
 *   2. MethodHandle - implementation method handle
 *   3. MethodType - enforced functional interface method type
 *
 * @param cp              Constant pool
 * @param bsm             Bootstrap methods table
 * @return                Bootstrap method index, or -1 on error
 */
int indy_add_lambda_metafactory(const_pool_t *cp, bootstrap_methods_t *bsm);

/**
 * Alternative bootstrap method for serializable lambdas.
 * Corresponds to java.lang.invoke.LambdaMetafactory.altMetafactory().
 *
 * @param cp              Constant pool
 * @param bsm             Bootstrap methods table
 * @return                Bootstrap method index, or -1 on error
 */
int indy_add_alt_metafactory(const_pool_t *cp, bootstrap_methods_t *bsm);

/* ========================================================================
 * Lambda Compilation
 * ======================================================================== */

/**
 * Create lambda info structure for tracking lambda compilation.
 *
 * @param func_interface  Functional interface name (internal form)
 * @param sam_name        Single abstract method name
 * @param sam_descriptor  SAM method type descriptor
 * @return                New lambda_info_t, or NULL on error
 */
lambda_info_t *lambda_info_new(const char *func_interface, const char *sam_name,
                                const char *sam_descriptor);

/**
 * Free a lambda info structure.
 *
 * @param info Lambda info to free
 */
void lambda_info_free(lambda_info_t *info);

/**
 * Add a captured variable to lambda info.
 *
 * @param info       Lambda info
 * @param name       Variable name
 * @param descriptor Type descriptor
 * @param slot       Local variable slot
 * @param is_this    True if capturing 'this'
 */
void lambda_info_add_capture(lambda_info_t *info, const char *name,
                              const char *descriptor, uint16_t slot, bool is_this);

/**
 * Generate the invokedynamic call site for a lambda expression.
 * Returns the CP index of the CONSTANT_InvokeDynamic entry.
 *
 * @param cp              Constant pool
 * @param bsm             Bootstrap methods table
 * @param info            Lambda compilation info
 * @param metafactory_idx Bootstrap method index for LambdaMetafactory
 * @return                CP index for invokedynamic, or 0 on error
 */
uint16_t indy_generate_lambda_callsite(const_pool_t *cp, bootstrap_methods_t *bsm,
                                        lambda_info_t *info, int metafactory_idx);

/* ========================================================================
 * Method Reference Compilation
 * ======================================================================== */

/**
 * Method reference kinds.
 */
typedef enum method_ref_kind
{
    MREF_STATIC,           /* ClassName::staticMethod */
    MREF_BOUND,            /* instance::method (bound receiver) */
    MREF_UNBOUND,          /* ClassName::instanceMethod (unbound) */
    MREF_CONSTRUCTOR,      /* ClassName::new */
    MREF_ARRAY_CONSTRUCTOR /* Type[]::new */
} method_ref_kind_t;

/**
 * Method reference compilation info.
 */
typedef struct method_ref_info
{
    method_ref_kind_t kind;         /* Type of method reference */
    char *target_class;             /* Target class (internal name) */
    char *method_name;              /* Method name (or "<init>" for constructor) */
    char *method_descriptor;        /* Method descriptor */
    char *functional_interface;     /* Target functional interface */
    char *sam_name;                 /* SAM method name */
    char *sam_descriptor;           /* SAM method descriptor */
} method_ref_info_t;

/**
 * Create method reference info.
 *
 * @param kind             Method reference kind
 * @param target_class     Target class (internal name)
 * @param method_name      Method name
 * @param method_desc      Method descriptor
 * @param func_interface   Functional interface name
 * @param sam_name         SAM method name
 * @param sam_descriptor   SAM method descriptor
 * @return                 New method_ref_info_t, or NULL on error
 */
method_ref_info_t *method_ref_info_new(method_ref_kind_t kind,
                                        const char *target_class,
                                        const char *method_name,
                                        const char *method_desc,
                                        const char *func_interface,
                                        const char *sam_name,
                                        const char *sam_descriptor);

/**
 * Free a method reference info structure.
 *
 * @param info Method reference info to free
 */
void method_ref_info_free(method_ref_info_t *info);

/**
 * Generate the invokedynamic call site for a method reference.
 *
 * @param cp              Constant pool
 * @param bsm             Bootstrap methods table
 * @param info            Method reference info
 * @param metafactory_idx Bootstrap method index for LambdaMetafactory
 * @return                CP index for invokedynamic, or 0 on error
 */
uint16_t indy_generate_method_ref_callsite(const_pool_t *cp, bootstrap_methods_t *bsm,
                                            method_ref_info_t *info, int metafactory_idx);

#endif /* INDY_H */

