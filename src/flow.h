/*
 * flow.h - definite assignment analysis (JLS 16)
 *
 * Copyright (C) 2020, 2026 Chris Burdess <dog@gnu.org>
 *
 * This file is part of genesis; see COPYING for the license terms.
 */

#ifndef GENESIS_FLOW_H
#define GENESIS_FLOW_H

#include "genesis.h"

/**
 * Check that every local variable declared without an initializer is definitely assigned
 * wherever it is read (JLS 16), reporting "variable x might not have been initialized" for each
 * read that is not. Runs over a whole compilation unit after semantic analysis, which it leaves
 * unchanged apart from the errors it reports.
 */
void flow_check_definite_assignment(semantic_t *sem, ast_node_t *unit);

#endif /* GENESIS_FLOW_H */
