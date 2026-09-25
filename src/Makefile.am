## src/Makefile.am - Automake input for the genesis compiler
## Copyright (C) 2016, 2020, 2026 Chris Burdess <dog@gnu.org>
##
## This file is part of genesis; see COPYING for the license terms.

bin_PROGRAMS = genesis

genesis_SOURCES = \
	classfile.c \
	classfile.h \
	classpath.c \
	classpath.h \
	classwriter.c \
	classwriter.h \
	codegen.c \
	codegen.h \
	codegen_expr.c \
	codegen_internal.h \
	codegen_stmt.c \
	constpool.c \
	constpool.h \
	encoding.c \
	encoding.h \
	genesis.c \
	genesis.h \
	indy.c \
	indy.h \
	jarwriter.c \
	jarwriter.h \
	lexer.c \
	parser.c \
	semantic.c \
	stackmap.c \
	stackmap.h \
	type.c \
	type.h \
	util.c \
	util.h

genesis_CPPFLAGS = -I$(srcdir)
genesis_CFLAGS = $(WARN_CFLAGS)
