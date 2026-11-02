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
	flow.c \
	flow.h \
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

genesis_CPPFLAGS = -I$(srcdir) -I.
genesis_CFLAGS = $(WARN_CFLAGS)

# commit.h records the source revision for "genesis --version", so a failure
# report says which build produced it. It is regenerated on every make but
# only rewritten when the revision changes, so genesis.c is not recompiled
# needlessly. Outside a git checkout (a release tarball) it says "unknown".
BUILT_SOURCES = commit.h
CLEANFILES = commit.h commit.h.tmp

commit.h: FORCE
	$(AM_V_GEN)rev=`git -C '$(srcdir)' describe --always --dirty 2>/dev/null` || rev=unknown; \
	echo "#define GENESIS_COMMIT \"$$rev\"" > commit.h.tmp; \
	if cmp -s commit.h.tmp commit.h; then rm -f commit.h.tmp; \
	else mv -f commit.h.tmp commit.h; fi

FORCE:
.PHONY: FORCE
