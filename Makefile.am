## Makefile.am - top-level Automake input for genesis
## Copyright (C) 2016, 2020, 2026 Chris Burdess <dog@gnu.org>
##
## This file is part of genesis; see COPYING for the license terms.

ACLOCAL_AMFLAGS = -I m4

SUBDIRS = src

## Only user-facing documentation is distributed.  Everything under test/
## (regression tests, jtreg glue, parity scripts) is development-only and is
## intentionally absent from EXTRA_DIST.
EXTRA_DIST = CONTRIBUTING TODO README

if HAVE_TESTS

## ---------------------------------------------------------------------
## Regression tests (development checkouts only)
## ---------------------------------------------------------------------

TEST_ENV = GENESIS="$(abs_top_builddir)/src/genesis" \
           JAVA="$(JAVA)" \
           TEST_SRC="$(abs_top_srcdir)/test" \
           TEST_BUILD="$(abs_top_builddir)/test/build"

check-local: all
	$(AM_V_at)$(TEST_ENV) $(SHELL) $(top_srcdir)/test/run-tests.sh

## Run one test verbosely: make check-one TEST=HelloWorldTest
check-one: all
	$(AM_V_at)$(TEST_ENV) $(SHELL) $(top_srcdir)/test/run-tests.sh "$(TEST)"

## ---------------------------------------------------------------------
## jtreg integration (OpenJDK compiler tests)
##
## Needs jtreg (JTREG_HOME) and OpenJDK sources (JDK_SRC):
##   git clone --depth 1 https://github.com/openjdk/jdk ../jdk
##   make jtreg-test JTREG_TESTS=lambda
##   make jtreg-test-one TEST=lambda/T8129740
## ---------------------------------------------------------------------

JTREG_HOME ?= $(abs_top_srcdir)/../jtreg/build/images/jtreg
JDK_SRC ?= $(abs_top_srcdir)/../jdk
JTREG_TESTS ?= .
JTREG_WORK ?= jtreg-work
JTREG_REPORT ?= jtreg-report
FAKE_JDK = $(abs_top_srcdir)/test/fake-jdk
JTREG_TEST_DIR = $(JDK_SRC)/test/langtools/tools/javac
JTREG_OPTS = -compilejdk:$(FAKE_JDK) -jdk:$(JAVA_HOME) \
             -w:$(JTREG_WORK) -r:$(JTREG_REPORT) -va

jtreg-check:
	@test -x "$(JTREG_HOME)/bin/jtreg" || { \
	  echo "jtreg not found at $(JTREG_HOME) (set JTREG_HOME)"; exit 1; }
	@test -d "$(JTREG_TEST_DIR)" || { \
	  echo "OpenJDK test sources not found at $(JDK_SRC) (set JDK_SRC)"; exit 1; }
	@test -e "$(FAKE_JDK)/bin/java" || ln -sf "$(JAVA)" "$(FAKE_JDK)/bin/java"

jtreg-test: all jtreg-check
	GENESIS="$(abs_top_builddir)/src/genesis" \
	  $(JTREG_HOME)/bin/jtreg $(JTREG_OPTS) -ignore:quiet -conc:1 \
	  $(JTREG_TEST_DIR)/$(JTREG_TESTS)

jtreg-test-one: all jtreg-check
	@test -n "$(TEST)" || { echo "usage: make jtreg-test-one TEST=<name>"; exit 1; }
	GENESIS="$(abs_top_builddir)/src/genesis" \
	  $(JTREG_HOME)/bin/jtreg $(JTREG_OPTS) $(JTREG_TEST_DIR)/$(TEST)

clean-local:
	-rm -rf test/build $(JTREG_WORK) $(JTREG_REPORT)

.PHONY: check-one jtreg-check jtreg-test jtreg-test-one

endif HAVE_TESTS
