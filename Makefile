# Makefile for genesis
#
# Build options:
#   make              - Debug build (with -g)
#   make release      - Optimized build (with -O2, no debug symbols)
#   make DEBUG=0      - Same as release

CC ?= cc
DEBUG ?= 1

# Java runtime for tests (needs Java 21+ for class file version 61)
# JAVA_HOME must be set in the environment
JAVA ?= $(JAVA_HOME)/bin/java

# Source and build directories
SRC_DIR = src
BUILD_DIR = build

ifeq ($(DEBUG),1)
CFLAGS = -Wall -Wextra -g -std=c99 -pedantic -I$(SRC_DIR)
else
CFLAGS = -Wall -Wextra -O2 -std=c99 -pedantic -DNDEBUG -I$(SRC_DIR)
endif

# Platform-specific flags
UNAME := $(shell uname)
ifeq ($(UNAME),Darwin)
# macOS
LDFLAGS ?=
else ifeq ($(UNAME),Linux)
# Linux
LDFLAGS ?=
endif

LIBS = -lz -pthread

# Source files
SOURCES = \
    util.c \
    type.c \
    encoding.c \
    constpool.c \
    classwriter.c \
    stackmap.c \
    indy.c \
    jarwriter.c \
    lexer.c \
    parser.c \
    semantic.c \
    classfile.c \
    classpath.c \
    codegen.c \
    codegen_expr.c \
    codegen_stmt.c \
    genesis.c

# Object files go in build directory
OBJECTS = $(addprefix $(BUILD_DIR)/,$(SOURCES:.c=.o))

# Main target
all: genesis

# Create build directory
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

genesis: $(BUILD_DIR) $(OBJECTS)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) $(LIBS)

# Pattern rule for object files
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) -c $< -o $@

# Dependencies (now referencing src directory)
$(BUILD_DIR)/util.o: $(SRC_DIR)/util.c $(SRC_DIR)/util.h
$(BUILD_DIR)/type.o: $(SRC_DIR)/type.c $(SRC_DIR)/type.h $(SRC_DIR)/util.h
$(BUILD_DIR)/encoding.o: $(SRC_DIR)/encoding.c $(SRC_DIR)/encoding.h
$(BUILD_DIR)/constpool.o: $(SRC_DIR)/constpool.c $(SRC_DIR)/constpool.h $(SRC_DIR)/util.h
$(BUILD_DIR)/classwriter.o: $(SRC_DIR)/classwriter.c $(SRC_DIR)/classwriter.h $(SRC_DIR)/codegen.h $(SRC_DIR)/constpool.h $(SRC_DIR)/stackmap.h $(SRC_DIR)/genesis.h $(SRC_DIR)/util.h
$(BUILD_DIR)/stackmap.o: $(SRC_DIR)/stackmap.c $(SRC_DIR)/stackmap.h $(SRC_DIR)/constpool.h $(SRC_DIR)/codegen.h $(SRC_DIR)/util.h
$(BUILD_DIR)/indy.o: $(SRC_DIR)/indy.c $(SRC_DIR)/indy.h $(SRC_DIR)/constpool.h $(SRC_DIR)/util.h
$(BUILD_DIR)/jarwriter.o: $(SRC_DIR)/jarwriter.c $(SRC_DIR)/jarwriter.h
$(BUILD_DIR)/lexer.o: $(SRC_DIR)/lexer.c $(SRC_DIR)/genesis.h $(SRC_DIR)/util.h
$(BUILD_DIR)/parser.o: $(SRC_DIR)/parser.c $(SRC_DIR)/genesis.h $(SRC_DIR)/util.h
$(BUILD_DIR)/semantic.o: $(SRC_DIR)/semantic.c $(SRC_DIR)/genesis.h $(SRC_DIR)/type.h $(SRC_DIR)/util.h
$(BUILD_DIR)/classfile.o: $(SRC_DIR)/classfile.c $(SRC_DIR)/classfile.h $(SRC_DIR)/util.h
$(BUILD_DIR)/classpath.o: $(SRC_DIR)/classpath.c $(SRC_DIR)/classpath.h $(SRC_DIR)/classfile.h $(SRC_DIR)/util.h
$(BUILD_DIR)/codegen.o: $(SRC_DIR)/codegen.c $(SRC_DIR)/codegen.h $(SRC_DIR)/codegen_internal.h $(SRC_DIR)/constpool.h $(SRC_DIR)/classwriter.h $(SRC_DIR)/genesis.h $(SRC_DIR)/util.h
$(BUILD_DIR)/codegen_expr.o: $(SRC_DIR)/codegen_expr.c $(SRC_DIR)/codegen.h $(SRC_DIR)/codegen_internal.h $(SRC_DIR)/constpool.h $(SRC_DIR)/genesis.h $(SRC_DIR)/util.h
$(BUILD_DIR)/codegen_stmt.o: $(SRC_DIR)/codegen_stmt.c $(SRC_DIR)/codegen.h $(SRC_DIR)/codegen_internal.h $(SRC_DIR)/constpool.h $(SRC_DIR)/genesis.h $(SRC_DIR)/util.h
$(BUILD_DIR)/genesis.o: $(SRC_DIR)/genesis.c $(SRC_DIR)/genesis.h $(SRC_DIR)/util.h $(SRC_DIR)/classpath.h $(SRC_DIR)/codegen.h $(SRC_DIR)/encoding.h $(SRC_DIR)/jarwriter.h

# Clean up
clean:
	$(RM) genesis
	$(RM) -r $(BUILD_DIR)
	$(RM) -r *.dSYM
	$(RM) -r $(TEST_BUILD)

# Install (adjust PREFIX as needed)
PREFIX ?= /usr/local
install: genesis
	install -d $(PREFIX)/bin
	install -m 755 genesis $(PREFIX)/bin/genesis

# Uninstall
uninstall:
	$(RM) $(PREFIX)/bin/genesis

# Test directories - organized by minimum source version required
TEST_SRC = test/src
TEST_BUILD = test/build

# Source version directories
TEST_SRC_8 = $(TEST_SRC)/java8
TEST_SRC_9 = $(TEST_SRC)/java9
TEST_SRC_10 = $(TEST_SRC)/java10
TEST_SRC_16 = $(TEST_SRC)/java16
TEST_SRC_17 = $(TEST_SRC)/java17
TEST_SRC_21 = $(TEST_SRC)/java21

# Test classes by version
TEST_CLASSES_8 = $(basename $(notdir $(wildcard $(TEST_SRC_8)/*Test.java)))
TEST_CLASSES_9 = $(basename $(notdir $(wildcard $(TEST_SRC_9)/*Test.java)))
TEST_CLASSES_10 = $(basename $(notdir $(wildcard $(TEST_SRC_10)/*Test.java)))
TEST_CLASSES_16 = $(basename $(notdir $(wildcard $(TEST_SRC_16)/*Test.java)))
TEST_CLASSES_17 = $(basename $(notdir $(wildcard $(TEST_SRC_17)/*Test.java)))
TEST_CLASSES_21 = $(basename $(notdir $(wildcard $(TEST_SRC_21)/*Test.java)))

# Helper classes (non-test files in java8 directory)
HELPER_CLASSES = $(filter-out $(addsuffix .java,$(TEST_CLASSES_8)), $(notdir $(wildcard $(TEST_SRC_8)/*.java)))

# Combined sourcepath for all version directories
ALL_SOURCEPATHS = $(TEST_SRC_8):$(TEST_SRC_9):$(TEST_SRC_10):$(TEST_SRC_16):$(TEST_SRC_17):$(TEST_SRC_21)

# Build and run all tests
test: genesis
	@echo "=== Genesis Test Suite ==="
	@echo ""
	@mkdir -p $(TEST_BUILD)
	@# Compile helper classes first (from java8 dir)
	@for helper in $(basename $(HELPER_CLASSES)); do \
		./genesis -source 8 -d $(TEST_BUILD) -sourcepath $(ALL_SOURCEPATHS) $(TEST_SRC_8)/$$helper.java 2>/dev/null || true; \
	done
	@passed=0; failed=0; \
	echo "--- Java 8 tests ---"; \
	for test in $(TEST_CLASSES_8); do \
		printf "Testing %-25s ... " "$$test"; \
		if ./genesis -source 8 -d $(TEST_BUILD) -sourcepath $(ALL_SOURCEPATHS) $(TEST_SRC_8)/$$test.java 2>/dev/null && \
		   $(JAVA) -cp $(TEST_BUILD) $$test >/dev/null 2>&1; then \
			echo "PASS"; \
			passed=$$((passed + 1)); \
		else \
			echo "FAIL"; \
			failed=$$((failed + 1)); \
		fi; \
	done; \
	echo ""; \
	echo "--- Java 9 tests (private interface methods) ---"; \
	for test in $(TEST_CLASSES_9); do \
		printf "Testing %-25s ... " "$$test"; \
		if ./genesis -source 9 -d $(TEST_BUILD) -sourcepath $(ALL_SOURCEPATHS) $(TEST_SRC_9)/$$test.java 2>/dev/null && \
		   $(JAVA) -cp $(TEST_BUILD) $$test >/dev/null 2>&1; then \
			echo "PASS"; \
			passed=$$((passed + 1)); \
		else \
			echo "FAIL"; \
			failed=$$((failed + 1)); \
		fi; \
	done; \
	echo ""; \
	echo "--- Java 10 tests (var) ---"; \
	for test in $(TEST_CLASSES_10); do \
		printf "Testing %-25s ... " "$$test"; \
		if ./genesis -source 10 -d $(TEST_BUILD) -sourcepath $(ALL_SOURCEPATHS) $(TEST_SRC_10)/$$test.java 2>/dev/null && \
		   $(JAVA) -cp $(TEST_BUILD) $$test >/dev/null 2>&1; then \
			echo "PASS"; \
			passed=$$((passed + 1)); \
		else \
			echo "FAIL"; \
			failed=$$((failed + 1)); \
		fi; \
	done; \
	echo ""; \
	echo "--- Java 16 tests (records, instanceof patterns) ---"; \
	for test in $(TEST_CLASSES_16); do \
		printf "Testing %-25s ... " "$$test"; \
		if ./genesis -source 16 -d $(TEST_BUILD) -sourcepath $(ALL_SOURCEPATHS) $(TEST_SRC_16)/$$test.java 2>/dev/null && \
		   $(JAVA) -cp $(TEST_BUILD) $$test >/dev/null 2>&1; then \
			echo "PASS"; \
			passed=$$((passed + 1)); \
		else \
			echo "FAIL"; \
			failed=$$((failed + 1)); \
		fi; \
	done; \
	echo ""; \
	echo "--- Java 17 tests (sealed classes) ---"; \
	for test in $(TEST_CLASSES_17); do \
		printf "Testing %-25s ... " "$$test"; \
		if ./genesis -source 17 -d $(TEST_BUILD) -sourcepath $(ALL_SOURCEPATHS) $(TEST_SRC_17)/$$test.java 2>/dev/null && \
		   $(JAVA) -cp $(TEST_BUILD) $$test >/dev/null 2>&1; then \
			echo "PASS"; \
			passed=$$((passed + 1)); \
		else \
			echo "FAIL"; \
			failed=$$((failed + 1)); \
		fi; \
	done; \
	echo ""; \
	echo "--- Java 21 tests (pattern switch, record patterns) ---"; \
	for test in $(TEST_CLASSES_21); do \
		printf "Testing %-25s ... " "$$test"; \
		if ./genesis -source 21 --enable-preview -d $(TEST_BUILD) -sourcepath $(ALL_SOURCEPATHS) $(TEST_SRC_21)/$$test.java 2>/dev/null && \
		   $(JAVA) --enable-preview -cp $(TEST_BUILD) $$test >/dev/null 2>&1; then \
			echo "PASS"; \
			passed=$$((passed + 1)); \
		else \
			echo "FAIL"; \
			failed=$$((failed + 1)); \
		fi; \
	done; \
	echo ""; \
	echo "=== Results: $$passed passed, $$failed failed ===" ; \
	if [ $$failed -gt 0 ]; then exit 1; fi

# Run a single test with verbose output (usage: make test-one TEST=HelloWorld)
# Auto-detects which source directory the test is in
test-one: genesis
	@mkdir -p $(TEST_BUILD)
	@if [ -z "$(TEST)" ]; then \
		echo "Usage: make test-one TEST=<TestName>"; \
		echo "Example: make test-one TEST=HelloWorld"; \
		exit 1; \
	fi
	@# Find which directory contains the test
	@if [ -f "$(TEST_SRC_8)/$(TEST).java" ]; then \
		echo "Compiling $(TEST) (Java 8)..."; \
		./genesis -verbose -source 8 -d $(TEST_BUILD) -sourcepath $(ALL_SOURCEPATHS) $(TEST_SRC_8)/$(TEST).java; \
		echo ""; echo "Running $(TEST)..."; echo "----------------------------------------"; \
		$(JAVA) -cp $(TEST_BUILD) $(TEST); \
	elif [ -f "$(TEST_SRC_9)/$(TEST).java" ]; then \
		echo "Compiling $(TEST) (Java 9)..."; \
		./genesis -verbose -source 9 -d $(TEST_BUILD) -sourcepath $(ALL_SOURCEPATHS) $(TEST_SRC_9)/$(TEST).java; \
		echo ""; echo "Running $(TEST)..."; echo "----------------------------------------"; \
		$(JAVA) -cp $(TEST_BUILD) $(TEST); \
	elif [ -f "$(TEST_SRC_10)/$(TEST).java" ]; then \
		echo "Compiling $(TEST) (Java 10)..."; \
		./genesis -verbose -source 10 -d $(TEST_BUILD) -sourcepath $(ALL_SOURCEPATHS) $(TEST_SRC_10)/$(TEST).java; \
		echo ""; echo "Running $(TEST)..."; echo "----------------------------------------"; \
		$(JAVA) -cp $(TEST_BUILD) $(TEST); \
	elif [ -f "$(TEST_SRC_16)/$(TEST).java" ]; then \
		echo "Compiling $(TEST) (Java 16)..."; \
		./genesis -verbose -source 16 -d $(TEST_BUILD) -sourcepath $(ALL_SOURCEPATHS) $(TEST_SRC_16)/$(TEST).java; \
		echo ""; echo "Running $(TEST)..."; echo "----------------------------------------"; \
		$(JAVA) -cp $(TEST_BUILD) $(TEST); \
	elif [ -f "$(TEST_SRC_17)/$(TEST).java" ]; then \
		echo "Compiling $(TEST) (Java 17)..."; \
		./genesis -verbose -source 17 -d $(TEST_BUILD) -sourcepath $(ALL_SOURCEPATHS) $(TEST_SRC_17)/$(TEST).java; \
		echo ""; echo "Running $(TEST)..."; echo "----------------------------------------"; \
		$(JAVA) -cp $(TEST_BUILD) $(TEST); \
	elif [ -f "$(TEST_SRC_21)/$(TEST).java" ]; then \
		echo "Compiling $(TEST) (Java 21 --enable-preview)..."; \
		./genesis -verbose -source 21 --enable-preview -d $(TEST_BUILD) -sourcepath $(ALL_SOURCEPATHS) $(TEST_SRC_21)/$(TEST).java; \
		echo ""; echo "Running $(TEST)..."; echo "----------------------------------------"; \
		$(JAVA) --enable-preview -cp $(TEST_BUILD) $(TEST); \
	else \
		echo "Error: Test $(TEST) not found in any source directory"; \
		exit 1; \
	fi
	@echo "----------------------------------------"

# Clean test build directory
test-clean:
	$(RM) -r $(TEST_BUILD)

# List all tests
test-list:
	@echo "=== Tests ($(words $(TEST_CLASSES))) ==="
	@for t in $(TEST_CLASSES); do echo "  $$t"; done

# Quick version check
test-version: genesis
	./genesis -version

# Release build (optimized, no debug symbols)
release:
	$(MAKE) DEBUG=0 clean all

# ============================================================================
# jtreg Integration (OpenJDK compiler tests)
# ============================================================================
# 
# Prerequisites:
#   1. jtreg built at ../jtreg/build/images/jtreg
#   2. OpenJDK sources at ../jdk (for test cases)
#
# To set up:
#   git clone --depth 1 https://github.com/openjdk/jdk ../jdk
#
# Usage:
#   make jtreg-test                    # Run all javac tests
#   make jtreg-test JTREG_TESTS=lambda # Run tests in lambda/ subdirectory
#   make jtreg-test-one TEST=T1234567  # Run a single test

JTREG_HOME ?= ../jtreg/build/images/jtreg
JDK_SRC ?= ../jdk
JTREG_TESTS ?= .
JTREG_WORK ?= jtreg-work
JTREG_REPORT ?= jtreg-report

# The fake JDK that wraps genesis
FAKE_JDK = $(CURDIR)/fake-jdk

# jtreg test directory (langtools compiler tests)
JTREG_TEST_DIR = $(JDK_SRC)/test/langtools/tools/javac

jtreg-check:
	@if [ ! -x "$(JTREG_HOME)/bin/jtreg" ]; then \
		echo "Error: jtreg not found at $(JTREG_HOME)"; \
		echo "Build jtreg first: cd ../jtreg && bash make/build.sh --jdk \$$JAVA_HOME"; \
		exit 1; \
	fi
	@if [ ! -d "$(JTREG_TEST_DIR)" ]; then \
		echo "Error: OpenJDK test sources not found at $(JDK_SRC)"; \
		echo "Clone them: git clone --depth 1 https://github.com/openjdk/jdk $(JDK_SRC)"; \
		exit 1; \
	fi
	@if [ ! -e "$(FAKE_JDK)/bin/java" ]; then \
		ln -sf "$$(which java)" "$(FAKE_JDK)/bin/java"; \
	fi

jtreg-test: genesis jtreg-check
	@echo "=== Running jtreg tests with Genesis ==="
	@mkdir -p $(JTREG_WORK) $(JTREG_REPORT)
	$(JTREG_HOME)/bin/jtreg \
		-compilejdk:$(FAKE_JDK) \
		-jdk:$(JAVA_HOME) \
		-w:$(JTREG_WORK) \
		-r:$(JTREG_REPORT) \
		-va \
		-ignore:quiet \
		-conc:1 \
		$(JTREG_TEST_DIR)/$(JTREG_TESTS)

# Run a single jtreg test (usage: make jtreg-test-one TEST=T1234567)
jtreg-test-one: genesis jtreg-check
	@if [ -z "$(TEST)" ]; then \
		echo "Usage: make jtreg-test-one TEST=<test-name>"; \
		echo "Example: make jtreg-test-one TEST=lambda/T8129740"; \
		exit 1; \
	fi
	$(JTREG_HOME)/bin/jtreg \
		-compilejdk:$(FAKE_JDK) \
		-jdk:$(JAVA_HOME) \
		-w:$(JTREG_WORK) \
		-r:$(JTREG_REPORT) \
		-va \
		$(JTREG_TEST_DIR)/$(TEST)

# List available jtreg test directories
jtreg-list:
	@echo "=== Available jtreg test directories ==="
	@ls -1 $(JTREG_TEST_DIR) 2>/dev/null || echo "OpenJDK sources not found. Run: git clone --depth 1 https://github.com/openjdk/jdk $(JDK_SRC)"

# Clean jtreg output
jtreg-clean:
	$(RM) -r $(JTREG_WORK) $(JTREG_REPORT)

.PHONY: all clean install uninstall test test-one test-clean test-list test-version release
.PHONY: jtreg-check jtreg-test jtreg-test-one jtreg-list jtreg-clean
