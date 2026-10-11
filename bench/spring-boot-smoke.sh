#!/bin/sh
#
# spring-boot-smoke.sh - smoke: compile Spring Boot with genesis and javac, run its unit tests
#
# Copyright (C) 2026 Chris Burdess <dog@gnu.org>
#
# This file is part of genesis; see COPYING for the license terms.
#
# Takes a release of Spring Boot from a git checkout, compiles core/, module/ and
# test-support/ with javac (the baseline) and with genesis, one project at a time
# with its own classpath, and runs each project's tests with the JUnit console
# launcher on both sets of classes. No Gradle or Maven is involved: the projects,
# their dependencies and the jars are worked out by bench/spring-boot/*.py (Python 3,
# jars from Maven Central, checked against bench/spring-boot/<version>.lock).
#
# The smoke fails when genesis cannot compile a project that javac can, or a test
# that passes on javac's classes fails on genesis's.
#
# Environment:
#   GENESIS          path to genesis (default: ../src/genesis from this directory)
#   JAVA_HOME        JDK 25 that ships jmods (required)
#   SB_DIR           git checkout of spring-boot (default: ../spring-boot next to genesis)
#   SB_REF           tag or commit to build (default: v4.1.1)
#   SB_PROJECTS      regular expression: only projects whose path matches, e.g. ':core:'
#   SB_COMPILERS     javac,genesis (default), or just one of them
#   SB_JOBS          projects handled at once (default: number of processors)
#   SB_NO_TESTS      if set, compile only
#   SB_KEEP          if set, keep the work directory (class files, logs, reports)
#   SB_CACHE         jar and pom cache (default: ~/.cache/genesis-bench/maven)
# See bench/spring-boot/smoke.py for the complete list.
#
# Usage: spring-boot-smoke.sh

set -e

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/.." && pwd)

if [ -z "$JAVA_HOME" ]; then
    echo "error: JAVA_HOME must be set (a JDK 25 that ships jmods)" >&2
    exit 1
fi
for tool in python3 git; do
    if ! command -v "$tool" >/dev/null 2>&1; then
        echo "error: $tool not found" >&2
        exit 1
    fi
done

GENESIS=${GENESIS:-$top/src/genesis}
export GENESIS JAVA_HOME

exec python3 "$here/spring-boot/smoke.py"
