#!/bin/sh
#
# spring-boot-bench.sh - benchmark genesis vs javac compiling Spring Boot
#
# Copyright (C) 2026 Chris Burdess <dog@gnu.org>
#
# This file is part of genesis; see COPYING for the license terms.
#
# Gets a release of Spring Boot (a shallow clone, unless SB_DIR already is a git
# checkout), works out its projects and downloads the jars they compile against
# (Maven Central, plus the Gradle distribution's own jars into SB_DIR/.gradle; no
# Gradle or Maven is run), then compiles every main, testFixtures and test source
# set with javac and with genesis, one source set at a time, and reports how long
# the compiler invocations took. The tests are not run: this is a raw compile-time
# comparison (see spring-boot-smoke.sh for the correctness check). The first pass
# over every compiler is untimed warm-up, and checks that genesis compiles
# everything javac does.
#
# Three rows are reported: javac, genesis -j1 (one process, one thread) and genesis
# with its default jobs. Every source set is one compiler invocation, so javac's
# JVM start-up is paid once per source set; Spring Boot's own Gradle build compiles
# inside a long-running daemon instead.
#
# Environment:
#   GENESIS          path to genesis (default: ../src/genesis from this directory)
#   JAVA_HOME        JDK 25 that ships jmods (required)
#   SB_DIR           Spring Boot checkout (default: ../spring-boot next to genesis);
#                    cloned there when it is not a git checkout
#   SB_REF           tag or branch to build (default: v4.1.1)
#   SB_REPO_URL      remote to clone (default: the spring-projects repository)
#   SB_CACHE         jar and pom cache (default: ~/.cache/genesis-bench/maven)
#   SB_PROJECTS      regular expression: only projects whose path matches
#   BENCH_ITERATIONS timed passes per compiler (default: 3)
#
# Usage: spring-boot-bench.sh

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
SB_DIR=${SB_DIR:-$top/../spring-boot}
SB_REF=${SB_REF:-v4.1.1}
SB_REPO_URL=${SB_REPO_URL:-https://github.com/spring-projects/spring-boot.git}
BENCH_ITERATIONS=${BENCH_ITERATIONS:-3}

case $BENCH_ITERATIONS in
    ''|*[!0-9]*|0) echo "error: BENCH_ITERATIONS must be a positive integer" >&2; exit 1 ;;
esac

# A directory that holds only a restored .gradle cache is not a checkout yet.
if [ ! -d "$SB_DIR/.git" ]; then
    echo "Cloning $SB_REPO_URL $SB_REF into $SB_DIR"
    mkdir -p "$SB_DIR"
    git -C "$SB_DIR" init -q
    git -C "$SB_DIR" remote add origin "$SB_REPO_URL"
    if git -C "$SB_DIR" fetch -q --depth 1 origin "refs/tags/$SB_REF:refs/tags/$SB_REF" 2>/dev/null; then
        :
    else
        git -C "$SB_DIR" fetch -q --depth 1 origin "refs/heads/$SB_REF:refs/tags/$SB_REF"
    fi
fi

SB_DIR=$(cd "$SB_DIR" && pwd)
export GENESIS JAVA_HOME SB_DIR SB_REF
export SB_COMPILERS=javac,genesis,genesis-par
export SB_NO_TESTS=1
export SB_BENCH_ITERATIONS=$BENCH_ITERATIONS

exec python3 "$here/spring-boot/smoke.py"
