#!/bin/sh
#
# gumdrop-ant-test.sh - functional smoke: gumdrop "ant clean test" with genesis as javac
#
# Copyright (C) 2026 Chris Burdess <dog@gnu.org>
#
# This file is part of genesis; see COPYING for the license terms.
#
# Compiles gumdrop with genesis (forked external compiler) and runs unit tests only
# (ant "test" -> junit-test). Integration tests are out of scope here.
#
# Environment:
#   GENESIS            path to genesis (default: ../src/genesis from repo root)
#   JAVA_HOME          JDK to run Ant/JUnit (required)
#   GUMDROP_DIR        copy an existing gumdrop checkout into a temp work tree
#   GUMDROP_REPO_URL   git remote when cloning (default: cpkb-bluezoo/gumdrop)
#   GUMDROP_GIT_REF    optional branch, tag, or commit after clone
#   GUMDROP_JAVAC      compiler executable for Ant javac task (default: see below)
#   SMOKE_ANT_VERBOSE  if set, run ant with -verbose (full log still tee'd)
#   SMOKE_KEEP_WORKDIR if set, do not delete the temp directory on exit
#
# The work copy's <junit> tasks are patched to haltonfailure='no'
# haltonerror='no' so one run always exercises every suite instead of
# stopping at the first failure - this costs little extra time and gives
# a full picture (which suites/error classes to prioritize) instead of
# just the first blocker. The script itself still reports overall
# success/failure based on the aggregated JUnit results, not Ant's own
# exit code (which no longer reflects test failures once halt-on-* is
# off).
#
# Usage: gumdrop-ant-test.sh

set -e
set -o pipefail

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/.." && pwd)

GENESIS=${GENESIS:-$top/src/genesis}
GUMDROP_REPO_URL=${GUMDROP_REPO_URL:-https://github.com/cpkb-bluezoo/gumdrop.git}

if [ -z "$JAVA_HOME" ]; then
    echo "error: JAVA_HOME must be set (JDK for running Ant and JUnit)" >&2
    exit 1
fi
export JAVA_HOME
if ! command -v "$GENESIS" >/dev/null 2>&1; then
    echo "error: genesis not found at $GENESIS (build genesis or set GENESIS)" >&2
    exit 1
fi
if ! command -v ant >/dev/null 2>&1; then
    echo "error: ant not found on PATH" >&2
    exit 1
fi
if ! command -v git >/dev/null 2>&1; then
    echo "error: git not found" >&2
    exit 1
fi
if ! command -v perl >/dev/null 2>&1; then
    echo "error: perl not found (needed to patch gumdrop Ant javac tasks)" >&2
    exit 1
fi
if [ ! -x "$JAVA_HOME/bin/java" ]; then
    echo "error: $JAVA_HOME/bin/java not found" >&2
    exit 1
fi

# Ant passes many javac flags; the jtreg wrapper filters unsupported ones when present.
if [ -n "$GUMDROP_JAVAC" ]; then
    JAVAC_EXECUTABLE=$GUMDROP_JAVAC
elif [ -x "$top/test/fake-jdk/bin/javac" ]; then
    export GENESIS
    JAVAC_EXECUTABLE=$top/test/fake-jdk/bin/javac
else
    JAVAC_EXECUTABLE=$GENESIS
fi

usage() {
    echo "Usage: $(basename "$0")" >&2
    echo "See script header for GENESIS, JAVA_HOME, GUMDROP_DIR, and related variables." >&2
    exit 1
}

[ "$1" = "-h" ] || [ "$1" = "--help" ] && usage

WORKDIR=$(mktemp -d "${TMPDIR:-/tmp}/genesis-gumdrop-smoke.XXXXXX")
ANT_LOG=$WORKDIR/ant.log
REPO=$WORKDIR/repo

cleanup() {
    if [ -n "$SMOKE_KEEP_WORKDIR" ]; then
        echo "Keeping work directory: $WORKDIR"
        echo "Ant log: $ANT_LOG"
    else
        rm -rf "$WORKDIR"
    fi
}
trap cleanup EXIT

if [ -n "$GUMDROP_DIR" ]; then
    src=$(cd "$GUMDROP_DIR" && pwd)
    echo "Copying gumdrop from $src (original tree is not modified)"
    cp -R "$src" "$REPO"
else
    git clone --depth 1 "$GUMDROP_REPO_URL" "$REPO"
    if [ -n "$GUMDROP_GIT_REF" ]; then
        git -C "$REPO" fetch --depth 1 origin "$GUMDROP_GIT_REF"
        git -C "$REPO" checkout FETCH_HEAD
    fi
fi

# Ant has no supported global javac executable; -Djava.home breaks JDK 9+ (lib/modules).
# Patch gumdrop build files in the work copy only: fork genesis on every <javac> task.
patch_gumdrop_javac_tasks() {
    repo=$1
    export JAVAC_FOR_PATCH=$JAVAC_EXECUTABLE
    for f in "$repo/build.xml" "$repo/ant/"*.xml; do
        [ -f "$f" ] || continue
        perl -i -pe '
            if (/<javac\b/ && !/executable=/) {
                s/<javac\b/<javac fork="true" executable="$ENV{JAVAC_FOR_PATCH}" /;
            }
        ' "$f"
    done
    if grep -h '<javac' "$repo/build.xml" "$repo/ant/"*.xml 2>/dev/null | grep -v 'executable=' | grep -q .; then
        echo "error: some <javac> tasks in gumdrop Ant files were not patched" >&2
        return 1
    fi
    patched=$(grep -hc '<javac' "$repo/build.xml" "$repo/ant/"*.xml 2>/dev/null | awk '{s+=$1} END{print s}')
    echo "Patched $patched <javac> task(s) to fork: $JAVAC_EXECUTABLE"
}

# By default gumdrop's own <junit> tasks stop at the first failing suite
# (haltonfailure='yes' haltonerror='yes'), so a smoke run only ever shows
# the single blocker in front of you. Disabling both in the work copy
# lets every suite run every time - a small time cost that turns each
# smoke run into a full picture of where the compiler still diverges from
# javac, not just the nearest blocker.
patch_gumdrop_junit_halt() {
    repo=$1
    perl -i -pe "s/haltonfailure='yes'/haltonfailure='no'/g; s/haltonerror='yes'/haltonerror='no'/g" "$repo/build.xml"
    echo "Patched unit-test <junit> tasks to haltonfailure='no' haltonerror='no'"
}

# javac emits enum-switch synthetics Outer$N (a static int[] field named
# $SwitchMap$<enum>); genesis does not. A bare Outer$1.class is NOT by
# itself evidence of this - it's also the standard JVM name for Outer's
# own first anonymous/local class, which BOTH compilers legitimately
# emit (e.g. DnssecValidator's own anonymous Comparator<byte[]>), so the
# fingerprint has to actually distinguish the two, not just check
# Outer$1.class's existence - confirmed via strings on a real genesis-
# compiled DnssecValidator$1.class (a real compare() method, not a
# $SwitchMap$ field) after the earlier false positive this produced.
verify_build_tree_is_genesis() {
    root=$1
    failed=0
    ok=0
    check_no_javac_enum_switch_synthetic() {
        outer_rel=$1
        outer=$root/$outer_rel
        synthetic=${outer%.class}\$1.class
        if [ ! -f "$outer" ]; then
            echo "  skip fingerprint (no $outer_rel)"
            return 0
        fi
        if [ -f "$synthetic" ] && grep -aq 'SwitchMap' "$synthetic"; then
            echo "  FAIL: $synthetic has a \$SwitchMap\$ field (typical of javac, not genesis)" >&2
            failed=1
        else
            echo "  OK: $outer_rel without a javac enum-switch-map synthetic (genesis fingerprint)"
            ok=$((ok + 1))
        fi
    }
    echo "Checking build/ fingerprints (genesis vs javac enum-switch synthetics):"
    check_no_javac_enum_switch_synthetic build/core/org/bluezoo/gumdrop/HandshakeSecurityInfo.class
    check_no_javac_enum_switch_synthetic build/core/org/bluezoo/gumdrop/SelectorLoop.class
    check_no_javac_enum_switch_synthetic build/core/org/bluezoo/gumdrop/dns/client/DnssecValidator.class
    if [ "$failed" -ne 0 ]; then
        echo "error: build/ looks javac-compiled; Ant did not fork genesis" >&2
        return 1
    fi
    if [ "$ok" -eq 0 ]; then
        echo "error: no genesis fingerprints found under build/ (compile did not run?)" >&2
        return 1
    fi
    return 0
}

# With haltonfailure/haltonerror off, `ant test` itself exits 0 even when
# JUnit suites failed - so the actual pass/fail signal has to come from
# aggregating the JUnit result files ourselves, not Ant's exit code.
# Prints a summary and the list of failing suites; sets
# JUNIT_TOTAL_PROBLEMS (failures + errors, 0 if all suites passed) as a
# side effect for the caller to check.
summarize_junit_results() {
    repo=$1
    results_dir="$repo/test/junit/results"
    JUNIT_TOTAL_PROBLEMS=0
    if [ ! -d "$results_dir" ]; then
        echo "warning: no JUnit results directory at $results_dir (did any test run?)" >&2
        return 0
    fi
    total_suites=0
    total_tests=0
    total_failures=0
    total_errors=0
    failing_suites=""
    for f in "$results_dir"/TEST-*.txt; do
        [ -f "$f" ] || continue
        line=$(grep -m1 '^Tests run:' "$f") || continue
        run=$(echo "$line" | sed -E 's/.*Tests run: ([0-9]+).*/\1/')
        fail=$(echo "$line" | sed -E 's/.*Failures: ([0-9]+).*/\1/')
        err=$(echo "$line" | sed -E 's/.*Errors: ([0-9]+).*/\1/')
        total_suites=$((total_suites + 1))
        total_tests=$((total_tests + run))
        total_failures=$((total_failures + fail))
        total_errors=$((total_errors + err))
        if [ "$fail" -gt 0 ] || [ "$err" -gt 0 ]; then
            suite=$(basename "$f" .txt)
            suite=${suite#TEST-}
            failing_suites="$failing_suites $suite"
        fi
    done
    n_failing=0
    for s in $failing_suites; do n_failing=$((n_failing + 1)); done
    echo ""
    echo "---- JUnit summary ----"
    echo "  Suites run:     $total_suites"
    echo "  Tests run:      $total_tests"
    echo "  Failures:       $total_failures"
    echo "  Errors:         $total_errors"
    echo "  Failing suites: $n_failing"
    if [ "$n_failing" -gt 0 ]; then
        echo ""
        echo "  Failing suite list:"
        for s in $failing_suites; do
            echo "    - $s"
        done
    fi
    echo "------------------------"
    JUNIT_TOTAL_PROBLEMS=$((total_failures + total_errors))
}

count_forked_compiler_in_log() {
    log=$1
    if [ ! -s "$log" ]; then
        echo 0
        return
    fi
    base=$(basename "$JAVAC_EXECUTABLE")
    grep -cE "Executing '.*($base|genesis)" "$log" 2>/dev/null || echo 0
}

run_ant() {
    phase=$1
    shift
    echo ""
    echo "======== $phase ========"
    if [ -n "$SMOKE_ANT_VERBOSE" ]; then
        set -- ant -verbose "$@"
    else
        set -- ant "$@"
    fi
    if ! "$@" 2>&1 | tee -a "$ANT_LOG"; then
        echo "error: $phase failed (see $ANT_LOG)" >&2
        exit 1
    fi
}

echo "=== Gumdrop genesis smoke (ant clean test) ==="
echo "Work copy:   $REPO"
echo "Genesis:     $GENESIS ($("$GENESIS" -version 2>&1 | head -1))"
echo "Ant javac:   fork executable -> $JAVAC_EXECUTABLE (patched into build.xml)"
echo "JAVA_HOME:   $JAVA_HOME (Ant + JUnit JVM)"
echo "Ant log:     $ANT_LOG"
echo ""
echo "Flow: clean removes build/, test compiles with forked genesis, then JUnit."
: >"$ANT_LOG"

patch_gumdrop_javac_tasks "$REPO"
patch_gumdrop_junit_halt "$REPO"

cd "$REPO"

run_ant "resolve dependencies" resolve-deps resolve-test-deps
run_ant "ant clean" clean
run_ant "ant test (compile + JUnit)" test

verify_build_tree_is_genesis "$REPO" || exit 1

summarize_junit_results "$REPO"

forks=$(count_forked_compiler_in_log "$ANT_LOG")
echo ""
echo "=============================================="
if [ "$JUNIT_TOTAL_PROBLEMS" -eq 0 ]; then
    echo "  SMOKE PASSED"
else
    echo "  SMOKE FAILED ($JUNIT_TOTAL_PROBLEMS JUnit failure/error(s) - see summary above)"
fi
echo "  Compile:  genesis forked from Ant ($JAVAC_EXECUTABLE)"
if [ -n "$SMOKE_ANT_VERBOSE" ] && [ "$forks" -gt 0 ]; then
    echo "            ($forks compiler Executing line(s) in log)"
fi
echo "  Tests:    JUnit ant test on classes under build/ (haltonfailure=no: every suite runs)"
echo "  Full log: $ANT_LOG"
echo "=============================================="

if [ "$JUNIT_TOTAL_PROBLEMS" -ne 0 ]; then
    exit 1
fi
