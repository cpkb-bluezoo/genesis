#!/bin/sh
#
# run-tests.sh - genesis regression test driver (development only)
#
# Copyright (C) 2020, 2026 Chris Burdess <dog@gnu.org>
#
# This file is part of genesis; see COPYING for the license terms.
#
# Environment (set by "make check"):
#   GENESIS     path to the genesis binary
#   JAVA        java runtime used to execute compiled tests
#   TEST_SRC    directory containing this script and src/ and invalid/
#   TEST_BUILD  output directory for compiled classes
#
# Usage: run-tests.sh [TestName]
#   With a TestName, compile and run just that test, verbosely.

here=$(cd "$(dirname "$0")" && pwd)
GENESIS=${GENESIS:-$here/../src/genesis}
TEST_SRC=${TEST_SRC:-$here}
TEST_BUILD=${TEST_BUILD:-$here/build}
JAVA=${JAVA:-${JAVA_HOME:+$JAVA_HOME/bin/java}}
JAVA=${JAVA:-java}

# Source-version directories, oldest first. Each holds tests requiring at
# least that language level.
versions="8 9 10 16 17 21 22 25"

sourcepath=
for v in $versions; do
    sourcepath="${sourcepath:+$sourcepath:}$TEST_SRC/src/java$v"
done

if ! command -v "$JAVA" >/dev/null 2>&1; then
    echo "SKIP: no Java runtime found (set JAVA or JAVA_HOME)"
    exit 77
fi

mkdir -p "$TEST_BUILD"

if [ -n "$1" ]; then
    for v in $versions; do
        f="$TEST_SRC/src/java$v/$1.java"
        if [ -f "$f" ]; then
            echo "Compiling $1 (Java $v)..."
            "$GENESIS" -verbose -source "$v" -d "$TEST_BUILD" \
                -sourcepath "$sourcepath" "$f" || exit 1
            echo "Running $1..."
            "$JAVA" -cp "$TEST_BUILD" "$1"
            exit $?
        fi
    done
    echo "Error: test $1 not found in any source directory" >&2
    exit 1
fi

echo "=== Genesis Test Suite ==="

# Helper classes (non-*Test files in every version directory) are compiled
# first, at that directory's language level; failures are tolerated because
# some depend on other helpers.
for v in $versions; do
    for f in "$TEST_SRC"/src/java$v/*.java; do
        [ -f "$f" ] || continue
        case $f in *Test.java) continue ;; esac
        "$GENESIS" -source "$v" -d "$TEST_BUILD" -sourcepath "$sourcepath" "$f" \
            >/dev/null 2>&1 || true
    done
done

passed=0
failed=0
for v in $versions; do
    dir="$TEST_SRC/src/java$v"
    [ -d "$dir" ] || continue
    echo
    echo "--- Java $v tests ---"
    for f in "$dir"/*Test.java; do
        [ -f "$f" ] || continue
        t=$(basename "$f" .java)
        printf "Testing %-30s ... " "$t"
        if "$GENESIS" -source "$v" -d "$TEST_BUILD" -sourcepath "$sourcepath" \
               "$f" >/dev/null 2>&1 &&
           "$JAVA" -cp "$TEST_BUILD" "$t" >/dev/null 2>&1; then
            echo PASS
            passed=$((passed + 1))
        else
            echo FAIL
            failed=$((failed + 1))
        fi
    done
done

echo
echo "--- Invalid input (must be rejected) ---"
for f in "$TEST_SRC"/invalid/*.java; do
    [ -f "$f" ] || continue
    t=$(basename "$f" .java)
    printf "Rejecting %-28s ... " "$t"
    "$GENESIS" -d "$TEST_BUILD" "$f" >/dev/null 2>&1
    rc=$?
    # Exit status 1 is a clean diagnostic; anything higher is a crash.
    if [ $rc -eq 1 ]; then
        echo PASS
        passed=$((passed + 1))
    else
        echo "FAIL (exit $rc)"
        failed=$((failed + 1))
    fi
done

echo
echo "--- Newer syntax under an older -source (must be rejected) ---"
# check_source_rejected <file> <expected message fragment> [genesis options...]
check_source_rejected() {
    name=$1
    msg=$2
    shift 2
    printf "%-14s rejected %-14s ... " "$name" "$*"
    rm -rf "$TEST_BUILD/oldsource"
    mkdir -p "$TEST_BUILD/oldsource"
    out=$("$GENESIS" "$@" -d "$TEST_BUILD/oldsource" \
              "$TEST_SRC/oldsource/$name.java" 2>&1)
    rc=$?
    if [ $rc -eq 1 ] && echo "$out" | grep -q "$msg"; then
        echo PASS
        passed=$((passed + 1))
    else
        echo "FAIL (exit $rc)"
        failed=$((failed + 1))
    fi
}
check_source_rejected OldRecord "records are not supported" -source 8
check_source_rejected OldRecord "records are not supported" -source 11
check_source_rejected OldRecord "records are not supported" -source 15
check_source_rejected OldSealed "sealed classes are not supported" -source 8
check_source_rejected OldSealed "sealed classes are not supported" -source 16

echo
echo "--- Generic classes loaded from class files ---"
# The library is compiled first, then the client against its class files
# only (-cp, no -sourcepath), so its symbols come from the class files.
printf "%-30s ... " "ExternalGenericTest"
ext_lib="$TEST_BUILD/external-lib"
ext_out="$TEST_BUILD/external-out"
rm -rf "$ext_lib" "$ext_out"
mkdir -p "$ext_lib" "$ext_out"
if "$GENESIS" -d "$ext_lib" "$TEST_SRC"/external/lib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$ext_lib" -d "$ext_out" \
       "$TEST_SRC/external/ExternalGenericTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$ext_out:$ext_lib" ExternalGenericTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Class file version (-source/-target/-release) ---"
# With no -target the version is computed from the features used (floor 52,
# Java 8). An explicit -target is a ceiling: needing more is an error.
#
# check_version <expected major> <TargetXxx> [genesis options...]
check_version() {
    want=$1
    name=$2
    shift 2
    printf "%-14s version %-3s %-22s ... " "$name" "$want" "${*:-(auto)}"
    rm -rf "$TEST_BUILD/classversion"
    mkdir -p "$TEST_BUILD/classversion"
    got=
    if "$GENESIS" "$@" -d "$TEST_BUILD/classversion" \
           "$TEST_SRC/classversion/$name.java" >/dev/null 2>&1; then
        got=$(od -An -tu1 -j6 -N2 "$TEST_BUILD/classversion/$name.class" |
              awk 'NR == 1 { print $1 * 256 + $2 }')
    fi
    if [ "$got" = "$want" ]; then
        echo PASS
        passed=$((passed + 1))
    else
        echo "FAIL (got ${got:-none})"
        failed=$((failed + 1))
    fi
}

# check_target_error <TargetXxx> [genesis options...]
# The compile must fail cleanly (exit 1) and name the -target conflict.
check_target_error() {
    name=$1
    shift
    printf "%-14s rejected %-22s ... " "$name" "$*"
    rm -rf "$TEST_BUILD/classversion"
    mkdir -p "$TEST_BUILD/classversion"
    out=$("$GENESIS" "$@" -d "$TEST_BUILD/classversion" \
              "$TEST_SRC/classversion/$name.java" 2>&1)
    rc=$?
    if [ $rc -eq 1 ] && echo "$out" | grep -q "require class file version" &&
       [ ! -f "$TEST_BUILD/classversion/$name.class" ]; then
        echo PASS
        passed=$((passed + 1))
    else
        echo "FAIL (exit $rc)"
        failed=$((failed + 1))
    fi
}

# Automatic target: lowest version the features need
check_version 52 TargetHello
check_version 52 TargetLambda
check_version 55 TargetNested
check_version 60 TargetRecord
check_version 61 TargetSealed
check_version 52 TargetHello -source 17
# Explicit target that is sufficient is used as given
check_version 69 TargetHello -target 25
check_version 61 TargetHello -source 21 -target 17
check_version 55 TargetHello -release 11
check_version 52 TargetHello -source 8 -target 8
check_version 52 TargetNested -target 8
check_version 55 TargetNested -target 11
# Explicit target lower than the features need is an error
check_target_error TargetLambda -target 7
check_target_error TargetRecord -target 8
check_target_error TargetRecord -target 11
check_target_error TargetSealed -target 16

echo
echo "=== Results: $passed passed, $failed failed ==="
[ $failed -eq 0 ]
