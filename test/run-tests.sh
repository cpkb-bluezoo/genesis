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
# Only used by the ExternalJavacConstant test below, which specifically
# needs a REAL javac-compiled library (genesis's own classwriter uses
# <clinit> for constant initialization, never a ConstantValue attribute -
# see the README - so a genesis-compiled library can never exercise
# genesis's ConstantValue-reading support at all).
JAVAC=${JAVAC:-${JAVA_HOME:+$JAVA_HOME/bin/javac}}
JAVAC=${JAVAC:-javac}

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

printf "%-30s ... " "MultiCatchExternalLibException"
mc_lib="$TEST_BUILD/multicatch-ext-lib"
mc_out="$TEST_BUILD/multicatch-ext-out"
rm -rf "$mc_lib" "$mc_out"
mkdir -p "$mc_lib" "$mc_out"
if "$GENESIS" -d "$mc_lib" "$TEST_SRC"/external/lib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$mc_lib" -d "$mc_out" \
       "$TEST_SRC/external/MultiCatchExternalLibExceptionTest.java" \
       "$TEST_SRC/external/MultiCatchLocalException.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$mc_out:$mc_lib" MultiCatchExternalLibExceptionTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "ExternalJavacConstantSwitch"
ejc_lib="$TEST_BUILD/external-javac-lib"
ejc_out="$TEST_BUILD/external-javac-out"
rm -rf "$ejc_lib" "$ejc_out"
mkdir -p "$ejc_lib" "$ejc_out"
if command -v "$JAVAC" >/dev/null 2>&1 &&
   "$JAVAC" -d "$ejc_lib" "$TEST_SRC"/external/javaclib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$ejc_lib" -d "$ejc_out" \
       "$TEST_SRC/external/ExternalJavacConstantSwitchTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$ejc_out:$ejc_lib" ExternalJavacConstantSwitchTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "CrossModuleConstantSwitch"
cmc_lib="$TEST_BUILD/cross-module-const-lib"
cmc_out="$TEST_BUILD/cross-module-const-out"
rm -rf "$cmc_lib" "$cmc_out"
mkdir -p "$cmc_lib" "$cmc_out"
if "$GENESIS" -d "$cmc_lib" "$TEST_SRC"/external/genesislib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$cmc_lib" -d "$cmc_out" \
       "$TEST_SRC/external/CrossModuleConstantSwitchTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$cmc_out:$cmc_lib" CrossModuleConstantSwitchTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "StaticImportSwitchCaseLabel"
sisc_out="$TEST_BUILD/static-import-switch-out"
rm -rf "$sisc_out"
mkdir -p "$sisc_out"
if "$GENESIS" -d "$sisc_out" "$TEST_SRC/external/genesislib/SocksAtypConsts.java" \
       "$TEST_SRC/external/StaticImportSwitchCaseLabelTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$sisc_out" StaticImportSwitchCaseLabelTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "CrossModuleQualifiedIfaceConst"
cmqi_lib="$TEST_BUILD/cross-module-qualified-iface-const-lib"
cmqi_out="$TEST_BUILD/cross-module-qualified-iface-const-out"
rm -rf "$cmqi_lib" "$cmqi_out"
mkdir -p "$cmqi_lib" "$cmqi_out"
if "$GENESIS" -d "$cmqi_lib" "$TEST_SRC/external/genesislib/FrameSettings.java" >/dev/null 2>&1 &&
   "$GENESIS" -cp "$cmqi_lib" -d "$cmqi_out" \
       "$TEST_SRC/external/CrossModuleQualifiedInterfaceConstantSwitchTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$cmqi_out:$cmqi_lib" CrossModuleQualifiedInterfaceConstantSwitchTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "NestedGenericInterfaceBridge"
if "$GENESIS" -cp "$ext_lib" -d "$ext_out" \
       "$TEST_SRC/external/NestedGenericInterfaceBridgeTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$ext_out:$ext_lib" NestedGenericInterfaceBridgeTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "GenericSuperCtorErasure"
if "$GENESIS" -cp "$ext_lib" -d "$ext_out" \
       "$TEST_SRC/external/GenericSuperCtorErasureTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$ext_out:$ext_lib" GenericSuperCtorErasureTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "BoundedTypeVarErasure"
if "$GENESIS" -cp "$ext_lib" -d "$ext_out" \
       "$TEST_SRC/external/BoundedTypeVarErasureTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$ext_out:$ext_lib" BoundedTypeVarErasureTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "BoundedInterfaceBridge"
if "$GENESIS" -cp "$ext_lib" -d "$ext_out" \
       "$TEST_SRC/external/BoundedInterfaceBridgeTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$ext_out:$ext_lib" BoundedInterfaceBridgeTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "AnnotationLongElement"
if "$GENESIS" -cp "$ext_lib" -d "$ext_out" \
       "$TEST_SRC/external/AnnotationLongElementTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$ext_out:$ext_lib" AnnotationLongElementTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "AnnotationClassElement"
if "$GENESIS" -cp "$ext_lib" -d "$ext_out" \
       "$TEST_SRC/external/AnnotationClassElementTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$ext_out:$ext_lib" AnnotationClassElementTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- RUNTIME-retention annotation defined outside the JDK builtins ---"
# The annotation type is compiled first, then the client is compiled
# against its class file only (-cp, no -sourcepath), so its RUNTIME
# retention must be discovered from ITS classfile's own @Retention
# meta-annotation - exactly like an externally-defined annotation such as
# JUnit's @Test.
printf "%-30s ... " "AnnotationRetentionTest"
anno_lib="$TEST_BUILD/annotest-lib"
anno_out="$TEST_BUILD/annotest-out"
rm -rf "$anno_lib" "$anno_out"
mkdir -p "$anno_lib" "$anno_out"
if "$GENESIS" -d "$anno_lib" "$TEST_SRC"/external/annotest/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$anno_lib" -d "$anno_out" \
       "$TEST_SRC/external/AnnotationRetentionTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$anno_out:$anno_lib" AnnotationRetentionTest >/dev/null 2>&1; then
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
echo "--- Same-package type vs import nested type ---"
printf "%-30s ... " "ResourceBundleControlShadowTest"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/shadowtest/Control.java" \
       "$TEST_SRC/src/java8/shadowtest/ResourceBundleControlShadowTest.java" \
       >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "NestedInterfaceExtends"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/NestedInterfaceExtends.java" \
       >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Package scan must not recurse into a file still being loaded ---"
printf "%-30s ... " "PackageScanSelfLoadCycle"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/pkgscantest/SiblingWithNestedType.java" \
       "$TEST_SRC/src/java8/pkgscantest/HasFieldOfSibling.java" \
       >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- @Override against a forward-referenced supertype in the same file ---"
printf "%-30s ... " "ForwardIfaceOverrideTest"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/overridetest/ForwardIfaceOverrideTest.java" \
       >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "ForwardSuperMethodOverrideTest"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/overridetest/ForwardSuperMethodOverrideTest.java" \
       >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Nested class implementing a classfile-loaded interface's nested type ---"
printf "%-30s ... " "NestedClassIfaceNestedType"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/pkgscantest/HandlerWithNestedType.java" \
       >/dev/null 2>&1 && \
   "$GENESIS" -source 8 -d "$TEST_BUILD" -classpath "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/pkgscantest/NestedClassImplementsInterfaceWithNestedType.java" \
       >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Varargs call as a nested argument to another call ---"
printf "%-30s ... " "VarargsForwardingNestedCall"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/varargstest/Concatenator.java" \
       "$TEST_SRC/src/java8/varargstest/VarargsForwardingNestedCallTest.java" \
       >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Chained field access through a doubly-nested generic call ---"
printf "%-30s ... " "NestedGenericFieldAccess"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/generictest/NestedGenericFieldAccessTest.java" \
       >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Array value passed as a single element to an unrelated varargs type ---"
printf "%-30s ... " "ArrayValueAsVarargsElement"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/varargstest/ArrayValueAsVarargsElementTest.java" \
       >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Circular constructor type dependency across two files (parallel batch) ---"
printf "%-30s ... " "CircularCtorDependency"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/circulartest/CircularCtorHelper.java" \
       "$TEST_SRC/src/java8/circulartest/CircularCtorDependencyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" CircularCtorDependencyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Varargs forwarded to another varargs method across files (parallel batch) ---"
printf "%-30s ... " "VarargsForwardCrossFile"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/varargstest/VarargsForwardCrossFileHelper.java" \
       "$TEST_SRC/src/java8/varargstest/VarargsForwardCrossFileTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" VarargsForwardCrossFileTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Self-referential instanceof needs package-qualified class name ---"
printf "%-30s ... " "InstanceofSelfQualify"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/instanceoftest/InstanceofSelfQualifyVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" instanceoftest.InstanceofSelfQualifyVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Cross-file nested generic type keeps its \$-separated name ---"
printf "%-30s ... " "CrossFileNestedGenericType"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/crossfilenested/CrossFileNestedGenericHelper.java" \
       "$TEST_SRC/src/java8/crossfilenested/CrossFileNestedGenericTypeTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" CrossFileNestedGenericTypeTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Cross-file enum switch keeps its constants' real ordinals ---"
printf "%-30s ... " "EnumOrdinalCrossFileSwitch"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/enumswitchcrossfile/TransportKind.java" \
       "$TEST_SRC/src/java8/enumswitchcrossfile/EnumOrdinalCrossFileSwitchVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" enumswitchcrossfile.EnumOrdinalCrossFileSwitchVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Qualified constant (Type.CONSTANT) as a switch case label ---"
printf "%-30s ... " "SwitchQualifiedConstantCaseLabel"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/switchqualifiedconst/SwitchConstants.java" \
       "$TEST_SRC/src/java8/switchqualifiedconst/SwitchQualifiedConstantCaseLabelVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" switchqualifiedconst.SwitchQualifiedConstantCaseLabelVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Cast-narrowed qualified constant as a switch case label ---"
printf "%-30s ... " "CastQualifiedConstantCaseLabel"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/castqualifiedconst/Descriptors.java" \
       "$TEST_SRC/src/java8/castqualifiedconst/CastQualifiedConstantCaseLabelVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" castqualifiedconst.CastQualifiedConstantCaseLabelVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Class literal as synchronized lock keeps its package prefix ---"
printf "%-30s ... " "PackagedClassLiteralSync"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/pkgclasslit/Foo.java" \
       "$TEST_SRC/src/java8/pkgclasslit/PackagedClassLiteralSyncVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" PackagedClassLiteralSyncVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Cross-file generic method call chained directly (Class<T> param) ---"
printf "%-30s ... " "CrossFileGenericMethodChain"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/crossfilegenericmethodchain/Registry.java" \
       "$TEST_SRC/src/java8/crossfilegenericmethodchain/CrossFileGenericMethodChainVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" crossfilegenericmethodchain.CrossFileGenericMethodChainVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Inherited field accessed via a doubly-cross-file receiver ---"
printf "%-30s ... " "InheritedCrossFileField"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/inheritedcrossfilefield/Base.java" \
       "$TEST_SRC/src/java8/inheritedcrossfilefield/Session.java" \
       "$TEST_SRC/src/java8/inheritedcrossfilefield/Mid.java" \
       "$TEST_SRC/src/java8/inheritedcrossfilefield/InheritedCrossFileFieldVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" inheritedcrossfilefield.InheritedCrossFileFieldVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Chained .name() on a cross-file enum returned by a cross-file method ---"
printf "%-30s ... " "CrossFileEnumMethodChainName"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/crossfileenummethodname/Kind.java" \
       "$TEST_SRC/src/java8/crossfileenummethodname/Question.java" \
       "$TEST_SRC/src/java8/crossfileenummethodname/Metrics.java" \
       "$TEST_SRC/src/java8/crossfileenummethodname/CrossFileEnumMethodChainNameVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" crossfileenummethodname.CrossFileEnumMethodChainNameVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Inherited member type named in a cross-file signature ---"
# Compiled into its own freshly emptied directory, from inside it, and
# never into the shared $TEST_BUILD: genesis puts both its -d directory
# and "." on the classpath, and class files left behind by an earlier
# compile let the early name-qualification pass resolve these names from
# the OLD class files, masking the bug this test covers.
printf "%-30s ... " "InheritedMemberType"
imt_out="$TEST_BUILD/inheritedmembertype-out"
rm -rf "$imt_out"
mkdir -p "$imt_out"
if (cd "$imt_out" && "$GENESIS" -source 8 -d "$imt_out" \
       "$TEST_SRC/src/java8/inheritedmembertype/base/Base.java" \
       "$TEST_SRC/src/java8/inheritedmembertype/base/Kind.java" \
       "$TEST_SRC/src/java8/inheritedmembertype/base/Tagged.java" \
       "$TEST_SRC/src/java8/inheritedmembertype/base/Mid.java" \
       "$TEST_SRC/src/java8/inheritedmembertype/lexer/Lexer.java" \
       "$TEST_SRC/src/java8/inheritedmembertype/lexer/Deep.java" \
       "$TEST_SRC/src/java8/inheritedmembertype/lexer/InheritedMemberTypeVerifyTest.java" \
       >/dev/null 2>&1) && \
   "$JAVA" -cp "$imt_out" inheritedmembertype.lexer.InheritedMemberTypeVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Cross-file call to a class-level BOUNDED type variable param ---"
# Own freshly emptied -d directory, same reason as InheritedMemberType above.
printf "%-30s ... " "GenericBoundedTypeVarErasure"
gbtv_out="$TEST_BUILD/genericboundedtypevar-out"
rm -rf "$gbtv_out"
mkdir -p "$gbtv_out"
if (cd "$gbtv_out" && "$GENESIS" -source 8 -d "$gbtv_out" \
       "$TEST_SRC/src/java8/genericboundedtypevar/Sink.java" \
       "$TEST_SRC/src/java8/genericboundedtypevar/Tok.java" \
       "$TEST_SRC/src/java8/genericboundedtypevar/User.java" \
       "$TEST_SRC/src/java8/genericboundedtypevar/GenericBoundedTypeVarErasureVerifyTest.java" \
       >/dev/null 2>&1) && \
   "$JAVA" -cp "$gbtv_out" genericboundedtypevar.GenericBoundedTypeVarErasureVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Cross-file static field assignment (Holder.field = value) ---"
printf "%-30s ... " "CrossFileStaticFieldAssign"
cfsfa_out="$TEST_BUILD/crossfilestaticfieldassign-out"
rm -rf "$cfsfa_out"
mkdir -p "$cfsfa_out"
if (cd "$cfsfa_out" && "$GENESIS" -source 8 -d "$cfsfa_out" \
       "$TEST_SRC/src/java8/crossfilestaticfieldassign/Holder.java" \
       "$TEST_SRC/src/java8/crossfilestaticfieldassign/User.java" \
       "$TEST_SRC/src/java8/crossfilestaticfieldassign/CrossFileStaticFieldAssignVerifyTest.java" \
       >/dev/null 2>&1) && \
   "$JAVA" -cp "$cfsfa_out" crossfilestaticfieldassign.CrossFileStaticFieldAssignVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Interface member class is implicitly public and static ---"
printf "%-30s ... " "InterfaceMemberClassImplicit"
imci_out="$TEST_BUILD/interfacemembertype-out"
rm -rf "$imci_out"
mkdir -p "$imci_out"
if (cd "$imci_out" && "$GENESIS" -source 8 -d "$imci_out" \
       "$TEST_SRC/src/java8/interfacemembertype/iface/Container.java" \
       "$TEST_SRC/src/java8/interfacemembertype/caller/InterfaceMemberClassImplicitPublicStaticVerifyTest.java" \
       >/dev/null 2>&1) && \
   "$JAVA" -cp "$imci_out" interfacemembertype.caller.InterfaceMemberClassImplicitPublicStaticVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "=== Results: $passed passed, $failed failed ==="
[ $failed -eq 0 ]
