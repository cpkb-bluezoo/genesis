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

printf "%-30s ... " "TypeVarArrayField"
if command -v "$JAVAC" >/dev/null 2>&1 &&
   "$JAVAC" -d "$ejc_lib" "$TEST_SRC"/external/javaclib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$ejc_lib" -d "$ejc_out" \
       "$TEST_SRC/external/TypeVarArrayFieldTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$ejc_out:$ejc_lib" TypeVarArrayFieldTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "MostSpecificOverloadClassFile"
if command -v "$JAVAC" >/dev/null 2>&1 &&
   "$JAVAC" -d "$ejc_lib" "$TEST_SRC"/external/javaclib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$ejc_lib" -d "$ejc_out" \
       "$TEST_SRC/external/MostSpecificOverloadClassFileTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$ejc_out:$ejc_lib" MostSpecificOverloadClassFileTest >/dev/null 2>&1; then
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

printf "%-30s ... " "WildcardImportParamType"
wipt_out="$TEST_BUILD/wildcard-import-param-type-out"
rm -rf "$wipt_out"
mkdir -p "$wipt_out"
if "$GENESIS" -d "$wipt_out" "$TEST_SRC/external/wcimport/server/ConnectHandler.java" \
       "$TEST_SRC/external/wcimport/Handler.java" \
       "$TEST_SRC/external/wcimport/server/WildcardImportParamTypeTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$wipt_out" wcimport.server.WildcardImportParamTypeTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "ImportShadowNestedType"
isnt_out="$TEST_BUILD/import-shadow-nested-type-out"
rm -rf "$isnt_out"
mkdir -p "$isnt_out"
if "$GENESIS" -d "$isnt_out" "$TEST_SRC/external/importshadow/other/Encoding.java" \
       "$TEST_SRC/external/importshadow/app/Marker.java" \
       "$TEST_SRC/external/importshadow/app/Props.java" \
       "$TEST_SRC/external/importshadow/app/ImportShadowNestedTypeTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$isnt_out" importshadow.app.ImportShadowNestedTypeTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "EnumSwitchStaticImportCollide"
essic_out="$TEST_BUILD/enum-switch-static-import-collide-out"
rm -rf "$essic_out"
mkdir -p "$essic_out"
if "$GENESIS" -d "$essic_out" "$TEST_SRC/external/genesislib/SharedNameConsts.java" \
       "$TEST_SRC/external/EnumSwitchStaticImportNameCollisionTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$essic_out" EnumSwitchStaticImportNameCollisionTest >/dev/null 2>&1; then
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

printf "%-30s ... " "ExternalVarargsFlag"
if "$GENESIS" -cp "$ext_lib" -d "$ext_out" \
       "$TEST_SRC/external/ExternalVarargsFlagTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$ext_out:$ext_lib" ExternalVarargsFlagTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "ExternalVarargsArrayElement"
if "$GENESIS" -cp "$ext_lib" -d "$ext_out" \
       "$TEST_SRC/external/ExternalVarargsArrayElementTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$ext_out:$ext_lib" ExternalVarargsArrayElementTest >/dev/null 2>&1; then
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

# The annotation type is NESTED inside another class and compiled first, so
# its real classfile path is "Markers$Marker.class" - the client's own
# retention lookup and type-descriptor writing must resolve that '$'
# boundary correctly, not just naively slash every dot in the dotted name.
printf "%-30s ... " "NestedAnnotationRetentionTest"
nanno_lib="$TEST_BUILD/nestedannotest-lib"
nanno_out="$TEST_BUILD/nestedannotest-out"
rm -rf "$nanno_lib" "$nanno_out"
mkdir -p "$nanno_lib" "$nanno_out"
if "$GENESIS" -d "$nanno_lib" "$TEST_SRC"/external/nestedannotest/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$nanno_lib" -d "$nanno_out" \
       "$TEST_SRC/external/NestedAnnotationRetentionTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$nanno_out:$nanno_lib" NestedAnnotationRetentionTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

# The overloaded "check" methods are compiled first, so resolving a call
# against them exercises overload resolution over CLASSFILE-loaded method
# symbols, not same-compilation-unit ones (see Checker.java's own comment
# for why that distinction matters here).
printf "%-30s ... " "PrimitiveOverloadResolutionTest"
pov_lib="$TEST_BUILD/overloadlib-lib"
pov_out="$TEST_BUILD/overloadlib-out"
rm -rf "$pov_lib" "$pov_out"
mkdir -p "$pov_lib" "$pov_out"
if "$GENESIS" -d "$pov_lib" "$TEST_SRC"/external/overloadlib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$pov_lib" -d "$pov_out" \
       "$TEST_SRC/external/PrimitiveOverloadResolutionTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$pov_out:$pov_lib" PrimitiveOverloadResolutionTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

# The annotation type is compiled first (same reason as annotest/ and
# nestedannotest/ above - a same-compilation-unit annotation type's own
# RUNTIME retention isn't reliably resolved, a separate, pre-existing gap
# not exercised or fixed here); the actual bug under test is the class
# literal VALUE's own descriptor for a NESTED type ("Outer.Nested.class"),
# declared inside the client class itself.
printf "%-30s ... " "NestedClassLiteralAnnotationTest"
ncl_lib="$TEST_BUILD/classlitlib-lib"
ncl_out="$TEST_BUILD/classlitlib-out"
rm -rf "$ncl_lib" "$ncl_out"
mkdir -p "$ncl_lib" "$ncl_out"
if "$GENESIS" -d "$ncl_lib" "$TEST_SRC"/external/classlitlib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$ncl_lib" -d "$ncl_out" \
       "$TEST_SRC/external/NestedClassLiteralAnnotationTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$ncl_out:$ncl_lib" NestedClassLiteralAnnotationTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

# The generic superclass is deliberately compiled with REAL javac (see
# BoundedBase.java's own comment for why genesis compiling it itself
# doesn't reproduce the bug), so the client's "super(...)" call must
# resolve the bounded type variable constructor parameter from a real
# classfile, exactly like javax.tools.ForwardingJavaFileManager.
printf "%-30s ... " "BoundedSuperConstructorTest"
bsc_lib="$TEST_BUILD/boundedctorlib-lib"
bsc_out="$TEST_BUILD/boundedctorlib-out"
rm -rf "$bsc_lib" "$bsc_out"
mkdir -p "$bsc_lib" "$bsc_out"
if command -v "$JAVAC" >/dev/null 2>&1 &&
   "$JAVAC" -d "$bsc_lib" "$TEST_SRC"/external/boundedctorlib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$bsc_lib" -d "$bsc_out" \
       "$TEST_SRC/external/BoundedSuperConstructorTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$bsc_out:$bsc_lib" BoundedSuperConstructorTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

# The library is compiled with REAL javac so its class file carries the generic Signature
# "Consumer<Liplib/Configurer<...>.Registry;>" a lambda's parameter type is read from.
printf "%-30s ... " "InnerOfParameterizedTypeTest"
ip_lib="$TEST_BUILD/innerofparameterizedlib-lib"
ip_out="$TEST_BUILD/innerofparameterizedlib-out"
rm -rf "$ip_lib" "$ip_out"
mkdir -p "$ip_lib" "$ip_out"
if command -v "$JAVAC" >/dev/null 2>&1 &&
   "$JAVAC" -d "$ip_lib" "$TEST_SRC"/external/innerofparameterizedlib/iplib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$ip_lib" -d "$ip_out" \
       "$TEST_SRC/external/InnerOfParameterizedTypeTest.java" >/dev/null 2>&1 &&
   "$JAVA" -Xverify:all -cp "$ip_out:$ip_lib" InnerOfParameterizedTypeTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

# The constants class is compiled with REAL javac (its ConstantValue attributes) and is NOT on
# the class path the client is run with: the constants must be in the client's own code.
printf "%-30s ... " "ClassFileConstantInliningTest"
cc_lib="$TEST_BUILD/constantlib-lib"
cc_out="$TEST_BUILD/constantlib-out"
rm -rf "$cc_lib" "$cc_out"
mkdir -p "$cc_lib" "$cc_out"
if command -v "$JAVAC" >/dev/null 2>&1 &&
   "$JAVAC" -d "$cc_lib" "$TEST_SRC"/external/constantlib/constlib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$cc_lib" -d "$cc_out" \
       "$TEST_SRC/external/ClassFileConstantInliningTest.java" >/dev/null 2>&1 &&
   "$JAVA" -Xverify:all -cp "$cc_out" ClassFileConstantInliningTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

# The library is compiled by genesis itself: the ConstantValue it writes is what the client inlines.
printf "%-30s ... " "EmbeddedNulConstantValueTest"
nc_lib="$TEST_BUILD/nulconstlib-lib"
nc_out="$TEST_BUILD/nulconstlib-out"
rm -rf "$nc_lib" "$nc_out"
mkdir -p "$nc_lib" "$nc_out"
if "$GENESIS" -d "$nc_lib" "$TEST_SRC"/external/nulconstlib/nulconst/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$nc_lib" -d "$nc_out" \
       "$TEST_SRC/external/EmbeddedNulConstantValueTest.java" >/dev/null 2>&1 &&
   "$JAVA" -Xverify:all -cp "$nc_out" EmbeddedNulConstantValueTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

# The interface is deliberately compiled with REAL javac (see
# ExternalStringConstants.java's own comment), so the client's String
# switch case labels must resolve a classfile-loaded String constant's
# own ConstantValue attribute, exactly like jakarta.servlet's
# HttpServletRequest.DIGEST_AUTH/BASIC_AUTH.
printf "%-30s ... " "StringSwitchExternalConstantTest"
sse_lib="$TEST_BUILD/strswitchlib-lib"
sse_out="$TEST_BUILD/strswitchlib-out"
rm -rf "$sse_lib" "$sse_out"
mkdir -p "$sse_lib" "$sse_out"
if command -v "$JAVAC" >/dev/null 2>&1 &&
   "$JAVAC" -d "$sse_lib" "$TEST_SRC"/external/strswitchlib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$sse_lib" -d "$sse_out" \
       "$TEST_SRC/external/StringSwitchExternalConstantTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$sse_out:$sse_lib" StringSwitchExternalConstantTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

# The library is compiled with genesis itself, into a separate classfile
# (see protectedclasslib/Base.java's own comment) - the bug is in the
# nested class's own access_flags, produced by genesis's codegen
# regardless of same-batch vs. separate-invocation compilation.
printf "%-30s ... " "ProtectedNestedClassAccessTest"
pnc_lib="$TEST_BUILD/protectedclasslib-lib"
pnc_out="$TEST_BUILD/protectedclasslib-out"
rm -rf "$pnc_lib" "$pnc_out"
mkdir -p "$pnc_lib" "$pnc_out"
if "$GENESIS" -d "$pnc_lib" "$TEST_SRC"/external/protectedclasslib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$pnc_lib" -d "$pnc_out" \
       "$TEST_SRC/external/ProtectedNestedClassAccessTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$pnc_out:$pnc_lib" ProtectedNestedClassAccessTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

# The library is compiled with genesis itself, into a separate classfile
# (see protectedfieldlib/Base.java's own comment) - the bug is in codegen's
# own decision of which class hosts the field access, independent of
# same-batch vs. separate-invocation compilation.
printf "%-30s ... " "ProtectedFieldViaNestedClassTest"
pfl_lib="$TEST_BUILD/protectedfieldlib-lib"
pfl_out="$TEST_BUILD/protectedfieldlib-out"
rm -rf "$pfl_lib" "$pfl_out"
mkdir -p "$pfl_lib" "$pfl_out"
if "$GENESIS" -d "$pfl_lib" "$TEST_SRC"/external/protectedfieldlib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$pfl_lib" -d "$pfl_out" \
       "$TEST_SRC/external/ProtectedFieldViaNestedClassTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$pfl_out:$pfl_lib" ProtectedFieldViaNestedClassTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

# The library is compiled with genesis itself, into a separate classfile
# (see protectedmethodreceiverlib/Base.java's own comment) - the bug is in
# semantic.c's own receiver resolution, independent of same-batch vs.
# separate-invocation compilation.
printf "%-30s ... " "ProtectedMethodReceiverViaNestedClassTest"
pmr_lib="$TEST_BUILD/protectedmethodreceiverlib-lib"
pmr_out="$TEST_BUILD/protectedmethodreceiverlib-out"
rm -rf "$pmr_lib" "$pmr_out"
mkdir -p "$pmr_lib" "$pmr_out"
if "$GENESIS" -d "$pmr_lib" "$TEST_SRC"/external/protectedmethodreceiverlib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$pmr_lib" -d "$pmr_out" \
       "$TEST_SRC/external/ProtectedMethodReceiverViaNestedClassTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$pmr_out:$pmr_lib" ProtectedMethodReceiverViaNestedClassTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

# GitHub issue #2: the library is compiled with genesis itself, into a
# separate classfile - the two-step shape that exercises reading a
# constant's value back from its own classfile ConstantValue attribute,
# as opposed to same-batch compilation, where the source AST's own
# literal initializer is still available regardless.
printf "%-30s ... " "SwitchClassfileConstantTest"
scf_lib="$TEST_BUILD/switchclassfileconstlib-lib"
scf_out="$TEST_BUILD/switchclassfileconstlib-out"
rm -rf "$scf_lib" "$scf_out"
mkdir -p "$scf_lib" "$scf_out"
if "$GENESIS" -d "$scf_lib" "$TEST_SRC"/external/switchclassfileconstlib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$scf_lib" -d "$scf_out" \
       "$TEST_SRC/external/SwitchClassfileConstantTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$scf_out:$scf_lib" SwitchClassfileConstantTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

# Sub.Mode is Base.Mode. The caller is analyzed before the class that
# declares getMode(), so the return type is resolved from the other file's
# stub. Use.java is listed first on purpose.
printf "%-30s ... " "InheritedMemberTypeCallTest"
im_lib="$TEST_BUILD/inhm-lib"
im_out="$TEST_BUILD/inhm-out"
rm -rf "$im_lib" "$im_out"
mkdir -p "$im_lib" "$im_out"
if "$GENESIS" -d "$im_lib" "$TEST_SRC"/external/inhm/Base.java "$TEST_SRC"/external/inhm/Sub.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$im_lib" -d "$im_out" \
       "$TEST_SRC/external/inhm/Use.java" "$TEST_SRC/external/inhm/Holder.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$im_out:$im_lib" p.Use >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

# The library field is Integer.MAX_VALUE - 5. Its ConstantValue is what the
# annotation in a later compilation inlines.
printf "%-30s ... " "AnnotationConstantFoldTest"
ac_lib="$TEST_BUILD/annconst-lib"
ac_out="$TEST_BUILD/annconst-out"
rm -rf "$ac_lib" "$ac_out"
mkdir -p "$ac_lib" "$ac_out"
if "$GENESIS" -d "$ac_lib" "$TEST_SRC"/external/annconstlib/Precedence.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$ac_lib" -d "$ac_out" \
       "$TEST_SRC/external/AnnotationConstantFoldTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$ac_out:$ac_lib" AnnotationConstantFoldTest >/dev/null 2>&1; then
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
echo "--- Named String constant (bare or qualified) as a switch case label ---"
printf "%-30s ... " "StringSwitchQualifiedConstantCaseLabel"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/stringswitchconst/StringConstants.java" \
       "$TEST_SRC/src/java8/stringswitchconst/StringSwitchQualifiedConstantCaseLabelVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" stringswitchconst.StringSwitchQualifiedConstantCaseLabelVerifyTest >/dev/null 2>&1; then
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
echo "--- Static interface factory method reference (cross-file) ---"
printf "%-30s ... " "MethodRefStaticInterfaceFactory"
if "$GENESIS" -source 8 -d "$TEST_BUILD" -sourcepath "$TEST_SRC/src/java8" \
       "$TEST_SRC/src/java8/methodreffactory/HttpComponentsClientHttpRequestFactoryBuilder.java" \
       "$TEST_SRC/src/java8/methodreffactory/ClientHttpRequestFactoryBuilder.java" \
       "$TEST_SRC/src/java8/methodreffactory/MethodRefStaticInterfaceFactoryVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" methodreffactory.MethodRefStaticInterfaceFactoryVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Binder ctor: varargs over Iterable for single source ---"
printf "%-30s ... " "BinderVarargsOverIterable"
if "$GENESIS" -source 8 -d "$TEST_BUILD" \
       "$TEST_SRC/src/java8/BinderVarargsOverIterableVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" BinderVarargsOverIterableVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "BinderVarargsMockPropertySource"
if "$GENESIS" -source 8 -d "$TEST_BUILD" \
       "$TEST_SRC/src/java8/BinderVarargsMockPropertySourceVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" BinderVarargsMockPropertySourceVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "IterableOverVarargsContainsOnlyKeys"
if "$GENESIS" -source 8 -d "$TEST_BUILD" \
       "$TEST_SRC/src/java8/IterableOverVarargsContainsOnlyKeysVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" IterableOverVarargsContainsOnlyKeysVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Generic field chained access (checkcast before 2nd getfield) ---"
printf "%-30s ... " "GenericFieldChainedAccess"
if "$GENESIS" -source 8 -d "$TEST_BUILD" \
       "$TEST_SRC/src/java8/GenericFieldChainedAccessVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" GenericFieldChainedAccessVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Paths::get overload for String SAM (not URI) ---"
printf "%-30s ... " "PathsGetMethodRefOverload"
if "$GENESIS" -source 8 -d "$TEST_BUILD" \
       "$TEST_SRC/src/java8/PathsGetMethodRefOverloadVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" PathsGetMethodRefOverloadVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Class literal overload (Class vs Object) ---"
printf "%-30s ... " "ClassLiteralOverload"
if "$GENESIS" -source 8 -d "$TEST_BUILD" \
       "$TEST_SRC/src/java8/ClassLiteralOverloadVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" ClassLiteralOverloadVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Lambda in anonymous class captures outer locals (invokedynamic args) ---"
printf "%-30s ... " "LambdaInAnonymousCapturesLocal"
if "$GENESIS" -source 8 -d "$TEST_BUILD" \
       "$TEST_SRC/src/java8/LambdaInAnonymousClassCapturesLocalVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" LambdaInAnonymousClassCapturesLocalVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Supplier<E extends bound> block lambda SAM return (Flyway) ---"
printf "%-30s ... " "LambdaSupplierGenericTypeVar"
if "$GENESIS" -source 8 -d "$TEST_BUILD" \
       "$TEST_SRC/src/java8/LambdaSupplierGenericTypeVarVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" LambdaSupplierGenericTypeVarVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Flyway Extension: local shadows field in Supplier lambda ---"
printf "%-30s ... " "FlywayExtensionSupplierBlock"
if "$GENESIS" -source 8 -d "$TEST_BUILD" \
       "$TEST_SRC/src/java8/FlywayExtensionSupplierBlockVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" FlywayExtensionSupplierBlockVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Zero-arg generic varargs witness (ArgumentCaptor.captor) ---"
printf "%-30s ... " "GenericVarargsWitness"
if "$GENESIS" -source 8 -d "$TEST_BUILD" \
       "$TEST_SRC/src/java8/GenericVarargsWitnessVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" GenericVarargsWitnessVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Anonymous super ctor int to long widening ---"
printf "%-30s ... " "AnonSuperLongCtor"
if "$GENESIS" -source 8 -d "$TEST_BUILD" \
       "$TEST_SRC/src/java8/AnonSuperLongCtorVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" AnonSuperLongCtorVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Constructor ref to Supplier (ArrayList::new) ---"
printf "%-30s ... " "SupplierArrayListNew"
if "$GENESIS" -source 8 -d "$TEST_BUILD" \
       "$TEST_SRC/src/java8/SupplierArrayListNewVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" SupplierArrayListNewVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Method ref picks varargs flatExtracting overload ---"
printf "%-30s ... " "FlatExtractingMethodRefOverload"
if "$GENESIS" -source 8 -d "$TEST_BUILD" \
       "$TEST_SRC/src/java8/FlatExtractingMethodRefOverloadVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" FlatExtractingMethodRefOverloadVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "BoundMethodRefErasure"
if "$JAVAC" -source 8 -target 8 -d "$TEST_BUILD" \
       "$TEST_SRC/src/java8/selfspec/SelfSpec.java" >/dev/null 2>&1 && \
   "$GENESIS" -source 8 -cp "$TEST_BUILD" -d "$TEST_BUILD" \
       "$TEST_SRC/src/java8/selfspec/BoundMethodRefErasureVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" BoundMethodRefErasureVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "KeyValuesOfEntryMethodRef"
if MICROMETER_COMMONS_JAR="$(find "$HOME/.cache/genesis-bench/maven/io/micrometer" -name 'micrometer-commons-*.jar' 2>/dev/null | sort -V | tail -1)" && \
   [ -n "$MICROMETER_COMMONS_JAR" ] && \
   "$GENESIS" -source 8 -cp "$MICROMETER_COMMONS_JAR" -d "$TEST_BUILD" \
       "$TEST_SRC/src/java8/micrometer/KeyValuesOfEntryMethodRefVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD:$MICROMETER_COMMONS_JAR" KeyValuesOfEntryMethodRefVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "EntryMethodRefFunction"
if "$GENESIS" -source 8 -d "$TEST_BUILD" \
       "$TEST_SRC/src/java8/EntryMethodRefFunctionVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" EntryMethodRefFunctionVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Method ref SAM return stays Object ---"
printf "%-30s ... " "MethodRefFlatExtractObjectReturn"
if "$GENESIS" -source 8 -d "$TEST_BUILD" \
       "$TEST_SRC/src/java8/MethodRefFlatExtractObjectReturnVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" MethodRefFlatExtractObjectReturnVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Generic superclass field chain (checkcast before getfield) ---"
printf "%-30s ... " "GenericActualFieldChain"
if "$GENESIS" -source 8 -d "$TEST_BUILD" \
       "$TEST_SRC/src/java8/GenericActualFieldChainVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$TEST_BUILD" GenericActualFieldChainVerifyTest >/dev/null 2>&1; then
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
echo "--- @Override of a cross-file method with a primitive varargs parameter ---"
printf "%-30s ... " "PrimitiveVarargsOverride"
pvo_out="$TEST_BUILD/primitivevarargsoverride-out"
rm -rf "$pvo_out"
mkdir -p "$pvo_out"
if "$GENESIS" -source 8 -d "$pvo_out" \
       "$TEST_SRC/src/java8/primitivevarargsoverride/Base.java" \
       "$TEST_SRC/src/java8/primitivevarargsoverride/PrimitiveVarargsOverrideVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$pvo_out" primitivevarargsoverride.PrimitiveVarargsOverrideVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Varargs parameter whose element type is itself an array, cross-file ---"
printf "%-30s ... " "VarargsArrayElement"
vae_out="$TEST_BUILD/varargsarrayelement-out"
rm -rf "$vae_out"
mkdir -p "$vae_out"
if "$GENESIS" -source 8 -d "$vae_out" \
       "$TEST_SRC/src/java8/varargsarrayelement/Session.java" \
       "$TEST_SRC/src/java8/varargsarrayelement/Impl.java" \
       "$TEST_SRC/src/java8/varargsarrayelement/VarargsArrayElementVerifyTest.java" \
       >/dev/null 2>&1 && \
   "$JAVA" -cp "$vae_out" varargsarrayelement.VarargsArrayElementVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Classes reached only through -sourcepath ---"
# One file on the command line; everything it uses lives under a separate
# -sourcepath root and must be both resolved and compiled from there.
# Fresh output directory, and run with ONLY that directory on the class
# path, so the dependencies' class files have to have been written too.
# check_sourcepath <label> <file relative to test/sourcepath/src> <main class>
check_sourcepath() {
    printf "%-30s ... " "$1"
    sp_out="$TEST_BUILD/sourcepath-out"
    rm -rf "$sp_out"
    mkdir -p "$sp_out"
    if (cd "$sp_out" && "$GENESIS" -source 8 -d "$sp_out" \
           -sourcepath "$TEST_SRC/sourcepath/lib" \
           "$TEST_SRC/sourcepath/src/$2" >/dev/null 2>&1) && \
       "$JAVA" -cp "$sp_out" "$3" >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
}
check_sourcepath SamePackageSourcepath spdep/SamePackageSourcepathTest.java spdep.SamePackageSourcepathTest
check_sourcepath ImportSourcepath spuser/ImportSourcepathTest.java spuser.ImportSourcepathTest

echo
echo "--- A qualified reference to a genuinely nonexistent package must fail to compile ---"
# "pkg2.Foo" doesn't exist anywhere under this sourcepath - only an
# UNRELATED default-package Foo.java shares its simple name. This must be
# rejected at compile time (matching real javac's "package pkg2 does not
# exist"), not silently resolved to the unrelated class and left to fail
# at runtime with NoClassDefFoundError instead.
printf "%-30s ... " "DefaultPackageFallbackRejected"
dpf_out="$TEST_BUILD/defaultpackagefallback-out"
rm -rf "$dpf_out"
mkdir -p "$dpf_out"
if ! "$GENESIS" -sourcepath "$TEST_SRC/external/defaultpackagefallback" -d "$dpf_out" \
       "$TEST_SRC/external/defaultpackagefallback/pkg1/UsesNonExistentQualifiedClass.java" >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Annotation nested in an imported, unrelated same-batch class ---"
# Outer (and its nested @interface Marker) and the class using "@Marker"
# via "import ...lib.Outer;" are compiled together, in the same batch - no
# classfile exists yet for either. Own freshly emptied -d directory, same
# reason as InheritedMemberType above.
printf "%-30s ... " "ImportedAnnotationRetention"
iar_out="$TEST_BUILD/importedannotest-out"
rm -rf "$iar_out"
mkdir -p "$iar_out"
if (cd "$iar_out" && "$GENESIS" -source 8 -d "$iar_out" \
       "$TEST_SRC/src/java8/importedannotest/lib/Outer.java" \
       "$TEST_SRC/src/java8/importedannotest/main/ImportedAnnotationRetentionVerifyTest.java" \
       >/dev/null 2>&1) && \
   "$JAVA" -cp "$iar_out" importedannotest.main.ImportedAnnotationRetentionVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Class-file nested type extending its own outer type ---"
# The library is compiled first, on its own, so that the test sees Shape,
# Shape$Circle, Registration and Registration$Dynamic as class files. The
# test names each outer type before its nested type; the nested type must
# still inherit the outer type's members.
printf "%-30s ... " "NestedExtendsOuter"
neo_lib="$TEST_BUILD/nested-extends-outer-lib"
neo_out="$TEST_BUILD/nested-extends-outer-out"
rm -rf "$neo_lib" "$neo_out"
mkdir -p "$neo_lib" "$neo_out"
if "$GENESIS" -d "$neo_lib" "$TEST_SRC"/external/nestedouter/lib/*.java >/dev/null 2>&1 &&
   (cd "$neo_out" && "$GENESIS" -cp "$neo_lib" -d "$neo_out" \
       "$TEST_SRC/external/NestedExtendsOuterTest.java" >/dev/null 2>&1) &&
   "$JAVA" -cp "$neo_out:$neo_lib" NestedExtendsOuterTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Class-file nested type met before its outer type ---"
# Two files in one batch, in this order: the first names only the nested
# type FileObject.Kind (so it is loaded on its own), the second then needs
# it as a member of FileObject, inherited through a class file superclass.
# The symbols of class-file classes are shared by the files of a batch, so
# what the first file's analysis loaded is what the second one sees.
printf "%-30s ... " "NestedBeforeOuter"
nbo_out="$TEST_BUILD/nested-before-outer-out"
rm -rf "$nbo_out"
mkdir -p "$nbo_out"
if (cd "$nbo_out" && "$GENESIS" -j1 -cp "$neo_lib" -d "$nbo_out" \
       "$TEST_SRC/external/nestedouter/use/KindUser.java" \
       "$TEST_SRC/external/nestedouter/use/NestedBeforeOuterTest.java" >/dev/null 2>&1) &&
   "$JAVA" -cp "$nbo_out:$neo_lib" nestedouter.use.NestedBeforeOuterTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Field of a field inherited from a class-file superclass ---"
# The library is compiled first, on its own, so that the test inherits
# "kind" and "holder" from a class file. Own freshly emptied -d directory
# and working directory, same reason as InheritedMemberType above.
printf "%-30s ... " "InheritedFieldChain"
ifc_lib="$TEST_BUILD/inherited-field-chain-lib"
ifc_out="$TEST_BUILD/inherited-field-chain-out"
rm -rf "$ifc_lib" "$ifc_out"
mkdir -p "$ifc_lib" "$ifc_out"
if "$GENESIS" -d "$ifc_lib" "$TEST_SRC"/external/fieldchain/lib/*.java >/dev/null 2>&1 &&
   (cd "$ifc_out" && "$GENESIS" -cp "$ifc_lib" -d "$ifc_out" \
       "$TEST_SRC/external/InheritedFieldChainTest.java" >/dev/null 2>&1) &&
   "$JAVA" -cp "$ifc_out:$ifc_lib" InheritedFieldChainTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Wildcard-imported type vs nested type of an unrelated single-type import ---"
printf "%-30s ... " "NestedVsWildcardImport"
nvw_out="$TEST_BUILD/nestedshadow-out"
rm -rf "$nvw_out"
mkdir -p "$nvw_out"
if (cd "$nvw_out" && "$GENESIS" -source 8 -d "$nvw_out" \
       "$TEST_SRC/src/java8/nestedshadow/lib/Filter.java" \
       "$TEST_SRC/src/java8/nestedshadow/main/NestedVsWildcardImportVerifyTest.java" \
       >/dev/null 2>&1) && \
   "$JAVA" -cp "$nvw_out" nestedshadow.main.NestedVsWildcardImportVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Nested class implementing a member type its enclosing class inherits ---"
printf "%-30s ... " "NestedImplementsInherited"
nii_out="$TEST_BUILD/nestedimplinherited-out"
rm -rf "$nii_out"
mkdir -p "$nii_out"
if (cd "$nii_out" && "$GENESIS" -source 8 -d "$nii_out" \
       "$TEST_SRC/src/java8/nestedimplinherited/p/Transport.java" \
       "$TEST_SRC/src/java8/nestedimplinherited/p/NestedImplementsInheritedMemberTypeVerifyTest.java" \
       >/dev/null 2>&1) && \
   "$JAVA" -cp "$nii_out" nestedimplinherited.p.NestedImplementsInheritedMemberTypeVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Class declared in an interface, used from its class file ---"
printf "%-30s ... " "InterfaceMemberClassFromClassfile"
imc_lib="$TEST_BUILD/ifacememberlib-lib"
imc_out="$TEST_BUILD/ifacememberlib-out"
rm -rf "$imc_lib" "$imc_out"
mkdir -p "$imc_lib" "$imc_out"
if "$GENESIS" -d "$imc_lib" "$TEST_SRC"/external/ifacememberlib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$imc_lib" -d "$imc_out" \
       "$TEST_SRC/external/InterfaceMemberClassFromClassfileTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$imc_out:$imc_lib" InterfaceMemberClassFromClassfileTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Bridge for a generic parent interface loaded from a class file ---"
printf "%-30s ... " "IndirectGenericBridgeClassfile"
igb_lib="$TEST_BUILD/indirectbridgelib-lib"
igb_out="$TEST_BUILD/indirectbridgelib-out"
rm -rf "$igb_lib" "$igb_out"
mkdir -p "$igb_lib" "$igb_out"
if "$GENESIS" -d "$igb_lib" "$TEST_SRC"/external/indirectbridgelib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$igb_lib" -d "$igb_out" \
       "$TEST_SRC/external/IndirectGenericBridgeFromClassfileTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$igb_out:$igb_lib" IndirectGenericBridgeFromClassfileTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Covariant override bridge when superclass came from a class file ---"
printf "%-30s ... " "PushCovariantOverrideBridge"
pcov_lib="$TEST_BUILD/pushcovlib-lib"
pcov_out="$TEST_BUILD/pushcovlib-out"
rm -rf "$pcov_lib" "$pcov_out"
mkdir -p "$pcov_lib" "$pcov_out"
if "$GENESIS" -d "$pcov_lib" "$TEST_SRC"/external/pushcovlib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$pcov_lib" -d "$pcov_out" \
       "$TEST_SRC/external/pushcovlib/MidTests.java" \
       "$TEST_SRC/external/pushcovlib/LeafTests.java" \
       "$TEST_SRC/external/PushCovariantOverrideBridgeFromClassfileTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$pcov_out:$pcov_lib" PushCovariantOverrideBridgeFromClassfileTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Generic method with a bounded type variable, called from another file ---"
printf "%-30s ... " "BoundedMethodTypeVarCrossFile"
bmt_out="$TEST_BUILD/boundedmethodtypevar-out"
rm -rf "$bmt_out"
mkdir -p "$bmt_out"
if (cd "$bmt_out" && "$GENESIS" -source 8 -d "$bmt_out" \
       "$TEST_SRC/src/java8/boundedmethodtypevar/Maker.java" \
       "$TEST_SRC/src/java8/boundedmethodtypevar/BoundedMethodTypeVarCrossFileVerifyTest.java" \
       >/dev/null 2>&1) && \
   "$JAVA" -cp "$bmt_out" boundedmethodtypevar.BoundedMethodTypeVarCrossFileVerifyTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- outer.new Inner(...) where Inner comes from a class file ---"
printf "%-30s ... " "InnerCtorFromClassfile"
ic_lib="$TEST_BUILD/innerctorlib-lib"
ic_out="$TEST_BUILD/innerctorlib-out"
rm -rf "$ic_lib" "$ic_out"
mkdir -p "$ic_lib" "$ic_out"
if "$GENESIS" -d "$ic_lib" "$TEST_SRC"/external/innerctorlib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$ic_lib" -d "$ic_out" \
       "$TEST_SRC/external/InnerCtorFromClassfileTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$ic_out:$ic_lib" InnerCtorFromClassfileTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Every annotation element value kind, in uses and in defaults ---"
printf "%-30s ... " "AnnotationElementKinds"
aek_lib="$TEST_BUILD/annoelementslib-lib"
aek_out="$TEST_BUILD/annoelementslib-out"
rm -rf "$aek_lib" "$aek_out"
mkdir -p "$aek_lib" "$aek_out"
if "$GENESIS" -d "$aek_lib" "$TEST_SRC"/external/annoelementslib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$aek_lib" -d "$aek_out" \
       "$TEST_SRC/external/AnnotationElementKindsTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$aek_out:$aek_lib" AnnotationElementKindsTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Switch case labels that are constants defined through another class's constant ---"
printf "%-30s ... " "StringConstantChainSwitch"
scc_lib="$TEST_BUILD/stringconstchainlib-lib"
scc_out="$TEST_BUILD/stringconstchainlib-out"
rm -rf "$scc_lib" "$scc_out"
mkdir -p "$scc_lib" "$scc_out"
if "$GENESIS" -d "$scc_lib" "$TEST_SRC"/external/stringconstchainlib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$scc_lib" -d "$scc_out" \
       "$TEST_SRC/external/StringConstantChainSwitchTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$scc_out:$scc_lib" StringConstantChainSwitchTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Narrowing an int constant from a class file (byte/short/char) ---"
printf "%-30s ... " "NarrowingConstantClassfile"
nc_lib="$TEST_BUILD/narrowconstlib-lib"
nc_out="$TEST_BUILD/narrowconstlib-out"
rm -rf "$nc_lib" "$nc_out"
mkdir -p "$nc_lib" "$nc_out"
if "$GENESIS" -d "$nc_lib" "$TEST_SRC"/external/narrowconstlib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$nc_lib" -d "$nc_out" \
       "$TEST_SRC/external/NarrowingConstantFromClassfileTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$nc_out:$nc_lib" NarrowingConstantFromClassfileTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Inner class calling a method its outer class inherits through a generic class-file chain ---"
printf "%-30s ... " "InnerCallThroughGenericSuper"
ig_lib="$TEST_BUILD/genericsuperlib-lib"
ig_out="$TEST_BUILD/genericsuperlib-out"
rm -rf "$ig_lib" "$ig_out"
mkdir -p "$ig_lib" "$ig_out"
if "$GENESIS" -d "$ig_lib" "$TEST_SRC"/external/genericsuperlib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$ig_lib" -d "$ig_out" \
       "$TEST_SRC/external/InnerCallThroughGenericSuperTest.java" \
       "$TEST_SRC"/external/genericsuperclient/*.java >/dev/null 2>&1 &&
   "$JAVA" -cp "$ig_out:$ig_lib" InnerCallThroughGenericSuperTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- A lambda passed to a method of a generic class-file type a generic call returned ---"
printf "%-30s ... " "ChainedGenericCallbackLambda"
cb_lib="$TEST_BUILD/callbackslib-lib"
cb_out="$TEST_BUILD/callbackslib-out"
rm -rf "$cb_lib" "$cb_out"
mkdir -p "$cb_lib" "$cb_out"
if "$GENESIS" -d "$cb_lib" "$TEST_SRC"/external/callbackslib/callbackslib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$cb_lib" -d "$cb_out" \
       "$TEST_SRC/external/ChainedGenericCallbackLambdaTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$cb_out:$cb_lib" ChainedGenericCallbackLambdaTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- A chain of calls on a self-typed class-file API reached through a wildcard receiver ---"
printf "%-30s ... " "WildcardSelfTypeChain"
st_lib="$TEST_BUILD/selftypelib-lib"
st_out="$TEST_BUILD/selftypelib-out"
rm -rf "$st_lib" "$st_out"
mkdir -p "$st_lib" "$st_out"
if "$GENESIS" -d "$st_lib" "$TEST_SRC"/external/selftypelib/selftypelib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$st_lib" -d "$st_out" \
       "$TEST_SRC/external/WildcardSelfTypeChainTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$st_out:$st_lib" WildcardSelfTypeChainTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- A generic call returning its bare type variable, passed as an argument, types its lambda ---"
printf "%-30s ... " "ArgumentMatcherLambda"
am_lib="$TEST_BUILD/argmatcherlib-lib"
am_out="$TEST_BUILD/argmatcherlib-out"
rm -rf "$am_lib" "$am_out"
mkdir -p "$am_lib" "$am_out"
if "$GENESIS" -d "$am_lib" "$TEST_SRC"/external/argmatcherlib/argmatcherlib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$am_lib" -d "$am_out" \
       "$TEST_SRC/external/ArgumentMatcherLambdaTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$am_out:$am_lib" ArgumentMatcherLambdaTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- A method variable bounded by a class variable, reached through a wildcard ---"
printf "%-30s ... " "MethodVariableBoundByClassVar"
mv_lib="$TEST_BUILD/methodvarbound-lib"
mv_out="$TEST_BUILD/methodvarbound-out"
rm -rf "$mv_lib" "$mv_out"
mkdir -p "$mv_lib" "$mv_out"
if "$GENESIS" -d "$mv_lib" "$TEST_SRC"/external/selftypelib/selftypelib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$mv_lib" -d "$mv_out" \
       "$TEST_SRC/external/MethodVariableBoundByClassVariableTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$mv_out:$mv_lib" MethodVariableBoundByClassVariableTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- A type variable fixed through the supertypes of the argument's class ---"
printf "%-30s ... " "SubtypeArgumentInference"
sa_lib="$TEST_BUILD/providerlib-lib"
sa_out="$TEST_BUILD/providerlib-out"
rm -rf "$sa_lib" "$sa_out"
mkdir -p "$sa_lib" "$sa_out"
if "$GENESIS" -d "$sa_lib" "$TEST_SRC"/external/providerlib/providerlib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$sa_lib" -d "$sa_out" \
       "$TEST_SRC/external/SubtypeArgumentInferenceTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$sa_out:$sa_lib" SubtypeArgumentInferenceTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- An Object result of a non-generic interface is not a type argument of its subinterface ---"
printf "%-30s ... " "ObjectReturnOfNonGenericIface"
or_lib="$TEST_BUILD/objectreturnlib-lib"
or_out="$TEST_BUILD/objectreturnlib-out"
rm -rf "$or_lib" "$or_out"
mkdir -p "$or_lib" "$or_out"
if "$GENESIS" -d "$or_lib" "$TEST_SRC"/external/objectreturnlib/objectreturnlib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$or_lib" -d "$or_out" \
       "$TEST_SRC/external/ObjectReturnOfNonGenericInterfaceTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$or_out:$or_lib" ObjectReturnOfNonGenericInterfaceTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- A single-type import of a nested class beats another class's nested type of the same name ---"
printf "%-30s ... " "ImportedNestedReceiver"
in_out="$TEST_BUILD/importednested-out"
rm -rf "$in_out"
mkdir -p "$in_out"
if (cd "$in_out" && "$GENESIS" -j1 -d "$in_out" \
       "$TEST_SRC/external/importednested/p/G.java" \
       "$TEST_SRC/external/importednested/p/F.java" \
       "$TEST_SRC/external/importednested/p/ImportedNestedReceiverTest.java" >/dev/null 2>&1) &&
   "$JAVA" -cp "$in_out" p.ImportedNestedReceiverTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- Overloaded interface default method called on a new-expression receiver ---"
printf "%-30s ... " "InterfaceOverloadNewReceiver"
io_lib="$TEST_BUILD/ifaceoverloadlib-lib"
io_out="$TEST_BUILD/ifaceoverloadlib-out"
rm -rf "$io_lib" "$io_out"
mkdir -p "$io_lib" "$io_out"
if "$GENESIS" -d "$io_lib" "$TEST_SRC"/external/ifaceoverloadlib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$io_lib" -d "$io_out" \
       "$TEST_SRC/external/InterfaceOverloadOnNewReceiverTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$io_out:$io_lib" InterfaceOverloadOnNewReceiverTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo
echo "--- javac options a build passes are accepted (and ignored), on the command line and in an @argfile ---"
printf "%-30s ... " "JavacOptionsAccepted"
jo_out="$TEST_BUILD/javacoptions-out"
jo_args="$TEST_BUILD/javacoptions.args"
rm -rf "$jo_out"
mkdir -p "$jo_out/a" "$jo_out/b"
printf '%s\n' -encoding UTF-8 -parameters -proc:none -Xlint:unchecked -deprecation \
    -d "$jo_out/b" "$TEST_SRC/external/javacoptions/Hello.java" >"$jo_args"
jo_msgs=$("$GENESIS" -encoding UTF-8 -parameters -proc:none -Xlint:all -deprecation -Xmaxerrs 5 \
              -implicit:none -d "$jo_out/a" "$TEST_SRC/external/javacoptions/Hello.java" 2>&1;
          "$GENESIS" "@$jo_args" 2>&1)
if [ -z "$jo_msgs" ] &&
   "$JAVA" -cp "$jo_out/a" Hello >/dev/null 2>&1 &&
   "$JAVA" -cp "$jo_out/b" Hello >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    echo "$jo_msgs" | head -3
    failed=$((failed + 1))
fi

echo
echo "--- Records declared in another file of the batch (accessors, constructors) ---"
for rc_order in "RecordCrossFileTest Resource Named" "Named RecordCrossFileTest Resource" "Resource Named RecordCrossFileTest"; do
    set -- $rc_order
    printf "%-30s ... " "RecordCrossFile ($1 first)"
    rc_out="$TEST_BUILD/recordcrossfile-out"
    rm -rf "$rc_out"
    mkdir -p "$rc_out"
    if (cd "$rc_out" && "$GENESIS" -j1 -d "$rc_out" \
           "$TEST_SRC/external/recordcrossfile/p/$1.java" \
           "$TEST_SRC/external/recordcrossfile/p/$2.java" \
           "$TEST_SRC/external/recordcrossfile/p/$3.java" >/dev/null 2>&1) &&
       "$JAVA" -cp "$rc_out" p.RecordCrossFileTest >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

echo
echo "--- Type::new for a class in another file that declares no constructor ---"
for dc_order in "DefaultCtorRefTest Conv" "Conv DefaultCtorRefTest"; do
    set -- $dc_order
    printf "%-30s ... " "DefaultCtorRef ($1 first)"
    dc_out="$TEST_BUILD/defaultctorref-out"
    rm -rf "$dc_out"
    mkdir -p "$dc_out"
    if (cd "$dc_out" && "$GENESIS" -j1 -d "$dc_out" \
           "$TEST_SRC/external/defaultctorref/p/$1.java" \
           "$TEST_SRC/external/defaultctorref/p/$2.java" >/dev/null 2>&1) &&
       "$JAVA" -cp "$dc_out" p.DefaultCtorRefTest >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

echo
echo "--- A varargs method of another file, called with an array, still returns its own type ---"
for vr_order in "VarargsReturnTest SP" "SP VarargsReturnTest"; do
    set -- $vr_order
    printf "%-30s ... " "VarargsReturn ($1 first)"
    vr_out="$TEST_BUILD/varargsreturn-out"
    rm -rf "$vr_out"
    mkdir -p "$vr_out"
    if (cd "$vr_out" && "$GENESIS" -j1 -d "$vr_out" \
           "$TEST_SRC/external/varargsreturn/p/$1.java" \
           "$TEST_SRC/external/varargsreturn/p/$2.java" >/dev/null 2>&1) &&
       "$JAVA" -cp "$vr_out" p.VarargsReturnTest >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

echo
echo "--- A member interface's own type variable in a method of another file ---"
for ng_order in "NestedGenericBuilderTest Cust" "Cust NestedGenericBuilderTest"; do
    set -- $ng_order
    printf "%-30s ... " "NestedGenericBuilder ($1 first)"
    ng_out="$TEST_BUILD/nestedgenericbuilder-out"
    rm -rf "$ng_out"
    mkdir -p "$ng_out"
    if (cd "$ng_out" && "$GENESIS" -j1 -d "$ng_out" \
           "$TEST_SRC/external/nestedgenericbuilder/p/$1.java" \
           "$TEST_SRC/external/nestedgenericbuilder/p/$2.java" >/dev/null 2>&1) &&
       "$JAVA" -cp "$ng_out" p.NestedGenericBuilderTest >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

for sl_order in "StubLambdaParamTest Holder" "Holder StubLambdaParamTest"; do
    set -- $sl_order
    printf "%-30s ... " "StubLambdaParam ($1 first)"
    sl_out="$TEST_BUILD/stublambdaparam-out"
    rm -rf "$sl_out"
    mkdir -p "$sl_out"
    if (cd "$sl_out" && "$GENESIS" -j1 -d "$sl_out" \
           "$TEST_SRC/external/stublambdaparam/p/$1.java" \
           "$TEST_SRC/external/stublambdaparam/p/$2.java" >/dev/null 2>&1) &&
       "$JAVA" -cp "$sl_out" p.StubLambdaParamTest >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

for bo_order in "BoundedOverloadTest Asserts" "Asserts BoundedOverloadTest"; do
    set -- $bo_order
    printf "%-30s ... " "BoundedOverload ($1 first)"
    bo_out="$TEST_BUILD/boundedoverload-out"
    rm -rf "$bo_out"
    mkdir -p "$bo_out"
    if (cd "$bo_out" && "$GENESIS" -j1 -d "$bo_out" \
           "$TEST_SRC/external/boundedoverload/p/$1.java" \
           "$TEST_SRC/external/boundedoverload/p/$2.java" >/dev/null 2>&1) &&
       "$JAVA" -cp "$bo_out" p.BoundedOverloadTest >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

printf "%-30s ... " "PrivateClassfileOverload"
pc_lib="$TEST_BUILD/privateclassfileoverload-lib"
pc_out="$TEST_BUILD/privateclassfileoverload-out"
rm -rf "$pc_lib" "$pc_out"
mkdir -p "$pc_lib" "$pc_out"
if (cd "$pc_lib" && "$GENESIS" -j1 -d "$pc_lib" \
       "$TEST_SRC/external/privateclassfileoverload/p/Gate.java" >/dev/null 2>&1) &&
   (cd "$pc_out" && "$GENESIS" -j1 -cp "$pc_lib" -d "$pc_out" \
       "$TEST_SRC/external/privateclassfileoverload/p/PrivateClassfileOverloadTest.java" >/dev/null 2>&1) &&
   "$JAVA" -cp "$pc_out:$pc_lib" p.PrivateClassfileOverloadTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "InheritedMethodRef"
im_lib="$TEST_BUILD/inheritedmethodref-lib"
im_out="$TEST_BUILD/inheritedmethodref-out"
rm -rf "$im_lib" "$im_out"
mkdir -p "$im_lib" "$im_out"
if (cd "$im_lib" && "$GENESIS" -j1 -d "$im_lib" \
       "$TEST_SRC/external/inheritedmethodref/p/NamedHint.java" \
       "$TEST_SRC/external/inheritedmethodref/p/Hint.java" >/dev/null 2>&1) &&
   (cd "$im_out" && "$GENESIS" -j1 -cp "$im_lib" -d "$im_out" \
       "$TEST_SRC/external/inheritedmethodref/p/InheritedMethodRefTest.java" >/dev/null 2>&1) &&
   "$JAVA" -cp "$im_out:$im_lib" p.InheritedMethodRefTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

for sm_order in "SuperInterfaceMemberTypeTest AnyPhased BasePhased Phased" "Phased BasePhased AnyPhased SuperInterfaceMemberTypeTest"; do
    set -- $sm_order
    printf "%-30s ... " "SuperInterfaceMemberType ($1 first)"
    sm_out="$TEST_BUILD/superinterfacemembertype-out"
    rm -rf "$sm_out"
    mkdir -p "$sm_out"
    sm_files=""
    for sm_name in "$@"; do
        sm_files="$sm_files $TEST_SRC/external/superinterfacemembertype/p/$sm_name.java"
    done
    if (cd "$sm_out" && "$GENESIS" -j1 -d "$sm_out" $sm_files >/dev/null 2>&1) &&
       "$JAVA" -cp "$sm_out" p.SuperInterfaceMemberTypeTest >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

for fs_order in "FunctionalSuperInterfaceTest JmxEndpoints Endpoints JmxExposable Exposable" "Exposable JmxExposable Endpoints JmxEndpoints FunctionalSuperInterfaceTest" "JmxEndpoints Endpoints Exposable JmxExposable FunctionalSuperInterfaceTest"; do
    set -- $fs_order
    printf "%-30s ... " "FunctionalSuperInterface ($1 first)"
    fs_out="$TEST_BUILD/functionalsuperinterface-out"
    rm -rf "$fs_out"
    mkdir -p "$fs_out"
    fs_files=""
    for fs_name in "$@"; do
        fs_files="$fs_files $TEST_SRC/external/functionalsuperinterface/p/$fs_name.java"
    done
    if (cd "$fs_out" && "$GENESIS" -j1 -d "$fs_out" $fs_files >/dev/null 2>&1) &&
       "$JAVA" -cp "$fs_out" p.FunctionalSuperInterfaceTest >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

for ig_order in "InheritedGenericMemberTest ExposableJmx Exposable JmxOperation Operation" "Operation JmxOperation Exposable ExposableJmx InheritedGenericMemberTest" "ExposableJmx InheritedGenericMemberTest Exposable Operation JmxOperation"; do
    set -- $ig_order
    printf "%-30s ... " "InheritedGenericMember ($1 first)"
    ig_out="$TEST_BUILD/inheritedgenericmember-out"
    rm -rf "$ig_out"
    mkdir -p "$ig_out"
    ig_files=""
    for ig_name in "$@"; do
        ig_files="$ig_files $TEST_SRC/external/inheritedgenericmember/p/$ig_name.java"
    done
    if (cd "$ig_out" && "$GENESIS" -j1 -d "$ig_out" $ig_files >/dev/null 2>&1) &&
       "$JAVA" -cp "$ig_out" p.InheritedGenericMemberTest >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

for iq_order in "InheritedMemberTypeQualifiedTest Sub Base" "Base Sub InheritedMemberTypeQualifiedTest" "Sub InheritedMemberTypeQualifiedTest Base"; do
    set -- $iq_order
    printf "%-30s ... " "InheritedMemberTypeQualified ($1 first)"
    iq_out="$TEST_BUILD/inheritedmembertypequalified-out"
    rm -rf "$iq_out"
    mkdir -p "$iq_out"
    iq_files=""
    for iq_name in "$@"; do
        iq_files="$iq_files $TEST_SRC/external/inheritedmembertypequalified/p/$iq_name.java"
    done
    if (cd "$iq_out" && "$GENESIS" -j1 -d "$iq_out" $iq_files >/dev/null 2>&1) &&
       "$JAVA" -cp "$iq_out" p.InheritedMemberTypeQualifiedTest >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

for tg_order in "TwoLevelGenericAdapterTest Adapter StepAdapter ConfigAdapter Props StepProperties Config" "Config StepProperties Props ConfigAdapter StepAdapter Adapter TwoLevelGenericAdapterTest" "Adapter StepAdapter ConfigAdapter Props StepProperties Config TwoLevelGenericAdapterTest"; do
    set -- $tg_order
    printf "%-30s ... " "TwoLevelGenericAdapter ($1 first)"
    tg_out="$TEST_BUILD/twolevelgenericadapter-out"
    rm -rf "$tg_out"
    mkdir -p "$tg_out"
    tg_files=""
    for tg_name in "$@"; do
        tg_files="$tg_files $TEST_SRC/external/twolevelgenericadapter/p/$tg_name.java"
    done
    if (cd "$tg_out" && "$GENESIS" -j1 -d "$tg_out" $tg_files >/dev/null 2>&1) &&
       "$JAVA" -cp "$tg_out" p.TwoLevelGenericAdapterTest >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

for fm_order in "FlatMapUnboundRefTest Composite Items" "Items Composite FlatMapUnboundRefTest" "Composite Items FlatMapUnboundRefTest"; do
    set -- $fm_order
    printf "%-30s ... " "FlatMapUnboundRef ($1 first)"
    fm_out="$TEST_BUILD/flatmapunboundref-out"
    rm -rf "$fm_out"
    mkdir -p "$fm_out"
    fm_files=""
    for fm_name in "$@"; do
        fm_files="$fm_files $TEST_SRC/external/flatmapunboundref/p/$fm_name.java"
    done
    if (cd "$fm_out" && "$GENESIS" -j1 -d "$fm_out" $fm_files >/dev/null 2>&1) &&
       "$JAVA" -cp "$fm_out" p.FlatMapUnboundRefTest >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

printf "%-30s ... " "QualifiedThisFieldInitializer"
qt_out="$TEST_BUILD/qualifiedthisfield-out"
rm -rf "$qt_out"
mkdir -p "$qt_out"
if (cd "$qt_out" && "$GENESIS" -j1 -d "$qt_out" \
       "$TEST_SRC/external/qualifiedthisfield/p/QualifiedThisFieldInitializerTest.java" >/dev/null 2>&1) &&
   "$JAVA" -cp "$qt_out" p.QualifiedThisFieldInitializerTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "NestedSiblingInterface"
ns_lib="$TEST_BUILD/nestedsiblinglib-lib"
ns_out="$TEST_BUILD/nestedsiblinglib-out"
rm -rf "$ns_lib" "$ns_out"
mkdir -p "$ns_lib" "$ns_out"
if "$GENESIS" -d "$ns_lib" "$TEST_SRC"/external/nestedsiblinglib/nestedsiblinglib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$ns_lib" -d "$ns_out" \
       "$TEST_SRC/external/NestedSiblingInterfaceTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$ns_out:$ns_lib" NestedSiblingInterfaceTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

for ap_order in "State ArrayTypeVariableParameterTest" "ArrayTypeVariableParameterTest State"; do
    set -- $ap_order
    printf "%-30s ... " "ArrayTypeVariableParameter ($1 first)"
    ap_out="$TEST_BUILD/arrayparamstub-out"
    rm -rf "$ap_out"
    mkdir -p "$ap_out"
    ap_files=""
    for ap_name in "$@"; do
        ap_files="$ap_files $TEST_SRC/external/arrayparamstub/p/$ap_name.java"
    done
    if (cd "$ap_out" && "$GENESIS" -j1 -d "$ap_out" $ap_files >/dev/null 2>&1) &&
       "$JAVA" -Xverify:all -cp "$ap_out" p.ArrayTypeVariableParameterTest >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

printf "%-30s ... " "RepeatedAnnotationFromClassFile"
ra_lib="$TEST_BUILD/repeatablelib-lib"
ra_out="$TEST_BUILD/repeatablelib-out"
rm -rf "$ra_lib" "$ra_out"
mkdir -p "$ra_lib" "$ra_out"
if "$GENESIS" -d "$ra_lib" "$TEST_SRC"/external/repeatablelib/repeatablelib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$ra_lib" -d "$ra_out" \
       "$TEST_SRC/external/RepeatedAnnotationFromClassFileTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$ra_out:$ra_lib" RepeatedAnnotationFromClassFileTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "NestedAnnotationNameFromClassFile"
na_lib="$TEST_BUILD/nestedannolib-lib"
na_out="$TEST_BUILD/nestedannolib-out"
rm -rf "$na_lib" "$na_out"
mkdir -p "$na_lib" "$na_out"
if "$GENESIS" -d "$na_lib" "$TEST_SRC"/external/nestedannolib/nestedannolib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$na_lib" -d "$na_out" \
       "$TEST_SRC/external/NestedAnnotationNameFromClassFileTest.java" >/dev/null 2>&1 &&
   "$JAVA" -cp "$na_out:$na_lib" NestedAnnotationNameFromClassFileTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "BoundedEnumOverload"
be_lib="$TEST_BUILD/boundedenumlib-lib"
be_out="$TEST_BUILD/boundedenumlib-out"
rm -rf "$be_lib" "$be_out"
mkdir -p "$be_lib" "$be_out"
if "$GENESIS" -d "$be_lib" "$TEST_SRC"/external/boundedenumlib/boundedenumlib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$be_lib" -d "$be_out" \
       "$TEST_SRC/external/BoundedEnumOverloadTest.java" >/dev/null 2>&1 &&
   "$JAVA" -Xverify:all -cp "$be_out:$be_lib" BoundedEnumOverloadTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "OverloadScore"
os_lib="$TEST_BUILD/ovlpick-lib"
os_out="$TEST_BUILD/ovlpick-out"
rm -rf "$os_lib" "$os_out"
mkdir -p "$os_lib" "$os_out"
if "$GENESIS" -d "$os_lib" "$TEST_SRC"/external/ovlpick/ovlpick/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$os_lib" -d "$os_out" \
       "$TEST_SRC/external/OverloadScoreTest.java" >/dev/null 2>&1 &&
   "$JAVA" -Xverify:all -cp "$os_out:$os_lib" OverloadScoreTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "QualifiedThisGenericField"
qg_lib="$TEST_BUILD/qtgflib-lib"
qg_out="$TEST_BUILD/qtgflib-out"
rm -rf "$qg_lib" "$qg_out"
mkdir -p "$qg_lib" "$qg_out"
if "$GENESIS" -d "$qg_lib" "$TEST_SRC"/external/qualifiedthisgenericfield/qtgflib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$qg_lib" -d "$qg_out" \
       "$TEST_SRC/external/QualifiedThisGenericFieldTest.java" >/dev/null 2>&1 &&
   "$JAVA" -Xverify:all -cp "$qg_out:$qg_lib" QualifiedThisGenericFieldTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "MethodParameters (-parameters)"
mp_out="$TEST_BUILD/methodparameters-out"
rm -rf "$mp_out"
mkdir -p "$mp_out"
if "$GENESIS" -parameters -d "$mp_out" "$TEST_SRC/external/MethodParametersTest.java" >/dev/null 2>&1 &&
   "$JAVA" -Xverify:all -cp "$mp_out" MethodParametersTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

for ci_order in "q/Limits p/Props r/Marker r/ImportedConstantInAnnotationTest" "r/ImportedConstantInAnnotationTest r/Marker p/Props q/Limits" "p/Props r/ImportedConstantInAnnotationTest q/Limits r/Marker"; do
    set -- $ci_order
    printf "%-30s ... " "ImportedConstantAnnotation ($1 first)"
    ci_out="$TEST_BUILD/crossfileconstimport-out"
    rm -rf "$ci_out"
    mkdir -p "$ci_out"
    ci_files=""
    for ci_name in "$@"; do
        ci_files="$ci_files $TEST_SRC/external/crossfileconstimport/$ci_name.java"
    done
    if (cd "$ci_out" && "$GENESIS" -j1 -d "$ci_out" $ci_files >/dev/null 2>&1) &&
       "$JAVA" -cp "$ci_out" r.ImportedConstantInAnnotationTest >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

for it_order in "Mappings IterableOfNestedTypeTest" "IterableOfNestedTypeTest Mappings"; do
    set -- $it_order
    printf "%-30s ... " "IterableOfNestedType ($1 first)"
    it_out="$TEST_BUILD/iterablestubs-out"
    rm -rf "$it_out"
    mkdir -p "$it_out"
    it_files=""
    for it_name in "$@"; do
        it_files="$it_files $TEST_SRC/external/iterablestubs/p/$it_name.java"
    done
    if (cd "$it_out" && "$GENESIS" -j1 -d "$it_out" $it_files >/dev/null 2>&1) &&
       "$JAVA" -Xverify:all -cp "$it_out" p.IterableOfNestedTypeTest >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

for es_order in "Settings EnumStubOverloadTest" "EnumStubOverloadTest Settings"; do
    set -- $es_order
    printf "%-30s ... " "EnumStubOverload ($1 first)"
    es_out="$TEST_BUILD/enumstubs-out"
    rm -rf "$es_out"
    mkdir -p "$es_out"
    es_files=""
    for es_name in "$@"; do
        es_files="$es_files $TEST_SRC/external/enumstubs/p/$es_name.java"
    done
    if (cd "$es_out" && "$GENESIS" -j1 -d "$es_out" $es_files >/dev/null 2>&1) &&
       "$JAVA" -Xverify:all -cp "$es_out" p.EnumStubOverloadTest >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

for sc_order in "Registry SuperConstructorMethodRefTest" "SuperConstructorMethodRefTest Registry"; do
    set -- $sc_order
    printf "%-30s ... " "SuperConstructorMethodRef ($1 first)"
    sc_out="$TEST_BUILD/superctorref-out"
    rm -rf "$sc_out"
    mkdir -p "$sc_out"
    sc_files=""
    for sc_name in "$@"; do
        sc_files="$sc_files $TEST_SRC/external/superctorref/p/$sc_name.java"
    done
    if (cd "$sc_out" && "$GENESIS" -j1 -d "$sc_out" $sc_files >/dev/null 2>&1) &&
       "$JAVA" -Xverify:all -cp "$sc_out" p.SuperConstructorMethodRefTest >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

for ae_order in "Cond SingleValueForArrayElementTest" "SingleValueForArrayElementTest Cond"; do
    set -- $ae_order
    printf "%-30s ... " "SingleValueForArrayElement ($1 first)"
    ae_out="$TEST_BUILD/arrayelementstubs-out"
    rm -rf "$ae_out"
    mkdir -p "$ae_out"
    ae_files=""
    for ae_name in "$@"; do
        ae_files="$ae_files $TEST_SRC/external/arrayelementstubs/p/$ae_name.java"
    done
    if (cd "$ae_out" && "$GENESIS" -j1 -d "$ae_out" $ae_files >/dev/null 2>&1) &&
       "$JAVA" -Xverify:all -cp "$ae_out" p.SingleValueForArrayElementTest >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

for mr_order in "Types MemberRecordUseTest" "MemberRecordUseTest Types"; do
    set -- $mr_order
    printf "%-30s ... " "MemberRecordUse ($1 first)"
    mr_out="$TEST_BUILD/memberrecordstubs-out"
    rm -rf "$mr_out"
    mkdir -p "$mr_out"
    mr_files=""
    for mr_name in "$@"; do
        mr_files="$mr_files $TEST_SRC/external/memberrecordstubs/p/$mr_name.java"
    done
    if (cd "$mr_out" && "$GENESIS" -j1 -d "$mr_out" $mr_files >/dev/null 2>&1) &&
       "$JAVA" -Xverify:all -cp "$mr_out" p.MemberRecordUseTest >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

for db_order in "Main Iter Source Handle IterHandle" "Source Handle IterHandle Iter Main" "Iter Main Source IterHandle Handle"; do
    set -- $db_order
    printf "%-30s ... " "DefaultBridgeStubs ($1 first)"
    db_out="$TEST_BUILD/defaultbridgestubs-out"
    rm -rf "$db_out"
    mkdir -p "$db_out"
    db_files=""
    for db_name in "$@"; do
        db_files="$db_files $TEST_SRC/external/defaultbridgestubs/p/$db_name.java"
    done
    if (cd "$db_out" && "$GENESIS" -d "$db_out" $db_files >/dev/null 2>&1) &&
       "$JAVA" -Xverify:all -cp "$db_out" p.Main >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

printf "%-30s ... " "ProtectedMethodFromNestedClass"
pm_lib="$TEST_BUILD/protectedmethodlib-lib"
pm_out="$TEST_BUILD/protectedmethodlib-out"
rm -rf "$pm_lib" "$pm_out"
mkdir -p "$pm_lib" "$pm_out"
if "$GENESIS" -d "$pm_lib" "$TEST_SRC"/external/protectedmethodlib/protlib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$pm_lib" -d "$pm_out" \
       "$TEST_SRC/external/ProtectedMethodFromNestedClassTest.java" >/dev/null 2>&1 &&
   "$JAVA" -Xverify:all -cp "$pm_out:$pm_lib" ProtectedMethodFromNestedClassTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "NestedLambdaSamReturn"
nls_out="$TEST_BUILD/nested-lambda-sam-out"
rm -rf "$nls_out"
mkdir -p "$nls_out"
if (cd "$nls_out" && "$GENESIS" -d "$nls_out" \
       "$TEST_SRC/external/executorproviderlib/ep/ExecutorProvider.java" \
       "$TEST_SRC/external/nestedlambdaouter/NestedLambdaOuter.java" >/dev/null 2>&1) &&
   "$JAVA" -Xverify:all -cp "$nls_out" NestedLambdaOuter >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "InheritedFromHiddenSuperclass"
hs_lib="$TEST_BUILD/hiddensuperlib-lib"
hs_out="$TEST_BUILD/hiddensuperlib-out"
rm -rf "$hs_lib" "$hs_out"
mkdir -p "$hs_lib" "$hs_out"
if "$GENESIS" -d "$hs_lib" "$TEST_SRC"/external/hiddensuperlib/hiddenlib/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$hs_lib" -d "$hs_out" \
       "$TEST_SRC/external/InheritedFromHiddenSuperclassTest.java" >/dev/null 2>&1 &&
   "$JAVA" -Xverify:all -cp "$hs_out:$hs_lib" InheritedFromHiddenSuperclassTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "ClassFileInterfaceChainOverload"
ic_lib="$TEST_BUILD/iterablesrclib-lib"
ic_out="$TEST_BUILD/iterablesrclib-out"
rm -rf "$ic_lib" "$ic_out"
mkdir -p "$ic_lib" "$ic_out"
if "$GENESIS" -d "$ic_lib" "$TEST_SRC"/external/iterablesrclib/itersrc/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$ic_lib" -d "$ic_out" \
       "$TEST_SRC/external/ClassFileInterfaceChainOverloadTest.java" >/dev/null 2>&1 &&
   "$JAVA" -Xverify:all -cp "$ic_out:$ic_lib" ClassFileInterfaceChainOverloadTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "WildcardResultOverload"
wr_lib="$TEST_BUILD/wildcardresultlib-lib"
wr_out="$TEST_BUILD/wildcardresultlib-out"
rm -rf "$wr_lib" "$wr_out"
mkdir -p "$wr_lib" "$wr_out"
if "$GENESIS" -d "$wr_lib" "$TEST_SRC"/external/wildcardresultlib/wcres/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$wr_lib" -d "$wr_out" \
       "$TEST_SRC/external/WildcardResultOverloadTest.java" >/dev/null 2>&1 &&
   "$JAVA" -Xverify:all -cp "$wr_out:$wr_lib" WildcardResultOverloadTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "ClassFileBoundedResultChain"
br_lib="$TEST_BUILD/boundedresultlib-lib"
br_out="$TEST_BUILD/boundedresultlib-out"
rm -rf "$br_lib" "$br_out"
mkdir -p "$br_lib" "$br_out"
if "$GENESIS" -d "$br_lib" "$TEST_SRC"/external/boundedresultlib/bres/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$br_lib" -d "$br_out" \
       "$TEST_SRC/external/ClassFileBoundedResultChainTest.java" >/dev/null 2>&1 &&
   "$JAVA" -Xverify:all -cp "$br_out:$br_lib" ClassFileBoundedResultChainTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "InheritedGenericChain"
gc_lib="$TEST_BUILD/genericchainlib-lib"
gc_out="$TEST_BUILD/genericchainlib-out"
rm -rf "$gc_lib" "$gc_out"
mkdir -p "$gc_lib" "$gc_out"
if "$GENESIS" -d "$gc_lib" "$TEST_SRC"/external/genericchainlib/gchain/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$gc_lib" -d "$gc_out" \
       "$TEST_SRC/external/InheritedGenericChainTest.java" >/dev/null 2>&1 &&
   "$JAVA" -Xverify:all -cp "$gc_out:$gc_lib" InheritedGenericChainTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "TypeVariableMethodReference"
eu_lib="$TEST_BUILD/emptyutilslib-lib"
eu_out="$TEST_BUILD/emptyutilslib-out"
rm -rf "$eu_lib" "$eu_out"
mkdir -p "$eu_lib" "$eu_out"
if "$GENESIS" -d "$eu_lib" "$TEST_SRC"/external/emptyutilslib/emptyutils/*.java >/dev/null 2>&1 &&
   "$GENESIS" -cp "$eu_lib" -d "$eu_out" \
       "$TEST_SRC/external/TypeVariableMethodReferenceOverloadTest.java" >/dev/null 2>&1 &&
   "$JAVA" -Xverify:all -cp "$eu_out:$eu_lib" TypeVariableMethodReferenceOverloadTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

printf "%-30s ... " "InheritedMemberShadow"
im_out="$TEST_BUILD/inheritedmembershadow-out"
rm -rf "$im_out"
mkdir -p "$im_out"
if (cd "$im_out" && "$GENESIS" -j1 -d "$im_out" -sourcepath "$TEST_SRC/external/inheritedmembershadow" \
       "$TEST_SRC/external/inheritedmembershadow/p/InheritedMemberShadowTest.java" >/dev/null 2>&1) &&
   "$JAVA" -Xverify:all -cp "$im_out" p.InheritedMemberShadowTest >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

for is_order in "InheritedSamVarargsTest SrvFactory SrvCustomizer Srv" "Srv SrvCustomizer SrvFactory InheritedSamVarargsTest" "SrvFactory InheritedSamVarargsTest Srv SrvCustomizer"; do
    set -- $is_order
    printf "%-30s ... " "InheritedSamVarargs ($1 first)"
    is_out="$TEST_BUILD/inheritedsamvarargs-out"
    rm -rf "$is_out"
    mkdir -p "$is_out"
    is_files=""
    for is_name in "$@"; do
        is_files="$is_files $TEST_SRC/external/inheritedsamvarargs/p/$is_name.java"
    done
    if (cd "$is_out" && "$GENESIS" -j1 -d "$is_out" $is_files >/dev/null 2>&1) &&
       "$JAVA" -cp "$is_out" p.InheritedSamVarargsTest >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

for ic_order in "IterableClassForEachTest Builder Convs Conv" "Conv Convs Builder IterableClassForEachTest" "Builder IterableClassForEachTest Conv Convs"; do
    set -- $ic_order
    printf "%-30s ... " "IterableClassForEach ($1 first)"
    ic_out="$TEST_BUILD/iterableclassforeach-out"
    rm -rf "$ic_out"
    mkdir -p "$ic_out"
    ic_files=""
    for ic_name in "$@"; do
        ic_files="$ic_files $TEST_SRC/external/iterableclassforeach/p/$ic_name.java"
    done
    if (cd "$ic_out" && "$GENESIS" -j1 -d "$ic_out" $ic_files >/dev/null 2>&1) &&
       "$JAVA" -cp "$ic_out" p.IterableClassForEachTest >/dev/null 2>&1; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

echo
echo "--- Narrowing a non-constant or an out-of-range constant must fail to compile ---"
printf "%-30s ... " "NarrowingRejected"
nr_out="$TEST_BUILD/narrowingrejected-out"
rm -rf "$nr_out"
mkdir -p "$nr_out"
if ! "$GENESIS" -d "$nr_out" "$TEST_SRC/external/narrowingrejected/NarrowingRejected.java" >/dev/null 2>&1; then
    echo PASS
    passed=$((passed + 1))
else
    echo FAIL
    failed=$((failed + 1))
fi

echo "--- A lambda or method reference cast to a type that is no functional interface ---"
for f in CastToPrimitive CastToArray CastToNonFunctional MethodRefCastToPrimitive; do
    printf "%-30s ... " "Rejecting $f"
    lc_out="$TEST_BUILD/lambdacast-out"
    rm -rf "$lc_out"
    mkdir -p "$lc_out"
    # A diagnostic from semantic analysis, not code generation failing on a lambda nothing typed
    lc_msg=$("$GENESIS" -d "$lc_out" "$TEST_SRC/external/lambdacast/$f.java" 2>&1)
    lc_rc=$?
    if [ $lc_rc -eq 1 ] && echo "$lc_msg" | grep -q "is not a functional interface" &&
       ! echo "$lc_msg" | grep -q "code generation failed"; then
        echo PASS
        passed=$((passed + 1))
    else
        echo FAIL
        failed=$((failed + 1))
    fi
done

echo
echo "=== Results: $passed passed, $failed failed ==="
[ $failed -eq 0 ]
