#!/bin/sh
#
# compare-compilers.sh - benchmark genesis vs javac on a git checkout
#
# Copyright (C) 2026 Chris Burdess <dog@gnu.org>
#
# This file is part of genesis; see COPYING for the license terms.
#
# Clones a repository into a fresh temporary directory (never uses an existing
# local checkout), prepares it, builds a javac-style @argument file listing
# every compilation unit, then times monolithic compiles.
#
# Environment:
#   GENESIS          path to genesis (default: ../src/genesis from repo root)
#   JAVA_HOME        JDK for javac (required)
#   BENCH_REPO_URL   git remote (default: gumdrop)
#   BENCH_GIT_REF    optional branch, tag, or commit after clone
#   BENCH_PREP       shell command run in repo root before compile (default: ant resolve-deps)
#   BENCH_SRC_DIR    directory to scan for .java files (default: src)
#   BENCH_RELEASE    -release version (default: parse java.release.version from build.xml, else 25)
#   BENCH_ITERATIONS timed iterations per compiler mode (default: 10)
#   BENCH_WARMUP     untimed warmup runs per mode (default: 1)
#   BENCH_KEEP_WORKDIR  if set, do not delete the temp directory on exit
#
# Usage: compare-compilers.sh

set -e

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/.." && pwd)

GENESIS=${GENESIS:-$top/src/genesis}
JAVAC=${JAVAC:-${JAVA_HOME:+$JAVA_HOME/bin/javac}}
JAVAC=${JAVAC:-javac}

BENCH_REPO_URL=${BENCH_REPO_URL:-https://github.com/cpkb-bluezoo/gumdrop.git}
BENCH_PREP=${BENCH_PREP:-ant resolve-deps}
BENCH_SRC_DIR=${BENCH_SRC_DIR:-src}
BENCH_ITERATIONS=${BENCH_ITERATIONS:-10}
BENCH_WARMUP=${BENCH_WARMUP:-1}

TIME=${TIME:-/usr/bin/time}
if [ ! -x "$TIME" ]; then
    TIME=time
fi

usage() {
    echo "Usage: $(basename "$0")" >&2
    echo "See script header for BENCH_* and GENESIS / JAVA_HOME variables." >&2
    exit 1
}

[ "$1" = "-h" ] || [ "$1" = "--help" ] && usage

if ! command -v "$GENESIS" >/dev/null 2>&1; then
    echo "error: genesis not found at $GENESIS (build genesis or set GENESIS)" >&2
    exit 1
fi
if ! command -v "$JAVAC" >/dev/null 2>&1; then
    echo "error: javac not found (set JAVA_HOME or JAVAC)" >&2
    exit 1
fi
if ! command -v git >/dev/null 2>&1; then
    echo "error: git not found" >&2
    exit 1
fi

case $BENCH_ITERATIONS in
    ''|*[!0-9]*)
        echo "error: BENCH_ITERATIONS must be a positive integer" >&2
        exit 1
        ;;
esac
case $BENCH_WARMUP in
    ''|*[!0-9]*)
        echo "error: BENCH_WARMUP must be a non-negative integer" >&2
        exit 1
        ;;
esac

WORKDIR=$(mktemp -d "${TMPDIR:-/tmp}/genesis-bench.XXXXXX")
cleanup() {
    if [ -n "$BENCH_KEEP_WORKDIR" ]; then
        echo "Keeping work directory: $WORKDIR"
    else
        rm -rf "$WORKDIR"
    fi
}
trap cleanup EXIT

REPO=$WORKDIR/repo
OUT=$WORKDIR/out
META=$WORKDIR/meta
mkdir -p "$OUT" "$META"

echo "=== Genesis compiler benchmark ==="
echo "Work directory: $WORKDIR"
echo "Repository:     $BENCH_REPO_URL"
echo "Genesis:        $GENESIS"
echo "javac:          $JAVAC ($("$JAVAC" -version 2>&1))"
echo "Iterations:     $BENCH_ITERATIONS (warmup $BENCH_WARMUP per mode)"
echo

git clone --depth 1 "$BENCH_REPO_URL" "$REPO"
if [ -n "$BENCH_GIT_REF" ]; then
    git -C "$REPO" fetch --depth 1 origin "$BENCH_GIT_REF"
    git -C "$REPO" checkout FETCH_HEAD
fi
BENCH_COMMIT=$(git -C "$REPO" rev-parse HEAD)
echo "Checked out commit: $BENCH_COMMIT"
echo

if [ -n "$BENCH_PREP" ]; then
    echo "Preparing repository: $BENCH_PREP"
    (cd "$REPO" && eval "$BENCH_PREP")
    echo
fi

if [ -n "$BENCH_RELEASE" ]; then
    RELEASE=$BENCH_RELEASE
elif [ -f "$REPO/build.xml" ]; then
    RELEASE=$(sed -n "s/.*name='java.release.version' value='\([^']*\)'.*/\1/p" \
        "$REPO/build.xml" | head -1)
fi
RELEASE=${RELEASE:-25}

# Classpath: jars from lib/ after resolve-deps (gumdrop and similar layouts).
CLASSPATH=
if [ -d "$REPO/lib" ]; then
    for jar in "$REPO/lib"/*.jar; do
        [ -f "$jar" ] || continue
        CLASSPATH=${CLASSPATH:+$CLASSPATH:}$jar
    done
fi

SOURCES=$META/sources.txt
if [ ! -d "$REPO/$BENCH_SRC_DIR" ]; then
    echo "error: source directory $REPO/$BENCH_SRC_DIR not found" >&2
    exit 1
fi
find "$REPO/$BENCH_SRC_DIR" -name '*.java' | LC_ALL=C sort >"$SOURCES"
SOURCE_COUNT=$(wc -l <"$SOURCES" | tr -d ' ')
if [ "$SOURCE_COUNT" -eq 0 ]; then
    echo "error: no .java files under $BENCH_SRC_DIR" >&2
    exit 1
fi
echo "Compilation units: $SOURCE_COUNT (under $BENCH_SRC_DIR/)"
echo "Release:           $RELEASE"
if [ -n "$CLASSPATH" ]; then
    echo "Classpath:         $REPO/lib/*.jar ($(echo "$CLASSPATH" | tr ':' '\n' | wc -l | tr -d ' ') jars)"
else
    echo "Classpath:         (empty)"
fi
echo

write_argfile() {
    destdir=$1
    file=$2
    {
        echo "-d"
        echo "$destdir"
        if [ -n "$CLASSPATH" ]; then
            echo "-cp"
            echo "$CLASSPATH"
        fi
        echo "--release"
        echo "$RELEASE"
        echo "-g"
        while IFS= read -r path; do
            echo "$path"
        done <"$SOURCES"
    } >"$file"
}

class_manifest() {
    root=$1
    find "$root" -name '*.class' ! -path '*/.*' \
        | sed "s|^$root/||" | LC_ALL=C sort
}

write_manifest() {
    root=$1
    out=$2
    class_manifest "$root" >"$out"
    wc -l <"$out" | tr -d ' '
}

# javac emits synthetic Outer$N holders with static $SwitchMap$... int[] fields for enum
# switches. Genesis uses ordinal()+lookupswitch (faster compile, same behavior).
is_enum_switch_synthetic_class() {
    javac_root=$1
    relpath=$2
    qname=$(echo "$relpath" | sed 's|\.class$||' | tr '/' '.')
    javap_bin=${JAVAP:-${JAVA_HOME:+$JAVA_HOME/bin/javap}}
    javap_bin=${javap_bin:-javap}
    out=$("$javap_bin" -classpath "$javac_root" -p "$qname" 2>/dev/null) || return 1
    case "$out" in
        *'$SwitchMap'*) ;;
        *) return 1 ;;
    esac
    case "$out" in
        *' implements '*|*' extends '*) return 1 ;;
    esac
    return 0
}

compare_manifests_functional() {
    ref=$1
    other=$2
    javac_root=$3
    label=$4
    only_gen=$(LC_ALL=C comm -13 "$ref" "$other")
    if [ -n "$only_gen" ]; then
        echo "error: $label emitted class files not produced by javac:" >&2
        echo "$only_gen" | head -20 >&2
        return 1
    fi
    only_ref=$(LC_ALL=C comm -23 "$ref" "$other")
    if [ -z "$only_ref" ]; then
        return 0
    fi
    bad=
    while IFS= read -r path; do
        [ -z "$path" ] && continue
        if ! is_enum_switch_synthetic_class "$javac_root" "$path"; then
            bad=$path
            break
        fi
    done <<EOF
$only_ref
EOF
    if [ -n "$bad" ]; then
        echo "error: class file list mismatch ($label vs reference)" >&2
        echo "Genesis is missing a class javac emitted that is not an enum-switch synthetic:" >&2
        echo "  $bad" >&2
        diff -u "$ref" "$other" | head -50 >&2
        return 1
    fi
    return 0
}

run_compile() {
    mode=$1
    extra_args=$2
    destdir=$3
    argfile=$META/compile.args

    rm -rf "$destdir"
    mkdir -p "$destdir"
    write_argfile "$destdir" "$argfile"

    case $mode in
        genesis)
            set -- "$GENESIS" $extra_args "@$argfile"
            ;;
        javac)
            set -- "$JAVAC" $extra_args "@$argfile"
            ;;
        *)
            echo "internal error: unknown mode $mode" >&2
            return 1
            ;;
    esac

    LOG=$META/compile.log
    TIMEFILE=$META/time.$$
    if "$TIME" -p sh -c '
        log=$1
        shift
        "$@" >"$log" 2>&1
    ' _ "$LOG" "$@" 2>"$TIMEFILE"; then
        status=0
    else
        status=$?
    fi
    if [ "$status" -ne 0 ]; then
        echo "error: compilation failed ($mode $extra_args)" >&2
        if [ -s "$LOG" ]; then
            tail -30 "$LOG" >&2
        fi
        sed '/^real /d;/^user /d;/^sys /d' "$TIMEFILE" >&2
        rm -f "$TIMEFILE"
        return "$status"
    fi
    eval "$(sed -n 's/^\(real\|user\|sys\) \(.*\)$/\1=\2;/p' "$TIMEFILE")"
    rm -f "$TIMEFILE"
    return 0
}

REF_MANIFEST=$META/classes.javac.ref
verify_outputs() {
    echo "Verifying class output (functional parity with javac)..."
    javac_out=$OUT/verify-javac
    gen_out=$OUT/verify-genesis
    gen1_out=$OUT/verify-genesis-j1

    run_compile javac "" "$javac_out" || return 1
    JAVAC_CLASS_COUNT=$(write_manifest "$javac_out" "$REF_MANIFEST")
    cp "$REF_MANIFEST" "$META/classes.reference"

    run_compile genesis "" "$gen_out" || return 1
    write_manifest "$gen_out" "$META/classes.genesis"
    compare_manifests_functional "$REF_MANIFEST" "$META/classes.genesis" \
        "$javac_out" "genesis (parallel)" || return 1

    run_compile genesis "-j1" "$gen1_out" || return 1
    write_manifest "$gen1_out" "$META/classes.genesis-j1"
    compare_manifests_functional "$REF_MANIFEST" "$META/classes.genesis-j1" \
        "$javac_out" "genesis -j1" || return 1

    echo "Reference: $JAVAC_CLASS_COUNT class files (javac)"
    echo
}

verify_outputs || exit 1

if [ "$BENCH_ITERATIONS" -eq 0 ]; then
    echo "BENCH_ITERATIONS=0: verification only, skipping timed runs."
    echo
    echo "Commit: $BENCH_COMMIT"
    echo "Sources: $SOURCE_COUNT .java files"
    echo "Classes: $JAVAC_CLASS_COUNT (javac reference)"
    exit 0
fi

run_timed_mode() {
    mode_id=$1
    compiler=$2
    extra=$3
    name=$4

    real_file=$META/stats.$mode_id.real
    user_file=$META/stats.$mode_id.user
    sys_file=$META/stats.$mode_id.sys
    : >"$real_file"
    : >"$user_file"
    : >"$sys_file"
    ok=0

    outdir=$OUT/timed-$mode_id
    w=0
    while [ "$w" -lt "$BENCH_WARMUP" ]; do
        run_compile "$compiler" "$extra" "$outdir" || return 1
        write_manifest "$outdir" "$META/check.$mode_id.w$w"
        compare_manifests_functional "$REF_MANIFEST" "$META/check.$mode_id.w$w" \
            "$OUT/verify-javac" "$name warmup" || return 1
        w=$((w + 1))
    done

    i=0
    while [ "$i" -lt "$BENCH_ITERATIONS" ]; do
        if run_compile "$compiler" "$extra" "$outdir"; then
            write_manifest "$outdir" "$META/check.$mode_id.$i"
            compare_manifests_functional "$REF_MANIFEST" "$META/check.$mode_id.$i" \
                "$OUT/verify-javac" "$name" || return 1
            echo "$real" >>"$real_file"
            echo "$user" >>"$user_file"
            echo "$sys" >>"$sys_file"
            ok=$((ok + 1))
        fi
        i=$((i + 1))
    done
    echo "$ok" >"$META/stats.$mode_id.n"
    echo "$name: $ok successful timed run(s)"
    if [ "$ok" -eq 0 ]; then
        echo "error: no successful timed runs for $name" >&2
        return 1
    fi
    return 0
}

echo "Timed compilation ($BENCH_ITERATIONS iteration(s) per mode)..."
run_timed_mode parallel genesis "" "Genesis (parallel)"
run_timed_mode single genesis "-j1" "Genesis (-j1)"
run_timed_mode javac javac "" "javac"
echo

echo "=== Results (seconds, successful runs only) ==="
printf "%-22s %6s %10s %10s %10s %5s\n" "Compiler" "Runs" "real avg" "user avg" "sys avg" "class"
printf "%-22s %6s %10s %10s %10s %5s\n" "--------" "----" "--------" "--------" "--------" "-----"

JAVAC_REAL_AVG=$(awk '{s+=$1} END {printf "%.4f", s/NR}' "$META/stats.javac.real")

print_result_row() {
    mode_id=$1
    name=$2
    n=$(cat "$META/stats.$mode_id.n")
    avg_real=$(awk '{s+=$1} END {printf "%.2f", s/NR}' "$META/stats.$mode_id.real")
    avg_user=$(awk '{s+=$1} END {printf "%.2f", s/NR}' "$META/stats.$mode_id.user")
    avg_sys=$(awk '{s+=$1} END {printf "%.2f", s/NR}' "$META/stats.$mode_id.sys")
    printf "%-22s %6s %10s %10s %10s %5s\n" "$name" "$n" "$avg_real" "$avg_user" "$avg_sys" "$JAVAC_CLASS_COUNT"
}

print_result_row parallel "Genesis (parallel)"
print_result_row single "Genesis (-j1)"
print_result_row javac "javac"

if awk -v j="$JAVAC_REAL_AVG" 'BEGIN { exit (j > 0) ? 0 : 1 }'; then
    echo
    echo "Speedup vs javac (wall clock, real time):"
    for mode_id in parallel single; do
        avg=$(awk '{s+=$1} END {print s/NR}' "$META/stats.$mode_id.real")
        speedup=$(awk -v j="$JAVAC_REAL_AVG" -v g="$avg" 'BEGIN { printf "%.2f", j / g }')
        case $mode_id in
            parallel) label="Genesis (parallel)" ;;
            single) label="Genesis (-j1)" ;;
        esac
        echo "  $label: ${speedup}x"
    done
fi

echo
echo "Commit: $BENCH_COMMIT"
echo "Sources: $SOURCE_COUNT .java files"
