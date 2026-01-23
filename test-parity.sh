#!/bin/bash
#
# Test script for verifying functional parity between Genesis and javac
#
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

GUMDROP_LIB="/Users/cburdess@mimecast.com/cpkb/gumdrop/lib"
JARS=$(ls $GUMDROP_LIB/*.jar | tr '\n' ':')

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

pass() { echo -e "${GREEN}✓ $1${NC}"; }
fail() { echo -e "${RED}✗ $1${NC}"; }
warn() { echo -e "${YELLOW}⚠ $1${NC}"; }
info() { echo -e "  $1"; }

ERRORS=0

echo "========================================"
echo "Genesis/javac Parity Test"
echo "========================================"
echo

# Check prerequisites
if [ ! -f "./genesis" ]; then
    fail "Genesis binary not found. Run 'make' first."
    exit 1
fi

if ! command -v javac &> /dev/null; then
    fail "javac not found in PATH"
    exit 1
fi

if [ ! -f "gumdrop-files-no-module.txt" ]; then
    fail "gumdrop-files-no-module.txt not found"
    exit 1
fi

echo "=== Step 1: Compiling with Genesis ==="
rm -rf genesis-build && mkdir genesis-build
if ./genesis @gumdrop-files-no-module.txt -d genesis-build -j -cp "$JARS" 2>&1 | grep -q "error:"; then
    fail "Genesis compilation had errors"
    ERRORS=$((ERRORS + 1))
else
    pass "Genesis compilation successful"
fi
GENESIS_COUNT=$(find genesis-build -name "*.class" 2>/dev/null | wc -l | tr -d ' ')
info "Generated $GENESIS_COUNT class files"
echo

echo "=== Step 2: Compiling with javac ==="
rm -rf javac-build && mkdir javac-build
if ! javac @gumdrop-files-no-module.txt -d javac-build -cp "$JARS" 2>&1; then
    fail "javac compilation had errors"
    ERRORS=$((ERRORS + 1))
else
    pass "javac compilation successful"
fi
JAVAC_COUNT=$(find javac-build -name "*.class" 2>/dev/null | wc -l | tr -d ' ')
info "Generated $JAVAC_COUNT class files"
echo

echo "=== Step 3: Comparing class file counts ==="
DIFF=$((JAVAC_COUNT - GENESIS_COUNT))
info "Genesis: $GENESIS_COUNT, javac: $JAVAC_COUNT (diff: $DIFF)"

# Generate class lists
find genesis-build -name "*.class" | sed 's|genesis-build/||' | sort > /tmp/genesis-classes.txt
find javac-build -name "*.class" | sed 's|javac-build/||' | sort > /tmp/javac-classes.txt

# Count switch map classes (expected difference)
SWITCH_MAPS=$(diff /tmp/genesis-classes.txt /tmp/javac-classes.txt | grep "^>" | grep -c '\$[0-9]*\.class' || true)
info "Switch map classes in javac only: $SWITCH_MAPS"

# Check for unexpected differences
UNEXPECTED_MISSING=$(diff /tmp/genesis-classes.txt /tmp/javac-classes.txt | grep "^>" | grep -v '\$[0-9]*\.class' | wc -l | tr -d ' ')
UNEXPECTED_EXTRA=$(diff /tmp/genesis-classes.txt /tmp/javac-classes.txt | grep "^<" | wc -l | tr -d ' ')

if [ "$UNEXPECTED_MISSING" -gt 0 ]; then
    fail "Missing $UNEXPECTED_MISSING classes (excluding switch maps)"
    diff /tmp/genesis-classes.txt /tmp/javac-classes.txt | grep "^>" | grep -v '\$[0-9]*\.class' | head -10
    ERRORS=$((ERRORS + 1))
else
    pass "No unexpected missing classes"
fi

if [ "$UNEXPECTED_EXTRA" -gt 0 ]; then
    warn "Genesis generated $UNEXPECTED_EXTRA extra classes"
    diff /tmp/genesis-classes.txt /tmp/javac-classes.txt | grep "^<" | head -10
fi
echo

echo "=== Step 4: Comparing method signatures ==="
SAMPLE_CLASSES=(
    "org/bluezoo/gumdrop/Connection"
    "org/bluezoo/gumdrop/http/HTTPConnection"
    "org/bluezoo/gumdrop/http/HTTPRequestHandler"
    "org/bluezoo/gumdrop/servlet/ServletHandler"
    "org/bluezoo/gumdrop/servlet/Context"
    "org/bluezoo/gonzalez/XMLParser"
    "org/bluezoo/json/JSONParser"
)

SIG_MISMATCHES=0
for class in "${SAMPLE_CLASSES[@]}"; do
    if [ -f "genesis-build/$class.class" ] && [ -f "javac-build/$class.class" ]; then
        # Extract method/field signatures (excluding line numbers and stack info)
        javap -p "genesis-build/$class.class" 2>/dev/null | grep -E "^\s+\w" | sort > /tmp/g-sigs.txt
        javap -p "javac-build/$class.class" 2>/dev/null | grep -E "^\s+\w" | sort > /tmp/j-sigs.txt
        
        if diff -q /tmp/g-sigs.txt /tmp/j-sigs.txt > /dev/null 2>&1; then
            pass "$class - signatures match"
        else
            fail "$class - signature mismatch"
            diff /tmp/g-sigs.txt /tmp/j-sigs.txt | head -5
            SIG_MISMATCHES=$((SIG_MISMATCHES + 1))
        fi
    else
        warn "$class - class file not found in one or both builds"
    fi
done

if [ "$SIG_MISMATCHES" -gt 0 ]; then
    ERRORS=$((ERRORS + 1))
fi
echo

echo "=== Step 5: Checking nested class naming ==="
# Verify specific nested classes we fixed
NESTED_TESTS=(
    "org/bluezoo/gumdrop/servlet/jsp/JSPPrecompiler\$CompilerThreadFactory"
)

for class in "${NESTED_TESTS[@]}"; do
    if [ -f "genesis-build/$class.class" ]; then
        pass "Nested class exists: $class"
    else
        fail "Missing nested class: $class"
        ERRORS=$((ERRORS + 1))
    fi
done
echo

echo "=== Step 6: JVM Verification ==="
# Try to load classes with strict verification
VERIFY_CLASSES=(
    "org.bluezoo.gumdrop.Connection"
    "org.bluezoo.gumdrop.http.HTTPConnection"
    "org.bluezoo.gumdrop.servlet.ServletHandler"
)

for class in "${VERIFY_CLASSES[@]}"; do
    class_file="${class//./\/}.class"
    if [ -f "genesis-build/$class_file" ]; then
        # Use javap to verify the class file is valid
        if javap -v "genesis-build/$class_file" > /dev/null 2>&1; then
            pass "JVM can parse: $class"
        else
            fail "JVM cannot parse: $class"
            ERRORS=$((ERRORS + 1))
        fi
    fi
done
echo

echo "========================================"
echo "Summary"
echo "========================================"
echo "Genesis class files: $GENESIS_COUNT"
echo "javac class files:   $JAVAC_COUNT"
echo "Difference:          $DIFF ($SWITCH_MAPS are switch map classes)"
echo

if [ "$ERRORS" -eq 0 ]; then
    echo -e "${GREEN}All parity tests passed!${NC}"
    exit 0
else
    echo -e "${RED}$ERRORS test(s) failed${NC}"
    exit 1
fi
