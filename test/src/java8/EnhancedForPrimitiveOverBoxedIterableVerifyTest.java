/*
 * Regression test: a primitive-typed enhanced-for loop variable iterating
 * a boxed Iterable (e.g. "for (int v : list)" over a List<Integer>) must
 * auto-unbox each element (JLS 14.14.2) - matching what an ordinary
 * assignment of a boxed value to a primitive-typed variable already does.
 * Used to throw at class-verification time:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *
 * (a reference from Iterator.next() stored directly into a primitive-
 * typed local slot, with no unboxing conversion at all).
 *
 * Root cause: codegen_stmt.c's AST_ENHANCED_FOR_STMT codegen, in the
 * Iterable (non-array) branch, only handled the loop variable's own type
 * node being AST_CLASS_TYPE (emit a reference CHECKCAST) or
 * AST_ARRAY_TYPE (emit an array CHECKCAST) after Iterator.next() - there
 * was no case at all for AST_PRIMITIVE_TYPE, so the raw boxed reference
 * fell straight through to mg_emit_store_local() with the primitive
 * var_kind, storing a reference where the JVM verifier expects (and the
 * emitted store opcode assumes) a primitive value. Separately, the loop
 * variable's own local slot was always allocated as a single JVM slot
 * (correct for a reference, since Iterable always yields one before any
 * unboxing) - too narrow for a primitive long/double loop variable, which
 * needs two slots once unboxed.
 *
 * Fixed by adding an AST_PRIMITIVE_TYPE case that emits a CHECKCAST to
 * the loop variable's own wrapper class (e.g. Integer for int, Long for
 * long) followed by the matching unboxing call (intValue()/longValue()/
 * etc, via the existing emit_unboxing() helper) - matching real javac's
 * own emitted bytecode for this exact construct - and by sizing the loop
 * variable's slot allocation from the primitive kind (2 slots for
 * long/double, 1 otherwise) instead of assuming a reference always.
 */
public class EnhancedForPrimitiveOverBoxedIterableVerifyTest {
    public static void main(String[] args) {
        java.util.List<Integer> ints = java.util.Arrays.asList(1, 2, 3, 4, 5);
        int intSum = 0;
        for (int v : ints) {
            intSum += v;
        }
        if (intSum != 15) {
            throw new RuntimeException("expected intSum=15, got " + intSum);
        }

        java.util.List<Long> longs = java.util.Arrays.asList(10L, 20L, 30L);
        long longSum = 0;
        for (long v : longs) {
            longSum += v;
        }
        if (longSum != 60L) {
            throw new RuntimeException("expected longSum=60, got " + longSum);
        }

        java.util.List<Double> doubles = java.util.Arrays.asList(1.5, 2.5, 3.0);
        double doubleSum = 0;
        for (double v : doubles) {
            doubleSum += v;
        }
        if (doubleSum != 7.0) {
            throw new RuntimeException("expected doubleSum=7.0, got " + doubleSum);
        }

        java.util.List<Character> chars = java.util.Arrays.asList('a', 'b', 'c');
        StringBuilder sb = new StringBuilder();
        for (char c : chars) {
            sb.append(c);
        }
        if (!"abc".equals(sb.toString())) {
            throw new RuntimeException("expected abc, got " + sb);
        }

        java.util.List<Boolean> bools = java.util.Arrays.asList(true, true, false);
        int trueCount = 0;
        for (boolean b : bools) {
            if (b) {
                trueCount++;
            }
        }
        if (trueCount != 2) {
            throw new RuntimeException("expected trueCount=2, got " + trueCount);
        }

        System.out.println("EnhancedForPrimitiveOverBoxedIterableVerifyTest passed!");
    }
}
