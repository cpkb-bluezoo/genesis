/**
 * Bug: an array initializer literal whose declared element type is a
 * reference type (e.g. `Object[]`) never autoboxed an individual
 * element whose own expression is primitive - only the numeric
 * WIDENING conversions (int->long/float/double, etc.) were handled,
 * missing primitive-to-reference boxing entirely. A raw primitive (2
 * stack words for `long`/`double`) got stored straight into a
 * reference-typed array slot via AASTORE: VerifyError "Bad type on
 * operand stack ... not assignable to 'java/lang/Object'". Matches
 * gumdrop's own LossDetector.ptoTimeAndSpace()'s
 * "new Object[] { nowMillis + duration, space }".
 *
 * The fix's own boxing check needed an explicit exclusion for a
 * WRAPPER-typed element (e.g. "Long.valueOf(7)", already a reference on
 * the stack): get_expr_type_kind() deliberately reports such an
 * expression's own UNDERLYING PRIMITIVE kind "for arithmetic" purposes,
 * which would otherwise have looked identical to a genuinely primitive
 * expression and gotten RE-boxed - corrupting the stack (a 1-word
 * reference where emit_boxing() expects a 2-word primitive `long`).
 * "alreadyBoxed" below exercises exactly that shape.
 */
public class ObjectArrayLiteralAutoboxVerifyTest {
    static Object[] pair(long value, String label) {
        return new Object[] { value, label };
    }

    static Object[] alreadyBoxed(long value) {
        return new Object[] { Long.valueOf(value), "tag" };
    }

    public static void main(String[] args) {
        Object[] r = pair(42L, "answer");
        long v = (Long) r[0];
        String s = (String) r[1];
        if (v != 42L || !"answer".equals(s)) {
            throw new RuntimeException("expected 42/answer, got " + v + "/" + s);
        }

        Object[] r2 = alreadyBoxed(7L);
        long v2 = (Long) r2[0];
        if (v2 != 7L || !"tag".equals(r2[1])) {
            throw new RuntimeException("expected 7/tag, got " + v2 + "/" + r2[1]);
        }
    }
}
