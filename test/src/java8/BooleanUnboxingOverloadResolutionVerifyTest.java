/**
 * Bug: passing a boxed Boolean argument to a method with overloads
 * taking boolean/long/double (an "instanceof Boolean, then (Boolean)
 * cast and pass" pattern, matching gumdrop's own
 * MqttProtocolHandler.addSessionAttribute()) could resolve to the WRONG
 * overload - invoking addAttribute(String, long) or addAttribute(String,
 * double) instead of the only real match, addAttribute(String, boolean).
 * Root cause: type_needs_unboxing()'s "allow widening after unboxing"
 * logic (e.g. Integer unboxes to int, which widens to long) had no
 * switch case for TYPE_BOOLEAN, leaving its computed "rank" at the
 * default 0 - lower than every real numeric rank (even byte's 1) - so
 * "target_rank >= prim_rank" was true for EVERY numeric target,
 * wrongly reporting that Boolean unboxes-and-widens to long/double/etc.
 * just like a real numeric wrapper does. Overload resolution then scored
 * the boolean/long/double overloads as EQUALLY good unboxing-compatible
 * matches, and could pick either non-boolean one: an invokevirtual
 * expecting long/double where a Boolean reference is actually on the
 * stack. VerifyError "Bad type on operand stack ... not assignable to
 * long_2nd/double_2nd".
 */
public class BooleanUnboxingOverloadResolutionVerifyTest {
    static class Attrs {
        String last;
        void set(String key, String value) { last = "String:" + value; }
        void set(String key, boolean value) { last = "boolean:" + value; }
        void set(String key, long value) { last = "long:" + value; }
        void set(String key, double value) { last = "double:" + value; }
    }

    static void addSessionAttribute(Attrs attrs, String key, Object value) {
        if (value instanceof Boolean) {
            attrs.set(key, (Boolean) value);
        } else {
            attrs.set(key, String.valueOf(value));
        }
    }

    public static void main(String[] args) {
        Attrs attrs = new Attrs();
        addSessionAttribute(attrs, "flag", Boolean.TRUE);
        if (!"boolean:true".equals(attrs.last)) {
            throw new RuntimeException("expected boolean:true, got " + attrs.last);
        }
        addSessionAttribute(attrs, "flag2", Boolean.FALSE);
        if (!"boolean:false".equals(attrs.last)) {
            throw new RuntimeException("expected boolean:false, got " + attrs.last);
        }
    }
}
