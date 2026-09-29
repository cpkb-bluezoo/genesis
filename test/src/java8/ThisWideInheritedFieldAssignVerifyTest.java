/*
 * Regression test: assigning to an inherited wide (long/double) field
 * via an explicit "this.field = value;" - e.g.
 * "class Sub extends Base { Sub(long v) { this.deliveryCount = v; } }"
 * where "deliveryCount" is declared as a "long" on the SUPERCLASS
 * (Base), not on Sub itself - used to generate:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type long_2nd (current frame, stack[N]) is not assignable
 *           to category1 type
 *
 * matching gumdrop's own AMQP 1.0 client, SenderImpl's constructor,
 * whose "this.deliveryCount = attach.getInitialDeliveryCount()
 * .longValue();" statement assigns a superclass-declared "long
 * deliveryCount" field (declared on LinkImpl, not SenderImpl itself).
 *
 * Root cause: codegen_expr.c's codegen_assignment() has two lookups for
 * an instance field assignment target: one using the receiver
 * expression's own sem_type (correctly superclass-aware, via
 * lookup_field_with_superclass()), and a SEPARATE, narrower one
 * specifically for a bare "this" receiver (AST_THIS_EXPR), which only
 * ever checked mg->class_gen->field_map - a hashtable holding fields
 * declared DIRECTLY on the current class, never anything inherited.
 * The first (correct) lookup never even ran for a bare "this" receiver,
 * since an AST_THIS_EXPR node carries no sem_type for that check to key
 * off. For an inherited field, this left the field's descriptor at its
 * hardcoded "I" (int) default, so the later choice between DUP_X1 (for
 * a category-1 value) and DUP2_X1 (for a category-2 long/double value,
 * needed here) picked the narrow one - corrupting the operand stack.
 * Fixed by falling back to the same lookup_field_with_superclass() walk
 * already used by the sem_type-based branch, when the own-class
 * field_map lookup for a "this.field" target misses.
 */
public class ThisWideInheritedFieldAssignVerifyTest {
    static class Base {
        long deliveryCount;
        double ratio;
    }

    static class Sub extends Base {
        final String label;

        Sub(String label, long count, double ratio) {
            this.label = label;
            this.deliveryCount = count;
            this.ratio = ratio;
        }
    }

    public static void main(String[] args) {
        Sub s = new Sub("h", 42L, 3.5);
        if (s.deliveryCount != 42L) {
            throw new RuntimeException("expected deliveryCount=42, got " + s.deliveryCount);
        }
        if (s.ratio != 3.5) {
            throw new RuntimeException("expected ratio=3.5, got " + s.ratio);
        }
        if (!"h".equals(s.label)) {
            throw new RuntimeException("expected label=h, got " + s.label);
        }
        System.out.println("ThisWideInheritedFieldAssignVerifyTest passed!");
    }
}
