/*
 * Regression test: calling a nested enum's synthetic `valueOf(String)`
 * method, followed later in the same method by an unrelated varargs
 * call whose stack-depth bookkeeping depends on everything before it
 * having been tracked correctly - matching gumdrop's own
 * DnssecTrustAnchorUpdater.loadState(), which does "KeyState state =
 * KeyState.valueOf(parts[1]);" (KeyState a nested enum) followed later
 * by a catch block's own "LOGGER.log(Level.WARNING, MessageFormat.
 * format(...), e);". Used to throw at class-verification time:
 *
 *   java.lang.VerifyError: Operand stack overflow
 *   Reason: Exceeded max stack size.
 *
 * Root cause: codegen_expr.c's codegen_method_call() computes the
 * number of stack slots a call's own arguments occupy (needed to pop
 * them correctly after the call) from `method_sym->data.method_data.
 * parameters` whenever `method_sym` is non-null - but for an enum's
 * synthetic `values()`/`valueOf(String)` methods, nothing in genesis's
 * own symbol tables actually declares them (they're compiler-generated,
 * not user-declared), so semantic analysis's own method-symbol
 * resolution can land on a completely unrelated same-named method from
 * a different class instead of leaving it unresolved. The call's own
 * ACTUAL descriptor is built correctly, independently, into
 * `custom_descriptor` specifically for this synthetic case - but the
 * arg-slot counting never consulted it, trusting the (here, wrong)
 * `method_sym` instead. Popping the wrong number of slots after the
 * call wrapped mg->stack_depth's own (unsigned) counter to a huge
 * value, corrupting every later max_stack comparison for the rest of
 * the method.
 *
 * Fixed by parsing arg_slots directly from custom_descriptor's own
 * parameter list (via the existing descriptor_parse_method() classfile
 * utility) whenever custom_descriptor is set, ahead of the method_sym-
 * based paths.
 */
public class EnumValueOfArgSlotCountVerifyTest {
    enum KeyState { A, B, C, D }

    public static void main(String[] args) {
        KeyState state = KeyState.valueOf("B");
        if (state != KeyState.B) {
            throw new RuntimeException("expected B, got " + state);
        }

        try {
            throw new RuntimeException("boom");
        } catch (RuntimeException e) {
            String msg = java.text.MessageFormat.format("state={0}, cause={1}", state, e.getMessage());
            if (!"state=B, cause=boom".equals(msg)) {
                throw new RuntimeException("expected state=B, cause=boom, got " + msg);
            }
        }

        System.out.println("EnumValueOfArgSlotCountVerifyTest passed!");
    }
}
