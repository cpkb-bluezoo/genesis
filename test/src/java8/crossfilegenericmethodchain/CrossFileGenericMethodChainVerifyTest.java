package crossfilegenericmethodchain;

/*
 * Regression test: a three-level chained call -
 * "registry.find(Widget.class).get(i).getValue()" - used directly as an
 * argument expression (no intermediate local variable), where the FIRST
 * call is to a generic method whose type argument is inferred from a
 * Class<T> argument (Widget.class), and Registry (declaring find()) is
 * compiled from a DIFFERENT file than the one making the call - matching
 * gumdrop's real-world shape in Amqp1ReceiverTest.testEchoedFlowIsAnswered:
 *
 *   assertEquals(Long.valueOf(5), h.sent(Flow.class).get(before).getLinkCredit());
 *
 * This used to compile silently but produce a bogus invokevirtual whose
 * owner and descriptor were both wrong:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type 'java/lang/Object' ... is not assignable to
 *     '...CrossFileGenericMethodChainVerifyTest'
 *
 * Root cause: Registry.find()'s parameter type - "Class<T>", where T is
 * the METHOD's own type variable, not the class's - is read back from a
 * DIFFERENT file (this one) via the unresolved_type_t/type-registry
 * cross-file resolution path (resolve_unresolved_type_full_for_method()
 * in semantic.c), not from find()'s own AST (only same-file resolution
 * ever reads that directly). That function's handling of a bare,
 * non-parameterized type argument (e.g. plain "T") already consulted the
 * enclosing method's own type parameters via lookup_method_type_param() -
 * but as soon as the parameter type was ITSELF parameterized (Class<T>,
 * not just T), it fell through to the plain, non-method-aware
 * resolve_unresolved_type_full(), whose own type-argument loop has no
 * "method" to consult when resolving the nested "T" - so it silently
 * failed to resolve, and the parameter's type came out as bare "Class"
 * with no type arguments at all. Without that type argument,
 * infer_type_arg() had nothing to match "T" against find()'s actual
 * argument type (Class<Widget>), so type inference for the whole call
 * failed silently and find()'s return type stayed the unsubstituted
 * "List<T>" instead of "List<Widget>" - which cascaded into the outer
 * ".get(i).getValue()" chain never resolving a real method symbol at
 * all, falling back to codegen's last-resort "implicit call on the
 * current class" path (return type defaulted to int, owner defaulted to
 * the calling class itself).
 *
 * Fixed by having resolve_unresolved_type_full_for_method() resolve a
 * parameterized type's own nested type arguments recursively through
 * itself (still method-aware at every level) instead of delegating to
 * the plain, method-oblivious resolve_unresolved_type_full().
 */
public class CrossFileGenericMethodChainVerifyTest {

    static String describe(Registry registry, int index) {
        return "value=" + registry.find(Widget.class).get(index).getValue();
    }

    public static void main(String[] args) {
        Registry registry = new Registry();
        registry.add(new Object());
        registry.add(new Widget(42));
        registry.add(new Object());
        registry.add(new Widget(7));

        String result = describe(registry, 1);
        if (!"value=7".equals(result)) {
            throw new RuntimeException("expected value=7 but got " + result);
        }
        System.out.println("CrossFileGenericMethodChainVerifyTest passed!");
    }
}
