package crossfileenummethodname;

/*
 * Regression test: a chained method call ending in `.name()` on an enum
 * value returned by a method call, where the enum type is declared in a
 * SEPARATE source file from both the caller and the intermediate return
 * type ("q.getType().name()", with Kind/Question/Metrics all in
 * different files, compiled together in one genesis invocation) -
 * matching gumdrop's own DoQStreamHandler.processAccumulatedQuery(),
 * whose "metrics.queryReceived(q.getType().name(), \"doq\");" does
 * exactly this (DnsQuestion.getType() returns the cross-file enum
 * DnsType). Used to throw at class-verification time:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type 'Kind' (current frame, stack[N]) is not assignable to
 *   'CrossFileEnumMethodChainNameVerifyTest'
 *
 * Root cause: codegen_expr.c's general method-call codegen
 * (codegen_method_call()) has several branches that resolve an explicit
 * receiver's own class to use as the invokevirtual's target - but the
 * branch handling a receiver that is ITSELF a method call
 * (AST_METHOD_CALL, e.g. "q.getType()" as the receiver of ".name()")
 * only set "receiver", never "target_class". When the called method
 * also couldn't be resolved to a symbol in genesis's own symbol tables
 * (true for an enum's INHERITED java.lang.Enum built-ins like name() -
 * unlike ordinal(), which had its own dedicated handling for switch
 * codegen only, name() had no handling anywhere), target_class stayed
 * unset all the way to the final "if (!target_class)" fallback, which
 * defaulted to the ENCLOSING class instead of the receiver's real type -
 * producing an invokevirtual whose owner was completely wrong.
 *
 * Fixed by preferring the receiver's own resolved type in that final
 * fallback (for a non-static call with a known receiver), mirroring the
 * same "receiver's own type over enclosing class" preference already
 * used by several sibling branches for other receiver expression kinds.
 * JVMS 5.4.3.3: invokevirtual's own method resolution already walks the
 * receiver class's superclass chain, so naming the receiver's own class
 * is correct even for an inherited method never redeclared there -
 * exactly what real javac itself emits for this shape.
 */
public class CrossFileEnumMethodChainNameVerifyTest {
    public static void main(String[] args) {
        Metrics metrics = new Metrics();
        Question q = new Question(Kind.B);

        metrics.queryReceived(q.getType().name(), "doq");

        if (!"B".equals(metrics.getLastType())) {
            throw new RuntimeException("expected B, got " + metrics.getLastType());
        }
        if (!"doq".equals(metrics.getLastProto())) {
            throw new RuntimeException("expected doq, got " + metrics.getLastProto());
        }

        System.out.println("CrossFileEnumMethodChainNameVerifyTest passed!");
    }
}
