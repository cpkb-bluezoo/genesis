import java.util.Arrays;
import java.util.List;

/*
 * Regression test: a static method call through a fully-qualified (dotted)
 * class name - e.g. "java.text.MessageFormat.format(pattern, name, list)",
 * matching gumdrop's own Amqp1ClientProtocolHandler.startSasl() exactly -
 * used to invoke the WRONG overload whenever the target class both:
 *   1. declares a STATIC VARARGS method of that name (here,
 *      MessageFormat's "static String format(String pattern,
 *      Object... arguments)"), and
 *   2. also declares one or more FIXED-ARITY (non-varargs, often instance)
 *      overloads of the same name whose declared parameter count happens
 *      to equal the call site's raw argument count (here, MessageFormat's
 *      own instance "StringBuffer format(Object[] arguments, StringBuffer
 *      result, FieldPosition pos)" - both the call site and this overload
 *      have "3" arguments/parameters, once the varargs call's trailing
 *      arguments are counted individually instead of collapsed into a
 *      single array).
 *
 * This produced java.lang.VerifyError: Bad type on operand stack at the
 * invokestatic, because the emitted constant-pool Methodref used the
 * WRONG overload's descriptor while the actual bytecode on the stack was
 * built for the (correct) static varargs one.
 *
 * Root cause: semantic analysis (semantic.c's AST_FIELD_ACCESS handling
 * for a method call whose receiver resolves to a class, e.g.
 * "java.util.Objects.requireNonNull(...)") correctly resolves the callee
 * via find_best_method_by_types(), which does real type-based overload
 * resolution and stores the right symbol on expr->sem_symbol. But
 * codegen_expr.c's codegen_method_call() - specifically its own "Check if
 * the field access is actually a class reference (FQN like
 * java.util.Objects)" branch - UNCONDITIONALLY re-resolved the callee a
 * second time via scope_lookup_method_with_args(), which matches by raw
 * argument COUNT ALONE (no static/instance filtering, no type checking,
 * no varargs-vs-fixed-arity distinction), and then overwrote the already-
 * correct expr->sem_symbol with whatever same-arity overload it found
 * first in hashtable bucket order. Fixed by only using that fallback
 * lookup when semantic analysis didn't already resolve a method
 * (mirroring the "prefer semantic analysis result" pattern already used
 * by every other call-resolution branch in the same function).
 */
public class QualifiedStaticVarargsOverloadVerifyTest {

    static String describe(String name, List<String> offered) {
        return java.text.MessageFormat.format(
                "Mechanism {0} not offered (offered: {1})", name, offered);
    }

    public static void main(String[] args) {
        List<String> offered = Arrays.asList("PLAIN", "EXTERNAL");
        String result = describe("GSSAPI", offered);
        String expected = "Mechanism GSSAPI not offered (offered: [PLAIN, EXTERNAL])";
        if (!expected.equals(result)) {
            throw new RuntimeException(
                    "expected \"" + expected + "\", got \"" + result + "\"");
        }
        System.out.println("QualifiedStaticVarargsOverloadVerifyTest passed!");
    }
}
