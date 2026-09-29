/*
 * Regression test: a constructor whose LAST parameter is varargs, and
 * which is NOT the constructor's only parameter, got the wrong
 * descriptor in the classfile - the varargs parameter's own array type
 * ([Ljava/lang/String; for `String...`) was recorded as its bare
 * element type (Ljava/lang/String;) instead. Matches gumdrop's own
 * SpkiPinnedCertTrustManager(X509TrustManager delegate,
 * String... fingerprints):
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type 'java/lang/String' (current frame, stack[3]) is not
 *   assignable to '[Ljava/lang/Object;'
 *
 * thrown from inside the constructor's own body, the moment it passed
 * the varargs parameter to Arrays.asList(T...) - the verifier trusted
 * the classfile's own declared descriptor for local slot 2, which said
 * plain String, not String[].
 *
 * Root cause: codegen.c's constructor-descriptor-building loop (inside
 * the AST_METHOD_DECL codegen, the `is_constructor` branch) built each
 * parameter's descriptor fragment directly from the AST parameter's own
 * type node via ast_type_to_descriptor(), without ever checking that
 * parameter's own MOD_VARARGS flag - unlike a varargs *method* (the
 * sibling branch just below, using method_to_descriptor(method_sym),
 * which builds from the symbol table's own parameter types, already
 * array-wrapped for varargs elsewhere in semantic.c) and unlike this
 * same constructor's own local-variable-slot allocation a bit earlier
 * in this file (which already checks MOD_VARARGS for exactly this AST
 * shape). A varargs parameter's AST type node is its *element* type (T
 * in "T... name"), per JLS 8.4.1 the parameter's real type is T[] - the
 * descriptor loop needed the identical array-wrapping and didn't have
 * it, only for constructors.
 *
 * Fixed by prepending "[" to a varargs parameter's own descriptor
 * fragment in that loop, mirroring the array-wrapping already applied
 * elsewhere for the same AST shape.
 */
public class VarargsConstructorDescriptorVerifyTest {

    // Varargs preceded by a non-varargs parameter - matches gumdrop's own
    // SpkiPinnedCertTrustManager(X509TrustManager delegate, String...
    // fingerprints). A single such constructor, deliberately with no
    // sibling overload, to keep this test focused on the descriptor bug
    // rather than overload resolution.
    static class Pinner {
        final String label;
        final java.util.Set<String> values;

        Pinner(String label, String... values) {
            this.label = label;
            this.values = new java.util.HashSet<String>(java.util.Arrays.asList(values));
        }
    }

    public static void main(String[] args) {
        Pinner labeled = new Pinner("custom", "x", "y");
        if (!"custom".equals(labeled.label)) {
            throw new RuntimeException("expected label=custom, got " + labeled.label);
        }
        if (!labeled.values.equals(new java.util.HashSet<String>(
                java.util.Arrays.asList("x", "y")))) {
            throw new RuntimeException("expected [x, y], got " + labeled.values);
        }

        Pinner empty = new Pinner("empty");
        if (!empty.values.isEmpty()) {
            throw new RuntimeException("expected empty, got " + empty.values);
        }

        System.out.println("VarargsConstructorDescriptorVerifyTest passed!");
    }
}
