/*
 * Regression test: an enum constant passing a `null` literal argument for
 * a reference-typed constructor parameter that ISN'T the last one (so
 * later parameters follow it) - matching gumdrop's own SignatureScheme,
 * an enum whose constructor is "(int code, String jcaAlgorithm, String
 * pssDigest, int pssSaltLength)" and whose RSA_PKCS1_* constants pass
 * literal "null" for pssDigest, e.g. "RSA_PKCS1_SHA256(0x0401,
 * "SHA256withRSA", null, 0)". Used to throw at class INITIALIZATION time
 * (not verification - this one loads fine but fails at runtime):
 *
 *   java.lang.NoSuchMethodError: 'void EnumConstantNullArg...(String,
 *   int, int, String, Object, int)'
 *
 * - note "Object" where the constructor's own real, declared parameter
 * type is "String".
 *
 * Root cause: codegen.c's enum-constant-construction loop (inside
 * <clinit>) built the invokespecial's constructor descriptor from EACH
 * ARGUMENT EXPRESSION's own inferred type (arg->sem_type, or
 * get_expression_type() as a fallback) - not from the constructor being
 * invoked. Those only coincidentally agree: a `null` literal's own
 * inferred type is TYPE_NULL, which type_to_descriptor() has no specific
 * case for and so erases to its generic fallback, "Ljava/lang/Object;"
 * - not the constructor's real declared parameter type at that
 * position. The resulting invokespecial then referenced a descriptor
 * that doesn't match the constructor actually compiled for the class at
 * all; the verifier doesn't check that a referenced method exists, so
 * this only surfaces later, as a NoSuchMethodError when the enum's
 * <clinit> actually runs.
 *
 * Fixed by looking up the enum's own declared constructor (matched by
 * parameter count - enums practically never overload their constructor)
 * and building the descriptor from ITS declared parameter types instead,
 * falling back to the old per-argument inference only if no matching
 * constructor is found.
 */
public enum EnumConstantNullArgConstructorDescriptorVerifyTest {
    A(1, "hello", null, 0),
    B(2, "world", "digest", 5);

    private final int code;
    private final String algo;
    private final String digest;
    private final int len;

    EnumConstantNullArgConstructorDescriptorVerifyTest(int code, String algo, String digest, int len) {
        this.code = code;
        this.algo = algo;
        this.digest = digest;
        this.len = len;
    }

    public static void main(String[] args) {
        if (A.code != 1 || !"hello".equals(A.algo) || A.digest != null || A.len != 0) {
            throw new RuntimeException("bad A: " + A.code + " " + A.algo + " " + A.digest + " " + A.len);
        }
        if (B.code != 2 || !"world".equals(B.algo) || !"digest".equals(B.digest) || B.len != 5) {
            throw new RuntimeException("bad B: " + B.code + " " + B.algo + " " + B.digest + " " + B.len);
        }

        System.out.println("EnumConstantNullArgConstructorDescriptorVerifyTest passed!");
    }
}
