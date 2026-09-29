/*
 * Deliberately a SEPARATE top-level compilation unit from
 * MultiCatchExternalLibExceptionTest.java, compiled in the same genesis
 * invocation - see the comment there for why this cross-file pairing (not
 * a nested class in the same file) is what actually reproduces the bug.
 */
public class MultiCatchLocalException extends Exception {
    public MultiCatchLocalException(String m) {
        super(m);
    }
}
