package varargstest;

/*
 * Regression test: a varargs call used as a NESTED argument to another
 * call (as opposed to a plain statement/assignment) used to fail to
 * resolve, because resolving the outer call's argument type re-resolves
 * the inner varargs call's overload a second time, and that second
 * resolution corrupted the varargs parameter's type from an array type
 * back to just its element type - losing the "expected T..." to "T[]"
 * conversion done for the first resolution. See genesis history for
 * details (search "resolve_types_for_symbol" / MOD_VARARGS in
 * semantic.c).
 */
public class VarargsForwardingNestedCallTest {
    static void takeBytes(byte[] b) {
    }

    void feed(java.nio.ByteBuffer... parts) {
        takeBytes(Concatenator.concat(parts));
    }
}
