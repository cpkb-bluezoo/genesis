import java.nio.file.CopyOption;
import java.nio.file.LinkOption;

/**
 * Bug: two overloaded methods differing ONLY in an array parameter's
 * element type (e.g. "follows(LinkOption... options)" and
 * "follows(CopyOption... options)", both erasing to a single TYPE_ARRAY
 * parameter) collapsed to the exact same overload-disambiguating key
 * during class member registration - the method-key builder used for a
 * plain, directly-compiled class's own AST_METHOD_DECL nodes (the main
 * semantic pass, semantic.c) represented EVERY array-typed parameter as
 * a bare "[" with no element-type information at all. The second
 * declaration's registration then silently overwrote the first in the
 * class's method hashtable, so only ONE of the two overloads was ever
 * resolvable at any call site - regardless of which one the calling
 * code's argument types actually required.
 *
 * A call site passing an array whose element type matched the now-
 * vanished overload got resolved against the SURVIVING (wrong) one
 * instead; since that overload's varargs parameter type didn't match
 * the actual argument's element type, codegen's array-to-varargs
 * pass-through check correctly declined to pass the array directly -
 * but then wrapped the WHOLE ARRAY as a single vararg element instead
 * of individual elements, producing a runtime
 * "ArrayStoreException: [Ljava.nio.file.LinkOption;" (storing an array
 * into a slot that expects a single element of a DIFFERENT type -
 * LinkOption implements CopyOption, so both overloads are genuinely
 * applicable to a LinkOption[] argument, and real javac picks the more
 * specific LinkOption... one). Confirmed against gumdrop's own
 * MemoryFileSystemProvider (test support code), whose
 * readAttributes() calls "follows(options)" (options declared
 * LinkOption...) - this hit exactly this bug via java.nio.file's own
 * LinkOption/CopyOption overload pair.
 */
public class ArrayParamOverloadKeyVerifyTest {
    private static boolean follows(LinkOption... options) {
        for (LinkOption o : options) {
            if (o == LinkOption.NOFOLLOW_LINKS) {
                return false;
            }
        }
        return true;
    }

    private static boolean follows(CopyOption... options) {
        throw new RuntimeException("wrong overload chosen: follows(CopyOption...)");
    }

    static boolean check(LinkOption... options) {
        return follows(options);
    }

    public static void main(String[] args) {
        if (check(LinkOption.NOFOLLOW_LINKS)) {
            throw new RuntimeException("expected false for NOFOLLOW_LINKS");
        }
        if (!check()) {
            throw new RuntimeException("expected true for no options");
        }
    }
}
