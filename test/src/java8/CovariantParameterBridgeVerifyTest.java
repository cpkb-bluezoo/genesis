import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;

/*
 * Regression test: a subclass overriding a generic superclass method with a
 * COVARIANT PARAMETER TYPE (java.nio.file.SimpleFileVisitor<T>'s own
 * "visitFile(T file, BasicFileAttributes attrs)", overridden here as
 * "visitFile(Path file, BasicFileAttributes attrs)") was silently never
 * invoked through the superclass's own entry points, for the same general
 * reason as CovariantReturnBridgeVerifyTest.java's own covariant-RETURN
 * case: java.nio.file.Files.walkFileTree() calls the visitor through the
 * ERASED "visitFile(Object, BasicFileAttributes)" entry point (the only one
 * SimpleFileVisitor itself declares), so the anonymous subclass needs a
 * synthetic bridge with that exact erased descriptor forwarding to the real,
 * concrete override - without it, the call falls back to SimpleFileVisitor's
 * own inherited no-op (CONTINUE, does nothing) instead of the real override,
 * silently - not with an AbstractMethodError or VerifyError.
 *
 * Root cause: generate_covariant_override_bridges() (codegen.c) found the
 * class's own override (needed to know what to forward TO) by comparing the
 * superclass method's own fully-ERASED parameter descriptor string
 * ("Ljava/lang/Object;Ljava/nio/file/attribute/BasicFileAttributes;") against
 * the override's own fully-CONCRETE parameter descriptor string
 * ("Ljava/nio/file/Path;Ljava/nio/file/attribute/BasicFileAttributes;") for
 * flat string equality - which, for a type-variable PARAMETER (as opposed to
 * only a type-variable RETURN type, where this comparison degenerates to
 * comparing two empty parameter lists and happens to still work), can never
 * match any real covariant override at all: that mismatch between the
 * erasure and the concrete type is the entire reason a bridge is needed.
 * impl_method was therefore never found, and no bridge was ever generated.
 * Fixed by comparing parameter types POSITION BY POSITION instead: a
 * superclass parameter that's a bare type variable matches ANY candidate
 * parameter at that position, while a non-type-variable position must still
 * match exactly (to correctly disambiguate same-arity overloads sharing a
 * name with the type-variable-bearing superclass method).
 *
 * Before the fix: prints "FAIL: visitFile() never ran, seen=[]".
 * After the fix: prints "CovariantParameterBridgeVerifyTest passed!".
 */
public class CovariantParameterBridgeVerifyTest {
    public static void main(String[] args) throws IOException {
        Path dir = Files.createTempDirectory("covariantParamBridgeTest");
        Path f = dir.resolve("f.txt");
        Files.write(f, new byte[] { 'h', 'i' });

        final List<String> seen = new ArrayList<String>();
        Files.walkFileTree(dir, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                seen.add(file.toString());
                return FileVisitResult.CONTINUE;
            }
        });

        Files.deleteIfExists(f);
        Files.deleteIfExists(dir);

        if (seen.isEmpty()) {
            throw new RuntimeException("FAIL: visitFile() never ran, seen=" + seen);
        }
        if (seen.size() != 1 || !seen.get(0).equals(f.toString())) {
            throw new RuntimeException("FAIL: unexpected visitFile() result, seen=" + seen);
        }
        System.out.println("CovariantParameterBridgeVerifyTest passed!");
    }
}
