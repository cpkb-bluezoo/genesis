/*
 * Regression test: a ternary expression whose two branches both resolve
 * to the exact same classfile-loaded (JDK library) type - exactly
 * gumdrop's own MboxMailbox shape:
 *
 *   FileChannel channel = readOnly
 *           ? FileChannel.open(mboxFile, StandardOpenOption.READ)
 *           : FileChannel.open(mboxFile, StandardOpenOption.READ, StandardOpenOption.WRITE);
 *
 * used to throw a compile-time error:
 *
 *   error: Incompatible types: cannot convert java.lang.Object to
 *   java.nio.channels.FileChannel
 *
 * even though both branches are unambiguously FileChannel.
 *
 * Root cause: a regression in the ternary-sibling-interface-types fix
 * (find_common_ancestor_symbol(), added to compute a common ancestor
 * when neither branch is a subtype of the other) - unlike the sibling
 * "one branch is a subtype of the other" check just above it in
 * get_expression_type()'s AST_CONDITIONAL_EXPR case, the new LUB block
 * didn't first check `then_type != else_type` before doing symbol-based
 * ancestor lookup. For a classfile-loaded library type like FileChannel,
 * whose type_t often carries no ->symbol at all (two separate
 * resolutions of the same call produce two distinct type_t instances
 * with the same name but a NULL ->symbol on each), find_common_ancestor_symbol()
 * had nothing to walk and fell back to java.lang.Object - discarding
 * perfectly valid, matching type information for what was actually an
 * exact match. Fixed by checking the two branches' class names for
 * equality first, returning immediately when they match, before ever
 * attempting a symbol-based ancestor search.
 */
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

public class TernarySameClassfileLoadedTypeVerifyTest {
    static long openAndSize(boolean readOnly, Path p) throws IOException {
        FileChannel channel = readOnly
                ? FileChannel.open(p, StandardOpenOption.READ)
                : FileChannel.open(p, StandardOpenOption.READ, StandardOpenOption.WRITE);
        try {
            return channel.size();
        } finally {
            channel.close();
        }
    }

    public static void main(String[] args) throws IOException {
        Path p = Files.createTempFile("ternary-same-type-test", ".txt");
        try {
            Files.write(p, "hello".getBytes());
            long size = openAndSize(true, p);
            if (size != 5) {
                throw new AssertionError("expected size 5, got " + size);
            }
            System.out.println("TernarySameClassfileLoadedTypeVerifyTest passed!");
        } finally {
            Files.deleteIfExists(p);
        }
    }
}
