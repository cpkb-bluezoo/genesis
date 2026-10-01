import java.io.*;
import java.nio.file.*;

/*
 * Bug: an exception thrown WHILE EVALUATING a try-with-resources
 * statement's own resource initializer expression (before the resource
 * variable is ever assigned) escaped every catch clause on that same try
 * statement, instead of being caught by a matching one - even though
 * JLS 14.20.3.1 makes a resource's initializer part of the try statement
 * itself, fully subject to its catch clauses. Confirmed against
 * gumdrop's own SharedLockStore.createRecord(): "try (OutputStream out =
 * Files.newOutputStream(record, CREATE_NEW)) { ... } catch
 * (FileAlreadyExistsException e) { ... }" - the exception is thrown BY
 * newOutputStream() itself (the resource initializer), so "out" is never
 * assigned, yet the real bug let the FileAlreadyExistsException escape
 * uncaught (as an UncheckedIOException wrapping it, from higher up)
 * instead of being handled right there.
 *
 * Root cause: codegen_try_with_resources() (codegen_stmt.c) generated
 * each declaration-form resource's initializer expression, and stored it
 * into its (freshly allocated) local slot, BEFORE recording "try_start" -
 * the offset used as the START of every exception-table entry on this try
 * statement, both the synthetic close-with-suppression handler and every
 * user catch clause. Any exception thrown while evaluating the
 * initializer itself was therefore, by construction, always OUTSIDE
 * every one of this try statement's own protected regions.
 *
 * Fixed by splitting resource setup into two passes: first, allocate
 * each declaration-form resource's local slot and pre-initialize it to
 * null (so the local is always validly assigned from this point on,
 * exactly like real javac's own try-with-resources desugaring); THEN
 * record try_start; THEN, in a second pass, actually evaluate each
 * resource's real initializer expression and store it into its
 * (already pre-nulled) slot - now safely inside the protected region.
 */
public class TwrResourceInitializerCaughtVerifyTest {
    static boolean createRecord(Path record) throws IOException {
        try (OutputStream out = Files.newOutputStream(record, StandardOpenOption.CREATE_NEW)) {
            out.write(1);
            return true;
        } catch (FileAlreadyExistsException e) {
            return false;
        }
    }

    public static void main(String[] args) throws Exception {
        Path p = Files.createTempFile("twr-resource-init", ".txt");
        try {
            boolean result = createRecord(p);
            if (result) {
                throw new RuntimeException("expected false (file already exists) but got true");
            }
            System.out.println("TwrResourceInitializerCaughtVerifyTest passed!");
        } finally {
            Files.deleteIfExists(p);
        }
    }
}
