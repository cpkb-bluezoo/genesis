import java.io.BufferedReader;
import java.io.StringReader;
import java.io.IOException;

/*
 * A local declared before a try-with-resources statement and assigned
 * only inside its body was seen as "top" (unassigned) immediately after
 * the construct, even though the only real control-flow edge reaching
 * that point (the normal exit) does assign it - the exception-handling/
 * resource-cleanup codegen generated AFTER the normal path's own exit
 * goto left mg->stackmap reflecting ITS OWN state (via a
 * stackmap_restore_state() back to try-entry state, for its own
 * legitimate purposes) instead of the normal path's, and the join-point
 * frame recorded after the whole construct wrongly read from that
 * leftover state. codegen_try_with_resources() (codegen_stmt.c) now
 * snapshots the normal path's own exit state (and the smallest
 * fall-through catch clause's exit state, if any) before that codegen
 * runs, and explicitly restores the correct one(s) before recording the
 * join-point frame - mirroring the identical fix already applied to
 * plain try/catch via try_exit_state/catch_exit_state.
 *
 * Confirmed against gumdrop's own MailboxIdFile.load(Path), whose shape
 * this matches exactly: a local declared before, assigned only inside,
 * a try-with-resources block, then read afterward.
 */
public class TwrNormalPathLocalVerifyTest {
    static String load(String content) throws IOException {
        String line;
        try (BufferedReader reader = new BufferedReader(new StringReader(content))) {
            line = reader.readLine();
        }
        if (line != null) {
            line = line.trim();
        }
        return line;
    }

    public static void main(String[] args) throws IOException {
        String r = load("  hello  \nworld");
        if (!"hello".equals(r)) {
            throw new RuntimeException("expected hello, got " + r);
        }
        System.out.println("TwrNormalPathLocalVerifyTest passed!");
    }
}
