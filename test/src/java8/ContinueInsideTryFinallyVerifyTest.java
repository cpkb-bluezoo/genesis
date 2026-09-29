/*
 * Regression test: a `continue` statement whose target loop is entirely
 * CONTAINED within an enclosing try-finally (the try wraps the whole
 * loop, not the other way around) must NOT run that finally block - only
 * a break/continue/return that actually leaves the try's own scope may
 * run it. Matches gumdrop's own HostsFile.parse():
 *
 *   try {
 *       BufferedReader reader = Files.newBufferedReader(path);
 *       try {
 *           String line;
 *           while ((line = reader.readLine()) != null) {
 *               ...
 *               if (line.isEmpty()) {
 *                   continue;
 *               }
 *               ...
 *           }
 *       } finally {
 *           reader.close();
 *       }
 *   } catch (IOException e) { ... }
 *
 * Root cause: codegen_stmt.c's AST_CONTINUE_STMT case unconditionally
 * called emit_pending_finally_blocks(mg), which ran EVERY finally block
 * currently on mg->finally_stack - including one belonging to a try
 * statement that simply contains the loop being continued, even though
 * continuing that loop never leaves the try's own lexical scope at all.
 * For HostsFile.parse(), this closed the BufferedReader on the very
 * first blank/comment line (the first `continue`), so the next
 * readLine() threw (caught and silently logged by the outer catch),
 * truncating parse() to whatever had been read before that line -
 * silently returning wrong data, no VerifyError or crash.
 *
 * Fixed by recording each loop's own finally_stack depth (its length at
 * the point the loop is entered) on its loop_context_t, and having
 * emit_pending_finally_blocks() stop once it reaches that depth for a
 * break/continue - only finally blocks pushed AFTER the loop was
 * entered (try statements nested INSIDE the loop body) still run; a
 * finally block for a try that wraps the loop itself is left alone,
 * exactly as it would for a `return` from ONLY the code up to (not past)
 * that outer try. `return` itself is unaffected (still runs everything
 * pending, passing stop_depth 0), since it always leaves the whole
 * method.
 */
public class ContinueInsideTryFinallyVerifyTest {

    static int outerCloseCount = 0;
    static int innerCloseCount = 0;

    // Try wraps the whole loop: continue must NOT trigger the finally.
    static java.util.List<String> scenarioA() {
        java.util.List<String> processed = new java.util.ArrayList<String>();
        String[] lines = { "", "  ", "keep1", "#comment", "keep2", "" };
        int[] index = { 0 };
        try {
            while (index[0] < lines.length) {
                String line = lines[index[0]];
                index[0]++;
                String trimmed = line.trim();
                if (trimmed.isEmpty() || trimmed.startsWith("#")) {
                    continue;
                }
                processed.add(trimmed);
            }
        } finally {
            outerCloseCount++;
        }
        return processed;
    }

    // Try is nested INSIDE the loop body: continue exits that inner try
    // each time, so its finally must still run on every iteration.
    static java.util.List<String> scenarioB() {
        java.util.List<String> processed = new java.util.ArrayList<String>();
        String[] lines = { "skip", "keep1", "skip", "keep2" };
        for (int i = 0; i < lines.length; i++) {
            try {
                if (lines[i].equals("skip")) {
                    continue;
                }
                processed.add(lines[i]);
            } finally {
                innerCloseCount++;
            }
        }
        return processed;
    }

    public static void main(String[] args) {
        java.util.List<String> a = scenarioA();
        if (!a.equals(java.util.Arrays.asList("keep1", "keep2"))) {
            throw new RuntimeException("scenarioA: expected [keep1, keep2], got " + a);
        }
        if (outerCloseCount != 1) {
            throw new RuntimeException("scenarioA: expected outer finally to run exactly once, ran "
                    + outerCloseCount + " times");
        }

        java.util.List<String> b = scenarioB();
        if (!b.equals(java.util.Arrays.asList("keep1", "keep2"))) {
            throw new RuntimeException("scenarioB: expected [keep1, keep2], got " + b);
        }
        if (innerCloseCount != 4) {
            throw new RuntimeException("scenarioB: expected inner finally to run once per iteration (4), ran "
                    + innerCloseCount + " times");
        }

        System.out.println("ContinueInsideTryFinallyVerifyTest passed!");
    }
}
