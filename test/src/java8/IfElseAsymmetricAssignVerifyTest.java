/*
 * A local declared (unassigned) before an if/else, assigned inside only
 * the ELSE branch (the THEN branch never touches it), had its join-point
 * stackmap frame recorded from whichever branch mg->stackmap's ambient
 * state happened to reflect after codegen (the else branch's, since it
 * runs last) instead of a real per-local LUB merge of both edges -
 * AST_IF_STMT's own join-point handling (codegen_stmt.c) explicitly kept
 * "only restore the slot allocation, not the stackmap types" whenever
 * neither branch terminates, on the (documented, but incomplete)
 * assumption that a variable declared before the if is either untouched
 * by both branches or assigned by both - never assigned by only one.
 * When only one branch assigns it, the correct join type is "top" (JVMS
 * 4.10.1.4: disagreeing types merge to unusable), not the assigning
 * branch's own type - the then-branch's real edge (reached via its own
 * "goto past else") still has it as top.
 *
 * Confirmed against gumdrop's own DotStuffer.processChunk() (org.bluezoo.
 * gumdrop.smtp.client): a "case SAW_CR:" whose then-branch only updates
 * `state` while its else-branch also assigns `currentPos`/`saveLimit`
 * (declared, unset, before the enclosing while-loop).
 */
public class IfElseAsymmetricAssignVerifyTest {
    static int run(int[] buf) {
        int currentPos;
        int i = 0;
        while (i < buf.length) {
            int b = buf[i];
            if (b == 1) {
                // then-branch never touches currentPos
                b = b + 1;
            } else {
                currentPos = i;
                b = currentPos;
            }
            i++;
        }
        return i;
    }

    public static void main(String[] args) {
        int r = run(new int[] { 1, 2, 1, 3 });
        if (r != 4) {
            throw new RuntimeException("expected 4, got " + r);
        }
        System.out.println("IfElseAsymmetricAssignVerifyTest passed!");
    }
}
