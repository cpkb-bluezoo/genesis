/**
 * Bug: a `switch` statement whose FIRST case ends in `return`, but whose
 * PHYSICALLY LAST case (reached via an earlier case's intentional
 * fall-through, no `break`) ends in a plain assignment and falls off the
 * end of the switch, could compile as a void method with no trailing
 * `return` bytecode at all - "VerifyError: Control flow falls through
 * code end". Matches gumdrop's own
 * HttpProtocolHandler.processRequestLine(): its first case
 * (HttpVersion.UNKNOWN) ends in `return;`, while its last case (the
 * `default:` block, reached directly or via HTTP_1_0's intentional
 * fall-through) ends with a plain field assignment and no `return`.
 * Root cause: the switch-statement case-body loop (codegen_stmt.c) never
 * reset `mg->last_opcode` before generating each case's own statements
 * (unlike if/while/for/synchronized bodies elsewhere in the same file,
 * which all reset it before their own nested body). A case whose body
 * doesn't itself touch `mg->last_opcode` (an ordinary assignment or call
 * statement doesn't) left it exactly as an EARLIER, unrelated case had
 * set it. For the LAST case, that stale value fed directly into the
 * switch's own "does this switch always terminate" computation
 * (`switch_terminates`), which fed into the enclosing void method's own
 * "does the method body already end in return/throw" check - so a
 * borrowed `return` from a completely different, earlier case suppressed
 * the synthetic trailing `return` the method actually needed.
 */
public class SwitchStaleLastOpcodeAcrossCasesVerifyTest {
    enum Kind { A, B, C }

    boolean flag;

    void process(Kind k) {
        switch (k) {
            case A:
                return;
            case B:
                if (flag) {
                    flag = false;
                } else {
                    flag = true;
                }
                return;
            case C:
                flag = true;
                // Intentional fall-through, no break.
            default:
                flag = false;
        }
    }

    public static void main(String[] args) {
        SwitchStaleLastOpcodeAcrossCasesVerifyTest t = new SwitchStaleLastOpcodeAcrossCasesVerifyTest();
        t.flag = true;
        t.process(Kind.C);
        if (t.flag) {
            throw new RuntimeException("expected flag=false after falling through into default");
        }
    }
}
