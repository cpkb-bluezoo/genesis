/**
 * Bug: a `switch` case that declares a local and falls through (no
 * `break`) into the next case corrupted the STORED StackMapTable frame
 * at that NEXT case's own label position. Every case label (default
 * included) is always a direct lookupswitch/tableswitch dispatch
 * target, so it is reached by TWO edges at once whenever the previous
 * case falls through: the lookupswitch's own direct entry (arriving
 * with exactly the switch's entry-state locals) and the fallthrough
 * edge (arriving with whatever extra locals the previous case's body
 * just assigned). The single frame recorded at that one bytecode offset
 * must be valid for both edges, but genesis only ever recorded the
 * fallthrough edge's state, wrongly claiming a fallthrough-only local
 * as definitely assigned even along the direct-dispatch edge:
 * "VerifyError: Inconsistent stackmap frames ... Type top ... is not
 * assignable to '<type>'" the moment the lookupswitch dispatched
 * directly into that case label without going through the fallthrough.
 * Matches gumdrop's own DeploymentDescriptorParser.endElement(), whose
 * `case HANDLER:` declares `HandlerDef handlerDef` and falls through
 * (no break) into `case MAPPED_NAME:`.
 */
public class SwitchFallthroughCaseLabelFrameMergeVerifyTest {
    static class Holder {
        Object value;
    }

    static void dispatch(int state, Holder h) {
        switch (state) {
            case 1:
                String local = "from-case-1";
                h.value = local;
            case 2:
                h.value = "case-2";
                break;
            case 3:
                h.value = "case-3";
                break;
            case 4:
                h.value = "case-4";
                break;
        }
    }

    public static void main(String[] args) {
        Holder h = new Holder();
        dispatch(3, h);
        if (!"case-3".equals(h.value)) {
            throw new RuntimeException("expected case-3, got " + h.value);
        }
        dispatch(2, h);
        if (!"case-2".equals(h.value)) {
            throw new RuntimeException("expected case-2, got " + h.value);
        }
        dispatch(1, h);
        if (!"case-2".equals(h.value)) {
            throw new RuntimeException("expected case-2 (via fallthrough), got " + h.value);
        }
    }
}
