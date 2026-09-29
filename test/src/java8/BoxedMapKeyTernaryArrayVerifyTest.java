/**
 * Bug: autoboxing a primitive argument (e.g. an int key passed to
 * TreeMap<Integer,...>.put()) left genesis's OWN internal stackmap
 * tracking (mg->stackmap) recording the PRE-boxing primitive type for
 * that stack slot, even though the actual bytecode - an invokestatic to
 * Integer.valueOf(I)Ljava/lang/Integer; - replaced it with a reference
 * on the real operand stack. If a LATER argument to the same call
 * contains its own branch (e.g. a ternary building an int[] element),
 * the branch target's recorded StackMapTable frame is computed from the
 * still-stale (primitive) tracked type for the boxed key sitting deeper
 * on the stack, while the real runtime stack has an Integer reference
 * there: "Inconsistent stackmap frames ... Type 'java/lang/Integer'
 * (current frame, stack[N]) is not assignable to integer (stack map,
 * stack[N])". Matches gumdrop's own
 * ContentTypeParser.processRawParamsFromSlices(), which does
 * ranges.put(index, new int[] { r.valueStart, r.valueEnd, r.quoted ? 1
 * : 0 }) on a TreeMap<Integer, int[]>.
 * Root cause: emit_boxing() (codegen_expr.c) emits the Integer.valueOf
 * invokestatic but never corrects mg->stackmap's tracked type for that
 * slot to the wrapper class - unlike its counterpart emit_unboxing(),
 * which already does this correctly.
 */
public class BoxedMapKeyTernaryArrayVerifyTest {
    static java.util.TreeMap<Integer, int[]> map = new java.util.TreeMap<>();

    static void put(int index, boolean flag) {
        map.put(index, new int[] { 0, 1, flag ? 1 : 0 });
    }

    public static void main(String[] args) {
        put(3, true);
        int[] v = map.get(3);
        if (v[2] != 1) {
            throw new RuntimeException("expected v[2]=1, got " + v[2]);
        }
        put(4, false);
        v = map.get(4);
        if (v[2] != 0) {
            throw new RuntimeException("expected v[2]=0, got " + v[2]);
        }
    }
}
