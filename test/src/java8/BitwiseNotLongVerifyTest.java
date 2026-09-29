/**
 * Bug: unary bitwise-not (`~`) on a `long` operand emitted `iconst_m1` +
 * `ixor` unconditionally (int semantics), corrupting the long value's
 * own type on the verifier's operand stack: "VerifyError: Bad type on
 * operand stack ... long_2nd ... is not assignable to integer". Every
 * OTHER bitwise operator (`&`, `|`, `^`, shifts) already special-cased
 * `long` vs `int` - this unary operator, having its own separate codegen
 * path, was simply missing the same check. Matches gumdrop's own
 * PacketNumberCodec.decode()'s "expected & ~pnMask", where pnMask is
 * declared long.
 */
public class BitwiseNotLongVerifyTest {
    static long decode(long expected, long mask) {
        return (expected & ~mask) | 7L;
    }

    public static void main(String[] args) {
        long r = decode(0xFFL, 0x0FL);
        if (r != 0xF7L) {
            throw new RuntimeException("expected 0xF7, got " + Long.toHexString(r));
        }
        int i = ~5;
        if (i != -6) {
            throw new RuntimeException("expected -6, got " + i);
        }
    }
}
