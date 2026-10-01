/**
 * Bug: a hexadecimal (or binary) `long` literal whose bit pattern has the
 * sign bit set (e.g. "0xc000000000000000L") was silently parsed as
 * Long.MAX_VALUE (0x7fffffffffffffffL) instead of the actual bit pattern
 * requested. Per JLS 3.10.1, a hex/octal/binary integer literal names a bit
 * pattern directly (the full unsigned range of its type is allowed), unlike
 * a decimal literal (whose allowed range is the signed type's own range).
 * The lexer parsed hex/binary literals with strtoll() (a SIGNED parse),
 * which saturates to LLONG_MAX for exactly this shape, with no error or
 * warning of any kind. Confirmed against gumdrop's own VarInt.encode()'s
 * "value | 0xc000000000000000L" (RFC 9000's varint length-prefix mask for
 * its 8-byte encoding), which silently corrupted every value requiring
 * that encoding.
 */
public class HexLongLiteralSignBitVerifyTest {
    public static void main(String[] args) {
        long mask = 0xc000000000000000L;
        if (mask != -4611686018427387904L) {
            throw new RuntimeException("expected 0xc000000000000000L == -4611686018427387904, got " + mask);
        }

        long allOnes = 0xffffffffffffffffL;
        if (allOnes != -1L) {
            throw new RuntimeException("expected 0xffffffffffffffffL == -1, got " + allOnes);
        }

        long minValue = 0x8000000000000000L;
        if (minValue != Long.MIN_VALUE) {
            throw new RuntimeException("expected 0x8000000000000000L == Long.MIN_VALUE, got " + minValue);
        }

        /* Binary literals hit the exact same lexer code path as hex. */
        long binMask = 0b1100000000000000000000000000000000000000000000000000000000000000L;
        if (binMask != mask) {
            throw new RuntimeException("expected the binary form of the mask to equal the hex form, got " + binMask);
        }

        long value = 1073741824L;
        long result = value | mask;
        if (result != -4611686017353646080L) {
            throw new RuntimeException("expected (1073741824L | 0xc000000000000000L) == -4611686017353646080, got " + result);
        }
    }
}
