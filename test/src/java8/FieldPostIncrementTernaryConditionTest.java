/* Regression test: an instance field post-incremented inside the condition of
 * a conditional that is an argument of a call whose receiver is already on the
 * stack ("sb.append(count++ == 0 ? "[" : ",")"). The stack-map frame at the
 * conditional's branch target carried a different stack depth than the code
 * at the jump ("Inconsistent stackmap frames ... stack size doesn't match").
 * Mirrors gumdrop's QlogFrames.open. */
public class FieldPostIncrementTernaryConditionTest {
    private final StringBuilder sb = new StringBuilder();
    private int count;
    private long total;
    private static int shared;
    private double dc;
    private float fc;
    private final int[] ia = new int[3];
    private final long[] la = new long[3];
    private final byte[] ba = new byte[3];
    private final double[] da = new double[3];

    private StringBuilder open(String type) {
        sb.append(count++ == 0 ? "[" : ",");
        sb.append("{").append(type).append('}');
        return sb;
    }

    private String sep() {
        return String.valueOf(total++ == 0L ? "first" : "next") + (shared++ > 0 ? "+" : "-");
    }

    /** Every kind of ++/-- operand, as a conditional's condition under a pending receiver. */
    private String matrix() {
        StringBuilder m = new StringBuilder();
        m.append(count++ == 0 ? "a" : "b");
        m.append(++count == 3 ? "c" : "d");
        m.append(total++ == 0L ? "e" : "f");
        m.append(--total == 0L ? "g" : "h");
        m.append(dc++ == 0.0 ? "i" : "j");
        m.append(fc++ == 0f ? "k" : "l");
        m.append(ia[1]++ == 0 ? "m" : "n");
        m.append(++ia[1] == 2 ? "o" : "p");
        m.append(la[2]++ == 0L ? "q" : "r");
        m.append(--la[2] == 0L ? "s" : "t");
        m.append(ba[0]++ == 0 ? "u" : "v");
        m.append(da[0]++ == 0.0 ? "w" : "x");
        m.append(this.count-- == 3 ? "y" : "z");
        int i = 0;
        m.append(i++ == 0 ? "1" : "2");
        m.append(++i == 2 ? "3" : "4");
        long l = 0;
        double d = 0;
        float f = 0;
        m.append(l++ == 0L ? "6" : "7");
        m.append(++d == 1.0 ? "8" : "9");
        m.append(f++ == 0f ? "A" : "B");
        m.append(shared++ == 0 ? "5" : "6");
        return m.toString();
    }

    public static void main(String[] args) {
        FieldPostIncrementTernaryConditionTest t = new FieldPostIncrementTernaryConditionTest();
        t.open("a");
        t.open("b");
        if (!"[{a},{b}".equals(t.sb.toString())) {
            throw new RuntimeException(t.sb.toString());
        }
        if (t.count != 2) {
            throw new RuntimeException("count " + t.count);
        }
        String s1 = t.sep();
        String s2 = t.sep();
        if (!"first-".equals(s1) || !"next+".equals(s2)) {
            throw new RuntimeException(s1 + " " + s2);
        }
        FieldPostIncrementTernaryConditionTest u = new FieldPostIncrementTernaryConditionTest();
        shared = 0;
        String got = u.matrix();
        if (!"adegikmoqsuwz1368A5".equals(got)) {
            throw new RuntimeException(got);
        }
        System.out.println("FieldPostIncrementTernaryConditionTest passed!");
    }
}
