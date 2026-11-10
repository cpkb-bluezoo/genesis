/**
 * A record whose components include a long, a double or a float gets a
 * generated equals() that must verify and compare each component.
 */
public class RecordWideComponentEqualsTest {
    record M(int i, long l) {}
    record D(double d, String s) {}
    record F(float f, long l, double d, boolean b, char c) {}

    static void check(boolean cond, String what) {
        if (!cond) {
            throw new AssertionError(what);
        }
    }

    public static void main(String[] args) {
        check(new M(1, 5L).equals(new M(1, 5L)), "M equal");
        check(!new M(1, 5L).equals(new M(1, 6L)), "M long differs");
        check(!new M(2, 5L).equals(new M(1, 5L)), "M int differs");
        check(new D(1.5, "x").equals(new D(1.5, "x")), "D equal");
        check(!new D(1.5, "x").equals(new D(2.5, "x")), "D double differs");
        check(!new D(1.5, "x").equals(new D(1.5, "y")), "D string differs");
        check(new F(1f, 2L, 3d, true, 'c').equals(new F(1f, 2L, 3d, true, 'c')), "F equal");
        check(!new F(1f, 2L, 3d, true, 'c').equals(new F(1f, 2L, 4d, true, 'c')), "F double differs");
        check(!new F(1f, 2L, 3d, true, 'c').equals(new F(1f, 3L, 3d, true, 'c')), "F long differs");
        check(!new F(1f, 2L, 3d, true, 'c').equals(new F(2f, 2L, 3d, true, 'c')), "F float differs");
        check(!new F(1f, 2L, 3d, true, 'c').equals(new F(1f, 2L, 3d, false, 'c')), "F boolean differs");
        System.out.println("RecordWideComponentEqualsTest passed!");
    }
}
