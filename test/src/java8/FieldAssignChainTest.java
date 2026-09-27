/*
 * An assignment expression's value is the value assigned (JLS 15.26), usable
 * as a sub-expression, for every kind of assignment target -- including a
 * field reached through an explicit receiver ("obj.field = x" or
 * "Test.sfield = x"), which is also the only place compound assignment on a
 * static field named by its class ("Test.sfield += x") is exercised at all.
 */
public class FieldAssignChainTest {

    static int sfield;
    int ifield;
    long lfield;

    static void check(boolean ok, String what) {
        if (!ok) {
            System.out.println("FAILED: " + what);
            System.exit(1);
        }
    }

    public static void main(String[] args) {
        FieldAssignChainTest t = new FieldAssignChainTest();

        int r1 = (t.ifield = 5);
        check(r1 == 5 && t.ifield == 5, "chained instance field simple assignment");

        int r2 = (t.ifield += 3);
        check(r2 == 8 && t.ifield == 8, "chained instance field compound assignment");

        long r3 = (t.lfield = 5);
        check(r3 == 5L && t.lfield == 5L, "chained wide instance field simple assignment");

        long r4 = (t.lfield += 3);
        check(r4 == 8L && t.lfield == 8L, "chained wide instance field compound assignment");

        int r5 = (FieldAssignChainTest.sfield = 5);
        check(r5 == 5 && sfield == 5, "chained static field (by class name) simple assignment");

        FieldAssignChainTest.sfield += 3;
        check(sfield == 8, "static field (by class name) compound assignment");

        int r6 = (FieldAssignChainTest.sfield += 2);
        check(r6 == 10 && sfield == 10, "chained static field (by class name) compound assignment");

        System.out.println("FieldAssignChainTest passed!");
    }
}
