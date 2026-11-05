/*
 * A non-final static field is read, assigned and updated through a subclass of
 * the class that declares it (JLS 8.3.1.1, 15.11.1): "Child.MODE" names the
 * field Holder declares.
 */
public class StaticFieldThroughSubclassTest {
    static class Holder {
        static int MODE = 3;
        static String NAME;
        static long TOTAL;
        static boolean ON;
    }

    static class Child extends Holder {
    }

    static class GrandChild extends Child {
    }

    public static void main(String[] args) {
        if (Child.MODE != 3) {
            throw new AssertionError(Child.MODE);
        }
        Child.MODE = 4;
        Child.NAME = "x";
        Child.MODE += 2;
        Child.TOTAL = 10L;
        Child.TOTAL *= 3;
        Child.ON = !Child.ON;
        if (Holder.MODE != 6 || !Holder.NAME.equals("x") || Holder.TOTAL != 30L || !Holder.ON) {
            throw new AssertionError(Holder.MODE + " " + Holder.NAME + " " + Holder.TOTAL);
        }
        GrandChild.MODE = GrandChild.MODE + 1;
        String s = GrandChild.NAME + GrandChild.MODE;
        if (!s.equals("x7")) {
            throw new AssertionError(s);
        }
        System.out.println("StaticFieldThroughSubclassTest passed!");
    }
}
