public class NestedQualifiedStaticFieldTest {
    public static void main(String[] args) {
        int n = Outer.Inner.VALUE;
        if (n != 42) throw new AssertionError("value");
        if (!Outer.Inner.NAME.equals("inner")) throw new AssertionError("name");
        Outer.Inner.counter = 3;
        Outer.Inner.counter += 2;
        if (Outer.Inner.counter != 5) throw new AssertionError("counter");
        if (Outer.Inner.Deep.FLAG != 'x') throw new AssertionError("deep");
        System.out.println("ok");
    }
}

class Outer {
    static class Inner {
        static final int VALUE = 42;
        static final String NAME = "inner";
        static int counter;
        static class Deep { static final char FLAG = 'x'; }
    }
}
