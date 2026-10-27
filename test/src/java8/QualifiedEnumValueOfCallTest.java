import java.util.function.Function;

public class QualifiedEnumValueOfCallTest {
    enum Level { LOW, HIGH }

    static class Inner {
        static final int LIMIT = 7;
        static int twice(int n) { return n * 2; }
    }

    // A qualified name for a nested type ("Outer.Level") is as good as the simple one.
    public static void main(String[] args) {
        if (QualifiedEnumValueOfCallTest.Level.valueOf("HIGH") != Level.HIGH) throw new AssertionError();
        if (QualifiedEnumValueOfCallTest.Level.values().length != 2) throw new AssertionError();
        Function<String, Level> f = s -> QualifiedEnumValueOfCallTest.Level.valueOf(s);
        if (f.apply("LOW") != Level.LOW) throw new AssertionError();
        // ... and so is a nested class named through the class that declares it
        if (QualifiedEnumValueOfCallTest.Inner.twice(2) != 4) throw new AssertionError();
        if (QualifiedEnumValueOfCallTest.Inner.LIMIT != 7) throw new AssertionError();
        System.out.println("ok");
    }
}
