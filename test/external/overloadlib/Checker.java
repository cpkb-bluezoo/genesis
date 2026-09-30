package overloadlib;

/*
 * Mirrors the overload shape of org.junit.Assert.assertEquals (compiled
 * to its own classfile first - see run-tests.sh - so PrimitiveOverloadResolutionTest.java
 * resolves these calls against classfile-loaded method symbols, not
 * same-compilation-unit ones): a reference-typed catch-all alongside
 * primitive-typed overloads of different widths.
 */
public class Checker {
    public static String lastCalled;

    public static void check(String msg, Object expected, Object actual) {
        lastCalled = "Object";
    }

    public static void check(String msg, double expected, double actual) {
        lastCalled = "double";
    }

    public static void check(String msg, long expected, long actual) {
        lastCalled = "long";
    }
}
