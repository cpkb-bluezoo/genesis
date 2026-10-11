package p;

import java.util.EnumSet;

import p.Asserts.Delegate;

/**
 * Overloads that differ only in the bound of their type variable, declared
 * in another file of the same compilation.
 */
public class BoundedOverloadTest {
	static class Plain {}
	static class Failure extends RuntimeException {}
	static class Dele implements Delegate {}

	public static void main(String[] args) {
		check("object", Asserts.that(new Plain()).same().describe());
		check("throwable", Asserts.that(new Failure()).describe());
		check("comparable", Asserts.that("text").describe());
		// "E extends Enum<E>" accepts an enum declared in another file
		EnumSet<Level> levels = EnumSet.of(Level.LOW, Level.HIGH, Level.MAX);
		check("3", String.valueOf(levels.size()));
		Dele d = Asserts.that(new Dele());
		if (d == null) {
			throw new AssertionError("delegate");
		}
		System.out.println("PASS");
	}

	static void check(String expected, String actual) {
		if (!expected.equals(actual)) {
			throw new AssertionError("expected " + expected + " but got " + actual);
		}
	}
}
