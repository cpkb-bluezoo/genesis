public class BoundedTypeVariableOverloadTest {
	interface Delegate {}
	interface Provider<T> { T assertion(); }

	static class ObjectAssert<T> {
		String describe() { return "object"; }
		ObjectAssert<T> same() { return this; }
	}
	static class ThrowableAssert<T> {
		String describe() { return "throwable"; }
	}
	static class ComparableAssert<T> {
		String describe() { return "comparable"; }
	}

	static <T> ObjectAssert<T> that(T t) { return new ObjectAssert<T>(); }
	static <T extends Throwable> ThrowableAssert<T> that(T t) { return new ThrowableAssert<T>(); }
	static <T extends Comparable<? super T>> ComparableAssert<T> that(T t) { return new ComparableAssert<T>(); }
	static <T extends Delegate> T that(T t) { return t; }
	static <T> T that(Provider<T> p) { return p.assertion(); }

	static class Plain {}
	static class Failure extends RuntimeException {}
	static class Dele implements Delegate {}

	public static void main(String[] args) {
		check("object", that(new Plain()).same().describe());
		check("throwable", that(new Failure()).describe());
		check("comparable", that("text").describe());
		check("comparable", that(5).describe());
		Dele d = that(new Dele());
		check("object", that((Object) new Plain()).describe());
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
