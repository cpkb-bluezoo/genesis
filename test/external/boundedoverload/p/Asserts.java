package p;

public class Asserts {
	public interface Delegate {}
	public interface Provider<T> { T assertion(); }

	public static class ObjectAssert<T> {
		public String describe() { return "object"; }
		public ObjectAssert<T> same() { return this; }
	}
	public static class ThrowableAssert<T> {
		public String describe() { return "throwable"; }
	}
	public static class ComparableAssert<T> {
		public String describe() { return "comparable"; }
	}

	public static <T> ObjectAssert<T> that(T t) { return new ObjectAssert<T>(); }
	public static <T extends Throwable> ThrowableAssert<T> that(T t) { return new ThrowableAssert<T>(); }
	public static <T extends Comparable<? super T>> ComparableAssert<T> that(T t) { return new ComparableAssert<T>(); }
	public static <T extends Delegate> T that(T t) { return t; }
	public static <T> T that(Provider<T> p) { return p.assertion(); }
}

enum Level { LOW, HIGH, MAX }
