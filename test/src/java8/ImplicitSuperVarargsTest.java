/*
 * Regression test: a constructor with no explicit super(), a class with no constructor at all, and a plain
 * "super()" all call a superclass constructor declared Base(String... names) with the empty array; they
 * called a Base() that does not exist (NoSuchMethodError); so did an anonymous subclass.
 */
public class ImplicitSuperVarargsTest {
	static abstract class Base { final int n; Base(String... names) { n = names.length; } }
	static class Sub extends Base { Sub() { } }
	static class Sub2 extends Base { }
	static class Sub3 extends Base { Sub3() { super(); } }
	static abstract class Named<T> extends Base { abstract T get(); }
	public static void main(String[] a) { Named<String> anonymous = new Named<>() { String get() { return "x"; } };
		String actual = new Sub().n + "" + new Sub2().n + new Sub3().n + anonymous.n;
		if (!"0000".equals(actual)) {
			throw new RuntimeException("FAIL: " + actual);
		}
		System.out.println("ImplicitSuperVarargsTest passed!"); }
}
