/*
 * Regression test: casts that box or unbox. "(Long) value" of a long primitive is a boxing
 * conversion, not a checkcast (VerifyError); "(long) anInteger" unboxes the Integer it has and widens, not
 * a cast of it to Long (ClassCastException).
 */
public class BoxingCastTest {
	static String put(int i, long v) { return put(i, (Long) v); }
	static String put(int i, Object v) { return i + ":" + v; }
	static Object a(int x) { return (Integer) x; }
	static Object b(double d) { return (Double) d; }
	static Number c(short s) { return (Short) s; }
	static Object d(char ch) { return (Character) ch; }
	static long e(Integer boxed) { return (long) boxed; }
	static int f(Object o) { return (int) o; }
	static int g(Long boxed) { return (int) (long) boxed; }
	public static void main(String[] args) { String actual = "" + put(1, 5L) + a(2) + b(3.5) + c((short) 4) + d('x') + e(7) + f(8) + g(9L);
		if (!"1:523.54x789".equals(actual)) {
			throw new RuntimeException("FAIL: " + actual);
		}
		System.out.println("BoxingCastTest passed!"); }
}
