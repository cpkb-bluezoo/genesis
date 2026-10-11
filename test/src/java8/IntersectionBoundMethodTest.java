/*
 * Regression test: a method of the second bound of "T extends Number & Comparable<T>"
 * ("a.compareTo(b)") was looked up in the first bound only and compiled as a call of
 * Number.compareTo (VerifyError or NoSuchMethodError).
 */
import java.util.function.*;
public class IntersectionBoundMethodTest {
	static <T extends Number & Comparable<T>> boolean pos(String v, Function<String, T> parse) {
		T zero = parse.apply("0");
		T min = parse.apply(v);
		return min.compareTo(zero) > 0;
	}
	static <T extends Number & Comparable<T>> int cmp(T a, T b) { return a.compareTo(b) < 0 ? -1 : a.intValue(); }
	public static void main(String[] a) { String actual = pos("5", Integer::valueOf) + " " + pos("-1", Long::valueOf) + " " + cmp(1, 2) + cmp(3.5, 1.5);
		if (!"true false -13".equals(actual)) {
			throw new RuntimeException("FAIL: " + actual);
		}
		System.out.println("IntersectionBoundMethodTest passed!"); }
}
