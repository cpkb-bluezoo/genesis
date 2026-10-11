/*
 * Regression test: an anonymous class had no Signature attribute, so what it extends or implements
 * ("new Ref<List<String>>() { }") read back through reflection as the raw class (Spring's
 * ParameterizedTypeReference: "Type must be a parameterized type").
 */
import java.lang.reflect.*;
import java.util.*;
public class AnonymousClassSignatureTest {
	static abstract class Ref<T> { final Type type; Ref() { type = ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[0]; } }
	public static void main(String[] a) {
		String actual = new Ref<String>() {}.type + " " + new Ref<List<String>>() {}.type;
		Comparable<String> c = new Comparable<String>() { public int compareTo(String s) { return 0; } };
		actual += " " + Arrays.toString(c.getClass().getGenericInterfaces());
		if (!"class java.lang.String java.util.List<java.lang.String> [java.lang.Comparable<java.lang.String>]".equals(actual)) {
			throw new RuntimeException("FAIL: " + actual);
		}
		System.out.println("AnonymousClassSignatureTest passed!");
	}
}
