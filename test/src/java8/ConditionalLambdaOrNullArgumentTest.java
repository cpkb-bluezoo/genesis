import java.util.function.Function;

public class ConditionalLambdaOrNullArgumentTest {
	static String apply(String value, Function<String, String> adapter) {
		return adapter != null ? adapter.apply(value) : value;
	}

	static <T> String generic(T value, Function<T, String> adapter) {
		return adapter != null ? adapter.apply(value) : "none";
	}

	public static void main(String[] args) {
		boolean on = args.length == 0;
		boolean off = !on;
		String a = apply("a", on ? (key) -> key.toUpperCase() : null);
		String b = apply("b", off ? (key) -> key.toUpperCase() : null);
		String c = apply("c", (on) ? null : (key) -> key.toUpperCase());
		String d = generic("dd", on ? (key) -> "len" + key.length() : null);
		if (!"A".equals(a) || !"b".equals(b) || !"c".equals(c) || !"len2".equals(d)) {
			throw new AssertionError(a + b + c + d);
		}
		System.out.println("PASS");
	}
}
