import java.util.function.Function;

public class LambdaBodyReturnsLambdaTest {
	interface Strategy {
		boolean upgrade(Object session, Object host, Object attachment);

		default void upgrade(Object session) {
		}
	}

	interface Plain {
		boolean test(Object left, Object right);
	}

	public static void main(String[] args) {
		StringBuilder seen = new StringBuilder();
		Function<String, Plain> plain = (name) -> {
			seen.append(name);
			return (left, right) -> left == right;
		};
		Function<String, Strategy> strategy = (name) -> {
			seen.append(name.length());
			return (session, host, attachment) -> attachment != null;
		};
		Object marker = new Object();
		if (!plain.apply("p").test(marker, marker) || !strategy.apply("abc").upgrade(null, null, marker)
				|| !"p3".equals(seen.toString())) {
			throw new AssertionError(seen.toString());
		}
		System.out.println("PASS");
	}
}
