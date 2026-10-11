import java.util.List;
import java.util.function.Predicate;

public class DiamondConstructorLambdaFromReturnTargetTest {
	static class Condition<T> {
		private final Predicate<T> predicate;

		private final String description;

		Condition(Predicate<T> predicate, String description, Object... args) {
			this.predicate = predicate;
			this.description = String.format(description, args);
		}

		boolean matches(T value) {
			return this.predicate.test(value);
		}
	}

	static class Data {
		final String value;

		Data(String value) {
			this.value = value;
		}

		String getValue() {
			return this.value;
		}
	}

	private Condition<Data> sanitized() {
		return new Condition<>((data) -> "***".equals(data.getValue()), "sanitized");
	}

	private Condition<List<? extends Data>> sizes(int expected) {
		return new Condition<>((list) -> list.size() == expected, "size %s", expected);
	}

	private Condition<String> field(String href) {
		Condition<String> c = new Condition<>((link) -> href.equals(link.trim()), "with %s", href);
		return c;
	}

	public static void main(String[] args) {
		DiamondConstructorLambdaFromReturnTargetTest test = new DiamondConstructorLambdaFromReturnTargetTest();
		if (!test.sanitized().matches(new Data("***")) || test.sanitized().matches(new Data("x"))
				|| !test.sizes(1).matches(java.util.Arrays.asList(new Data("a"))) || !test.field("a").matches(" a ")) {
			throw new AssertionError("wrong");
		}
		System.out.println("PASS");
	}
}
