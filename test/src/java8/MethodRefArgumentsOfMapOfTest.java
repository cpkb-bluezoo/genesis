import java.util.Map;
import java.util.function.Predicate;
import java.util.function.Supplier;

public class MethodRefArgumentsOfMapOfTest {
	private final Map<String, Supplier<String>> factories;

	private final Predicate<String>[] filters;

	static class Loader {
		final Predicate<String>[] filters;

		@SafeVarargs
		Loader(Predicate<String>... filters) {
			this.filters = filters;
		}
	}

	@SafeVarargs
	static Predicate<String>[] filters(Predicate<String>... filters) {
		return filters;
	}

	MethodRefArgumentsOfMapOfTest() {
		this.factories = Map.of("one", this::one, "two", this::two, "three", this::three);
		this.filters = filters(("META-INF/" + "services")::equals, "other"::equals);
		Loader loader = new Loader(("META-INF/" + Object.class.getName())::equals);
		if (!loader.filters[0].test("META-INF/java.lang.Object")) {
			throw new AssertionError("loader");
		}
	}

	String one() {
		return "1";
	}

	String two() {
		return "2";
	}

	String three() {
		return "3";
	}

	public static void main(String[] args) {
		MethodRefArgumentsOfMapOfTest test = new MethodRefArgumentsOfMapOfTest();
		String joined = test.factories.get("one").get() + test.factories.get("two").get()
				+ test.factories.get("three").get();
		if (!"123".equals(joined) || !test.filters[0].test("META-INF/services") || test.filters[0].test("x")
				|| !test.filters[1].test("other")) {
			throw new AssertionError(joined);
		}
		System.out.println("PASS");
	}
}
