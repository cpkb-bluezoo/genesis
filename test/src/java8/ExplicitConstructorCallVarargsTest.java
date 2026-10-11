import java.util.function.Supplier;

public class ExplicitConstructorCallVarargsTest {
	static class Base {
		final String description;

		protected Base(String first, Class<?>... more) {
			this.description = first + ":" + more.length;
		}

		protected Base(int number, Supplier<String> supplier, String... words) {
			this.description = number + ":" + supplier.get() + ":" + words.length;
		}
	}

	static class ViaSuper extends Base {
		ViaSuper() {
			super("none");
		}

		ViaSuper(Class<?> one) {
			super("one", one);
		}

		ViaSuper(Class<?> one, Class<?> two) {
			super("two", one, two);
		}

		ViaSuper(Class<?>[] array) {
			super("array", array);
		}

		ViaSuper(int number) {
			super(number, () -> "supplied");
		}

		ViaSuper(int number, String a, String b, String c) {
			super(number, String::new, a, b, c);
		}
	}

	static class ViaThis extends Base {
		ViaThis() {
			this(1, () -> "x");
		}

		ViaThis(int number, Supplier<String> supplier, String... words) {
			super(number, supplier, words);
		}

		ViaThis(Supplier<String> supplier) {
			this(2, supplier, "a", "b");
		}
	}

	public static void main(String[] args) {
		check(new ViaSuper().description, "none:0");
		check(new ViaSuper(String.class).description, "one:1");
		check(new ViaSuper(String.class, Integer.class).description, "two:2");
		check(new ViaSuper(new Class<?>[] { String.class, Integer.class, Long.class }).description, "array:3");
		check(new ViaSuper(7).description, "7:supplied:0");
		check(new ViaSuper(8, "a", "b", "c").description, "8::3");
		check(new ViaThis().description, "1:x:0");
		check(new ViaThis(() -> "y").description, "2:y:2");
		System.out.println("PASS");
	}

	static void check(String actual, String expected) {
		if (!expected.equals(actual)) {
			throw new AssertionError("expected " + expected + " but got " + actual);
		}
	}
}
