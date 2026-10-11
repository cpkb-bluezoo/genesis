import java.util.function.Supplier;

public class FunctionalInterfaceWithDefaultOverloadTest {
	interface Factory<T, R> {
		R create(T value);

		default R create(Supplier<? extends T> provider) {
			return create(provider.get());
		}
	}

	static <T, R> R use(T value, Factory<T, R> factory) {
		return factory.create(value);
	}

	public static void main(String[] args) {
		Factory<String, Integer> length = (value) -> value.length();
		Integer viaDefault = length.create(() -> "four");
		Integer direct = use("abc", (text) -> text.length() * 2);
		if (length.create("xy") != 2 || viaDefault != 4 || direct != 6) {
			throw new AssertionError("wrong");
		}
		System.out.println("PASS");
	}
}
