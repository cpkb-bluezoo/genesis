import java.util.function.Function;
import java.util.function.Supplier;

public class SiblingNestedTypeArgumentsTest {
	/* "Source<T>" is named from inside the sibling class Mapper, before Source is declared */
	static class Mapper {
		<T> Source<T> from(Supplier<T> supplier) {
			return new Source<T>(supplier.get());
		}
	}

	static class Source<T> {
		private final T value;

		Source(T value) {
			this.value = value;
		}

		<R> R toInstance(Function<T, R> function) {
			return function.apply(this.value);
		}
	}

	public static void main(String[] args) {
		Mapper mapper = new Mapper();
		Supplier<String> number = () -> "5";
		Long viaVariable = mapper.from(number).toInstance(Long::valueOf);
		Integer viaLambda = mapper.from(() -> "abc").toInstance((text) -> text.length());
		if (viaVariable != 5L || viaLambda != 3) {
			throw new AssertionError(viaVariable + " " + viaLambda);
		}
		System.out.println("PASS");
	}
}
