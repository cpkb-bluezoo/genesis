import java.util.function.Function;
import java.util.function.Supplier;

public class GenericNestedClassAsSupplierArgumentTest {
	static class Mapper {
		<T> Source<T> from(Supplier<T> supplier) {
			return new Source<T>(supplier.get());
		}

		<T> Source<T> from(T value) {
			return new Source<T>(value);
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

	static class Count<T> implements Supplier<T> {
		private final Supplier<T> source;

		private int count;

		Count(Supplier<T> source) {
			this.source = source;
		}

		public T get() {
			this.count++;
			return this.source.get();
		}
	}

	public static void main(String[] args) {
		Mapper map = new Mapper();
		Count<String> source = new Count<>(() -> "123");
		Long result = map.from(source).toInstance(Long::valueOf);
		if (result != 123L || source.count != 1) {
			throw new AssertionError(result + " " + source.count);
		}
		System.out.println("PASS");
	}
}
