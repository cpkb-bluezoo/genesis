import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Stream;

public class GenericVarargsStreamChainTest {
	static class Entry {
		String id() {
			return null;
		}

		String name() {
			return "name";
		}
	}

	@SafeVarargs
	private <T, P> P getFirstNonNull(T t, Function<T, P>... extractors) {
		return Stream.of(extractors)
			.map((extractor) -> extractor.apply(t))
			.filter(Objects::nonNull)
			.findFirst()
			.orElse(null);
	}

	public static void main(String[] args) {
		GenericVarargsStreamChainTest test = new GenericVarargsStreamChainTest();
		String first = test.getFirstNonNull(new Entry(), Entry::id, Entry::name);
		if (!"name".equals(first)) {
			throw new AssertionError(first);
		}
		System.out.println("PASS");
	}
}
