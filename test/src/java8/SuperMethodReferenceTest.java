import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

public class SuperMethodReferenceTest {
	interface Config {
		default String projectId() {
			return "default-project";
		}

		default int size(String text) {
			return text.length();
		}
	}

	static class Base {
		String name() {
			return "base";
		}

		String join(String a, String b) {
			return a + "+" + b;
		}
	}

	static class Adapter extends Base implements Config {
		static <T> T obtain(Supplier<T> primary, Supplier<T> fallback) {
			T value = primary.get();
			return (value != null) ? value : fallback.get();
		}

		@Override
		public String projectId() {
			return obtain(() -> null, Config.super::projectId) + "!";
		}

		@Override
		public int size(String text) {
			Function<String, Integer> sizer = Config.super::size;
			return sizer.apply(text) * 2;
		}

		@Override
		String name() {
			Supplier<String> parent = super::name;
			return parent.get() + "/adapter";
		}

		@Override
		String join(String a, String b) {
			BiFunction<String, String, String> parent = super::join;
			return parent.apply(a, b).toUpperCase();
		}
	}

	public static void main(String[] args) {
		Adapter adapter = new Adapter();
		if (!"default-project!".equals(adapter.projectId()) || adapter.size("abc") != 6
				|| !"base/adapter".equals(adapter.name()) || !"A+B".equals(adapter.join("a", "b"))) {
			throw new AssertionError(adapter.projectId() + " " + adapter.size("abc") + " " + adapter.name());
		}
		System.out.println("PASS");
	}
}
