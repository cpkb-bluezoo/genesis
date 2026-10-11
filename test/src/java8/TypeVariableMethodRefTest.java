import java.util.function.Function;

public class TypeVariableMethodRefTest {
	static class Props {
		String getName() {
			return "props";
		}

		boolean isEnabled() {
			return true;
		}
	}

	static class Adapter<T extends Props> {
		final T props;

		Adapter(T props) {
			this.props = props;
		}

		<V> V obtain(Function<T, V> getter) {
			return getter.apply(this.props);
		}

		String name() {
			return obtain(T::getName);
		}

		boolean enabled() {
			return obtain(T::isEnabled);
		}

		Function<T, String> asFunction() {
			return T::getName;
		}
	}

	public static void main(String[] args) {
		Adapter<Props> adapter = new Adapter<Props>(new Props());
		if (!"props".equals(adapter.name()) || !adapter.enabled() || !"props".equals(adapter.asFunction().apply(new Props()))) {
			throw new AssertionError("wrong");
		}
		System.out.println("PASS");
	}
}
