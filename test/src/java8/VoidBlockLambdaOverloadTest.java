import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

public class VoidBlockLambdaOverloadTest {
	interface Interceptor extends Function<StringBuilder, StringBuilder> {
	}

	static class Registry {
		final List<String> calls = new ArrayList<String>();

		Registry forAcceptor(Interceptor interceptor) {
			this.calls.add("interceptor:" + interceptor.apply(new StringBuilder("x")));
			return this;
		}

		Registry forAcceptor(Consumer<List<Interceptor>> interceptors) {
			List<Interceptor> list = new ArrayList<Interceptor>();
			interceptors.accept(list);
			this.calls.add("consumer:" + list.size());
			return this;
		}
	}

	public static void main(String[] args) {
		Registry registry = new Registry();
		registry.forAcceptor((interceptors) -> {
			interceptors.add((builder) -> builder);
			interceptors.size();
		});
		registry.forAcceptor((builder) -> {
			return builder.append("z");
		});
		if (!"[consumer:1, interceptor:xz]".equals(registry.calls.toString())) {
			throw new AssertionError(registry.calls.toString());
		}
		System.out.println("PASS");
	}
}
