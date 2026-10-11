import java.util.ArrayList;
import java.util.List;

public class LambdaForUnboundedWildcardTargetTest {
	static class Context {
		String name() { return "context"; }
	}

	static class WebContext extends Context {
		String name() { return "web"; }
	}

	interface Initializer<C extends Context> {
		void initialize(C context);
	}

	static final List<Initializer<?>> initializers = new ArrayList<Initializer<?>>();

	static void add(Initializer<?>... added) {
		for (Initializer<?> initializer : added) {
			initializers.add(initializer);
		}
	}

	static void addOne(Initializer<?> added) {
		initializers.add(added);
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	static String runAll(Context context) {
		StringBuilder sb = new StringBuilder();
		for (Initializer initializer : initializers) {
			initializer.initialize(context);
		}
		return sb.toString();
	}

	static final StringBuilder seen = new StringBuilder();

	public static void main(String[] args) {
		add((context) -> seen.append(context.name()).append(';'));
		addOne((context) -> {
			String name = context.name();
			seen.append(name.length()).append(';');
		});
		runAll(new WebContext());
		if (!"web;3;".equals(seen.toString())) {
			throw new AssertionError("got " + seen);
		}
		System.out.println("PASS");
	}
}
