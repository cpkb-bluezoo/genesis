import java.util.function.Consumer;

public class TypeVariableResourceTest<A extends AutoCloseable> {
	static class Res implements AutoCloseable {
		static int closed;

		public void close() {
			closed++;
		}
	}

	private final A resource;

	TypeVariableResourceTest(A resource) {
		this.resource = resource;
	}

	A create(boolean refresh) {
		return this.resource;
	}

	void consume(boolean refresh, Consumer<? super A> consumer) throws Exception {
		try (A context = create(refresh)) {
			if (refresh) {
				consumer.accept(context);
			}
		}
	}

	public static void main(String[] args) throws Exception {
		final StringBuilder out = new StringBuilder();
		new TypeVariableResourceTest<Res>(new Res()).consume(true, (res) -> out.append(res != null));
		new TypeVariableResourceTest<Res>(new Res()).consume(false, (res) -> out.append("never"));
		if (!"true".equals(out.toString()) || Res.closed != 2) {
			throw new AssertionError(out.toString() + Res.closed);
		}
		System.out.println("PASS");
	}
}
