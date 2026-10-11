import java.util.function.Supplier;

public class ConstructorTypeVariableFromClassLiteralTest {
	interface Handler {
		String handle(String request, String response);
	}

	static class Definition {
		final Object instance;

		<T> Definition(Class<T> type, Supplier<T> supplier) {
			this.instance = supplier.get();
		}
	}

	public static void main(String[] args) {
		Definition definition = new Definition(Handler.class, () -> (request, response) -> request + response);
		String handled = ((Handler) definition.instance).handle("a", "b");
		Definition plain = new Definition(String.class, () -> "text");
		if (!"ab".equals(handled) || !"text".equals(plain.instance)) {
			throw new AssertionError(handled);
		}
		System.out.println("PASS");
	}
}
