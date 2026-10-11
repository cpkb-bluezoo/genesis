import java.io.Closeable;

public class SequentialTryResourcesInLambdaTest {
	interface Body {
		void run() throws Exception;
	}

	static class Resource implements Closeable {
		static int closed;

		public void close() {
			closed++;
		}
	}

	static void execute(Body body) throws Exception {
		body.run();
	}

	public static void main(String[] args) throws Exception {
		execute(() -> {
			try (Resource connection = new Resource()) {
				connection.toString();
			}
			try (Resource connection = new Resource()) {
				connection.toString();
			}
			try (Resource connection = new Resource(); Resource other = new Resource()) {
				other.toString();
			}
		});
		if (Resource.closed != 4) {
			throw new AssertionError(Resource.closed);
		}
		System.out.println("PASS");
	}
}
