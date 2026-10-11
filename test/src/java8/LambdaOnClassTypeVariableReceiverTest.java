import java.util.function.Consumer;

public class LambdaOnClassTypeVariableReceiverTest {
	interface Provider<C> {
		C unwrap();
	}

	static abstract class Runner<SELF extends Runner<SELF, C, A>, C, A extends Provider<C>> {
		final StringBuilder log = new StringBuilder();

		abstract A create();

		@SuppressWarnings("unchecked")
		SELF withBean(String name) {
			this.log.append(name).append(';');
			return (SELF) this;
		}

		/* its own T is not the T of the class that calls it */
		@SuppressWarnings("unchecked")
		<T> SELF withBean(Class<T> type) {
			this.log.append(type.getSimpleName()).append(';');
			return (SELF) this;
		}

		void run(Consumer<? super A> consumer) {
			consumer.accept(create());
		}
	}

	/* the receiver is a type variable of the method itself: each call is an R again */
	static <R extends Runner<R, C, A>, C, A extends Provider<C>> R chain(R runner) {
		return runner.withBean("a").withBean("b");
	}

	static class StringProvider implements Provider<String> {
		public String unwrap() {
			return "context";
		}
	}

	static class StringRunner extends Runner<StringRunner, String, StringProvider> {
		StringProvider create() {
			return new StringProvider();
		}
	}

	static abstract class RunnerTests<T extends Runner<T, C, A>, C, A extends Provider<C>> {
		abstract T get();

		String check() {
			StringBuilder out = new StringBuilder();
			get().withBean("foo").run((context) -> out.append(context.unwrap()));
			get().withBean(String.class).run((context) -> out.append(context.unwrap()));
			return out.toString();
		}
	}

	static class StringRunnerTests extends RunnerTests<StringRunner, String, StringProvider> {
		StringRunner get() {
			return new StringRunner();
		}
	}

	public static void main(String[] args) {
		StringRunner chained = chain(new StringRunner());
		if (!"a;b;".equals(chained.log.toString())) {
			throw new AssertionError(chained.log.toString());
		}
		String checked = new StringRunnerTests().check();
		if (!"contextcontext".equals(checked)) {
			throw new AssertionError(checked);
		}
		System.out.println("PASS");
	}
}
