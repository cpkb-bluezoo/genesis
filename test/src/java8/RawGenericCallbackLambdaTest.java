import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.Consumer;

public class RawGenericCallbackLambdaTest {
	interface Builder<T> {
		String describe();
	}

	interface Customizer<B extends Builder<?>> {
		B customize(B builder);
	}

	static class Callbacks<C, A> {
		private final Collection<? extends C> callbacks;

		Callbacks(Collection<? extends C> callbacks) {
			this.callbacks = callbacks;
		}

		void invoke(Consumer<C> invoker) {
			for (C callback : this.callbacks) {
				invoker.accept(callback);
			}
		}
	}

	static <C, A> Callbacks<C, A> callbacks(Class<C> type, Collection<? extends C> instances, A argument) {
		return new Callbacks<C, A>(instances);
	}

	static class Plain implements Builder<String> {
		final String name;

		Plain(String name) {
			this.name = name;
		}

		public String describe() {
			return this.name;
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static void main(String[] args) {
		List<Customizer<?>> customizers = new ArrayList<Customizer<?>>();
		customizers.add(new Customizer<Plain>() {
			public Plain customize(Plain builder) {
				return new Plain(builder.name + "+");
			}
		});
		customizers.add(new Customizer<Plain>() {
			public Plain customize(Plain builder) {
				return new Plain(builder.name + "!");
			}
		});
		final Builder<?>[] reference = { new Plain("start") };
		callbacks(Customizer.class, customizers, reference[0])
			.invoke((customizer) -> reference[0] = customizer.customize(reference[0]));
		if (!"start+!".equals(reference[0].describe())) {
			throw new AssertionError(reference[0].describe());
		}
		System.out.println("PASS");
	}
}
