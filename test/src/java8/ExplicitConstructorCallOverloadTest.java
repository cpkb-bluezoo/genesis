import java.util.function.Function;
import java.util.function.Supplier;

public class ExplicitConstructorCallOverloadTest {
	static class Base<S extends Base<S, C>, C extends CharSequence> {
		final Function<Config<C>, S> factory;
		final Config<C> config;

		protected Base(Function<Config<C>, S> factory, Supplier<C> supplier, Class<?>... extra) {
			this.factory = factory;
			this.config = new Config<C>(supplier);
		}

		protected Base(Config<C> config, Function<Config<C>, S> factory) {
			this.config = config;
			this.factory = factory;
		}

		protected static final class Config<C> {
			final Supplier<C> supplier;
			Config(Supplier<C> supplier) { this.supplier = supplier; }
		}
	}

	static class Runner extends Base<Runner, StringBuilder> {
		Runner() {
			this(StringBuilder::new);
		}

		Runner(Supplier<StringBuilder> supplier) {
			super(Runner::new, supplier);
		}

		Runner(Supplier<StringBuilder> supplier, Class<?>... extra) {
			super(Runner::new, supplier, extra);
		}

		private Runner(Config<StringBuilder> config) {
			super(config, Runner::new);
		}
	}

	public static void main(String[] args) {
		Runner r = new Runner();
		if (r.config.supplier.get().length() != 0 || r.factory.apply(r.config).config.supplier.get().length() != 0) {
			throw new AssertionError("wrong");
		}
		System.out.println("PASS");
	}
}
