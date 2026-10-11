import java.util.ArrayList;
import java.util.List;

public class ListOfMethodRefReturnTargetTest {
	interface Builder<B extends Builder<B>> {
		B name(String name);
	}

	interface Customizer<B extends Builder<B>> {
		void customize(B builder);
	}

	static class Impl implements Builder<Impl> {
		final List<String> names = new ArrayList<String>();

		public Impl name(String name) {
			names.add(name);
			return this;
		}
	}

	private int applied;

	<B extends Builder<B>> void apply(B builder) {
		builder.name("applied");
		applied++;
	}

	<B extends Builder<B>> List<Customizer<B>> forFactory() {
		return List.of(this::apply);
	}

	<B extends Builder<B>> List<Customizer<B>> forFactoryTwo() {
		return List.of(this::apply, this::apply);
	}

	public static void main(String[] args) {
		ListOfMethodRefReturnTargetTest test = new ListOfMethodRefReturnTargetTest();
		Impl impl = new Impl();
		List<Customizer<Impl>> customizers = test.forFactory();
		customizers.get(0).customize(impl);
		List<Customizer<Impl>> two = test.forFactoryTwo();
		two.get(1).customize(impl);
		if (test.applied != 2 || impl.names.size() != 2) {
			throw new AssertionError(test.applied + " " + impl.names);
		}
		System.out.println("PASS");
	}
}
