package p;

/**
 * forEach (inherited from Iterable<Conv<?>>) of a class of another file, given a bound
 * method reference.
 */
public class IterableClassForEachTest {
	private final Convs legacy;

	IterableClassForEachTest(Convs legacy) {
		this.legacy = legacy;
	}

	void customize(Builder builder) {
		if (this.legacy != null) {
			this.legacy.forEach(builder::addCustom);
		}
	}

	public static void main(String[] args) {
		Convs convs = new Convs();
		convs.add(() -> "a");
		convs.add(() -> "b");
		Builder builder = new Builder();
		new IterableClassForEachTest(convs).customize(builder);
		if (!"a;b;".equals(builder.names())) {
			throw new AssertionError(builder.names());
		}
		System.out.println("PASS");
	}
}
