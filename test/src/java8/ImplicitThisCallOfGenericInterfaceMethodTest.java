import java.util.function.Supplier;

public class ImplicitThisCallOfGenericInterfaceMethodTest {
	interface Tagged<T> {
		default Object tag() {
			return "tag";
		}

		default T self(T value) {
			return value;
		}
	}

	interface Endpoints<E> extends Supplier<E> {
	}

	@FunctionalInterface
	interface Names extends Endpoints<String> {
	}

	static class Before {
	}

	static class Impl implements Tagged<String> {
		Object viaThis() {
			return tag();
		}

		String viaSelf() {
			return self("me");
		}

		Object explicit() {
			return this.tag();
		}
	}

	public static void main(String[] args) {
		Impl impl = new Impl();
		Names names = () -> "name";
		if (!"tag".equals(impl.viaThis()) || !"me".equals(impl.viaSelf()) || !"tag".equals(impl.explicit())
				|| !"name".equals(names.get())) {
			throw new AssertionError("wrong");
		}
		System.out.println("PASS");
	}
}
