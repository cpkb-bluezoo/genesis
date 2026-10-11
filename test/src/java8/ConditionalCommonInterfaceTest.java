public class ConditionalCommonInterfaceTest {
	interface Resolver {
		String label();
	}

	static class Default implements Resolver {
		public String label() {
			return "default";
		}
	}

	enum NoOp implements Resolver {
		INSTANCE;

		public String label() {
			return "noop";
		}
	}

	static Resolver pick(boolean flag) {
		Resolver resolver = flag ? new Default() : NoOp.INSTANCE;
		return resolver;
	}

	static Resolver pickReturn(boolean flag) {
		return flag ? new Default() : NoOp.INSTANCE;
	}

	static String pickArgument(boolean flag) {
		return describe(flag ? new Default() : NoOp.INSTANCE);
	}

	static String describe(Resolver resolver) {
		return resolver.label();
	}

	public static void main(String[] args) {
		if (!"default".equals(pick(true).label()) || !"noop".equals(pick(false).label())
				|| !"default".equals(pickReturn(true).label()) || !"noop".equals(pickArgument(false))) {
			throw new AssertionError("wrong");
		}
		System.out.println("PASS");
	}
}
