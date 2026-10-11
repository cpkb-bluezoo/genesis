public class DefaultOverridesAbstractFunctionalInterfaceTest {
	interface Contributor {
		Object asContributor();
	}

	@FunctionalInterface
	interface Indicator extends Contributor {
		@Override
		default String asContributor() {
			return "adapter:" + health();
		}

		String health();
	}

	interface Reactive {
		Object asContributor();
	}

	@FunctionalInterface
	interface ReactiveIndicator extends Reactive {
		@Override
		default String asContributor() {
			return "reactive:" + health();
		}

		String health();
	}

	public static void main(String[] args) {
		Indicator indicator = () -> "up";
		ReactiveIndicator reactive = () -> "ok";
		if (!"adapter:up".equals(indicator.asContributor()) || !"reactive:ok".equals(reactive.asContributor())) {
			throw new AssertionError("wrong");
		}
		System.out.println("PASS");
	}
}
