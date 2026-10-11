public class InheritedInterfaceConstantTest {
	interface Ordered {
		int HIGHEST = Integer.MIN_VALUE;
		int LOWEST = Integer.MAX_VALUE;
	}

	static abstract class Mapping implements Ordered {
		int order;

		void setOrder(int order) {
			this.order = order;
		}
	}

	static class Welcome extends Mapping {
		Welcome() {
			setOrder(LOWEST - 10);
		}

		int highest() {
			return HIGHEST;
		}
	}

	static class Deeper extends Welcome {
		int lowest() {
			return LOWEST;
		}
	}

	public static void main(String[] args) {
		Welcome welcome = new Welcome();
		if (welcome.order != Integer.MAX_VALUE - 10 || welcome.highest() != Integer.MIN_VALUE
				|| new Deeper().lowest() != Integer.MAX_VALUE) {
			throw new AssertionError(welcome.order);
		}
		System.out.println("PASS");
	}
}
