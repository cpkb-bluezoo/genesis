package p;

/**
 * "return obtain(Props::isLenient, Config.super::enabled)" through two levels of
 * generic superclass, the method's own type variable V apart from the class's T.
 */
public class TwoLevelGenericAdapterTest {
	public static void main(String[] args) {
		Adapter adapter = new Adapter();
		if (!adapter.enabled() || adapter.steps() != 3 || !"none".equals(adapter.prefix())) {
			throw new AssertionError("wrong");
		}
		System.out.println("PASS");
	}
}
