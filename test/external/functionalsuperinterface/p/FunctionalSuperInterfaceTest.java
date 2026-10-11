package p;

import java.util.Collection;
import java.util.Collections;

/**
 * @FunctionalInterface on an interface whose only abstract method is its
 * superinterface's, declared in another file of the same compilation.
 */
public class FunctionalSuperInterfaceTest {
	public static void main(String[] args) {
		JmxEndpoints endpoints = () -> Collections.<JmxExposable>emptyList();
		Collection<JmxExposable> found = endpoints.getEndpoints();
		if (!found.isEmpty()) {
			throw new AssertionError("expected none");
		}
		System.out.println("PASS");
	}
}
