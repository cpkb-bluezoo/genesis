package p;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * A method inherited from a generic interface of another file, whose own type
 * variable ("O extends Operation") the receiver binds ("Exposable<JmxOperation>").
 */
public class InheritedGenericMemberTest {
	private String describe(JmxOperation operation) {
		return operation.getName();
	}

	String[] names(ExposableJmx endpoint) {
		return endpoint.getOperations().stream().map(this::describe).toArray(String[]::new);
	}

	public static void main(String[] args) {
		final List<JmxOperation> operations = new ArrayList<JmxOperation>();
		operations.add(() -> "read");
		operations.add(() -> "write");
		ExposableJmx endpoint = new ExposableJmx() {
			public String getId() {
				return "id";
			}

			public Collection<JmxOperation> getOperations() {
				return operations;
			}
		};
		String[] names = new InheritedGenericMemberTest().names(endpoint);
		if (names.length != 2 || !"read".equals(names[0]) || !"write".equals(names[1])) {
			throw new AssertionError(names.length);
		}
		System.out.println("PASS");
	}
}
