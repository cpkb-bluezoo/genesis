public class ConstructorSuperArgumentConcatenationTest {
	static class Base extends RuntimeException {
		Base(String message, String description, Throwable cause) {
			super(message, cause);
		}
	}

	static class Mapping extends Base {
		final Object value;

		Mapping(String parameter, Object value, Throwable cause) {
			super("Failed to map " + value + " of type " + ((value != null) ? value.getClass() : null) + " to " + parameter,
					"Parameter mapping failure", cause);
			this.value = value;
		}
	}

	public static void main(String[] args) {
		Mapping with = new Mapping("p", "v", null);
		Mapping without = new Mapping("p", null, null);
		if (!"Failed to map v of type class java.lang.String to p".equals(with.getMessage())
				|| !"Failed to map null of type null to p".equals(without.getMessage())) {
			throw new AssertionError(with.getMessage() + without.getMessage());
		}
		System.out.println("PASS");
	}
}
