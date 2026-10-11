public class MethodCallOnTypeVariableResultTest<C extends CharSequence> {
	private final C value;

	MethodCallOnTypeVariableResultTest(C value) {
		this.value = value;
	}

	private C get() {
		return this.value;
	}

	<D extends Number> D pass(D number) {
		return number;
	}

	int length() {
		return get().length();
	}

	Object boxedLength() {
		return get().length();
	}

	Object notEmpty() {
		return !get().isEmpty();
	}

	Object sub() {
		return get().subSequence(0, 1);
	}

	String text() {
		return get().toString();
	}

	long viaMethodVariable() {
		return pass(Integer.valueOf(7)).longValue();
	}

	Object boxedViaMethodVariable() {
		return pass(Integer.valueOf(8)).intValue();
	}

	public static void main(String[] args) {
		MethodCallOnTypeVariableResultTest<String> test = new MethodCallOnTypeVariableResultTest<String>("abc");
		if (test.length() != 3 || !Integer.valueOf(3).equals(test.boxedLength()) || !Boolean.TRUE.equals(test.notEmpty())
				|| !"a".equals(test.sub().toString()) || !"abc".equals(test.text())
				|| test.viaMethodVariable() != 7L || !Integer.valueOf(8).equals(test.boxedViaMethodVariable())) {
			throw new AssertionError("wrong");
		}
		System.out.println("PASS");
	}
}
