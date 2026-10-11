public class NestedGenericCallTargetTest {
	interface Base {
	}

	static class Sub implements Base {
	}

	static class Stub<T> {
		T value;

		Stub<T> willReturn(T value) {
			this.value = value;
			return this;
		}
	}

	/* "eq(T)" as a matcher: its own T is not the T of the method whose parameter it fills */
	static <T> T eq(T value) {
		return value;
	}

	static <T extends Base> T session(Class<T> type, String config) {
		return null;
	}

	static <T> Stub<T> given(T call) {
		return new Stub<T>();
	}

	public static void main(String[] args) {
		Sub sub = new Sub();
		Stub<Sub> stub = given(session(eq(Sub.class), "config")).willReturn(sub);
		if (stub.value != sub) {
			throw new AssertionError("stub");
		}
		System.out.println("PASS");
	}
}
