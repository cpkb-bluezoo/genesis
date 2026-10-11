public class FieldOfGenericCallResultTest {
	static class Service {
		String name = "service";
		Bean bean = new Bean();
		static int count = 3;
	}

	static class Bean {
		String name = "bean";
		int[] values = { 1, 2, 3 };
	}

	static <T> T get(Class<T> type) {
		try {
			return type.getDeclaredConstructor().newInstance();
		} catch (Exception ex) {
			throw new IllegalStateException(ex);
		}
	}

	public static void main(String[] args) {
		String name = get(Service.class).name;
		String beanName = get(Service.class).bean.name;
		int first = get(Service.class).bean.values[0];
		int length = get(Service.class).bean.values.length;
		Object asObject = get(Service.class).bean;
		check(name, "service");
		check(beanName, "bean");
		check(String.valueOf(first), "1");
		check(String.valueOf(length), "3");
		check(String.valueOf(asObject instanceof Bean), "true");
		System.out.println("PASS");
	}

	static void check(String actual, String expected) {
		if (!expected.equals(actual)) {
			throw new AssertionError("expected " + expected + " but got " + actual);
		}
	}
}
