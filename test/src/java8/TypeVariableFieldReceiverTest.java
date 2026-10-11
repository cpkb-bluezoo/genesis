import java.util.Properties;

public class TypeVariableFieldReceiverTest<T extends CharSequence> {
	private final T value;

	TypeVariableFieldReceiverTest(T value) {
		this.value = value;
	}

	static class Contributor<P extends Properties> {
		private final P properties;

		Contributor(P properties) {
			this.properties = properties;
		}

		String copy(String key) {
			String viaGetProperty = this.properties.getProperty(key);
			Object viaGet = this.properties.get(key);
			return viaGetProperty + "/" + viaGet;
		}
	}

	int length() {
		return this.value.length();
	}

	CharSequence sub() {
		return this.value.subSequence(0, 1);
	}

	Object boxed() {
		return this.value.length();
	}

	String text() {
		return this.value.toString();
	}

	public static void main(String[] args) {
		TypeVariableFieldReceiverTest<String> test = new TypeVariableFieldReceiverTest<String>("abc");
		Properties properties = new Properties();
		properties.setProperty("k", "v");
		if (test.length() != 3 || !"a".equals(test.sub().toString()) || !Integer.valueOf(3).equals(test.boxed())
				|| !"abc".equals(test.text()) || !"v/v".equals(new Contributor<Properties>(properties).copy("k"))) {
			throw new AssertionError("wrong");
		}
		System.out.println("PASS");
	}
}
