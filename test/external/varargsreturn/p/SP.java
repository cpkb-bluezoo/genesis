package p;
public final class SP {
	private SP() {}
	public static String get(String... properties) {
		for (String property : properties) {
			String v = System.getProperty(property);
			if (v != null) return v;
		}
		return null;
	}
}
