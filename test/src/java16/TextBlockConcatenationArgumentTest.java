public class TextBlockConcatenationArgumentTest {
	static boolean same(String actual, String expected) {
		return actual.equals(expected);
	}

	static String joined() {
		return """
				{"a":1,""" + """
				"b":2}""";
	}

	public static void main(String[] args) {
		boolean inArgument = same("""
				{"a":1,""" + """
				"b":2}""", "{\"a\":1,\"b\":2}");
		String withVariable = """
				left""" + args.length + """
				right""";
		if (!inArgument || !"{\"a\":1,\"b\":2}".equals(joined()) || !"left0right".equals(withVariable)) {
			throw new AssertionError(joined() + withVariable);
		}
		System.out.println("PASS");
	}
}
