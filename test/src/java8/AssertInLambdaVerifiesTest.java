public class AssertInLambdaVerifiesTest {
	static int check(String s) {
		assert s != null : s;
		return s.length();
	}

	public static void main(String[] args) {
		String text = "four";
		Runnable lambda = () -> {
			assert text != null : "text";
			assert text.length() == 4;
		};
		lambda.run();
		assert args != null;
		if (check(text) != 4) {
			throw new AssertionError("length");
		}
		boolean assertionsOn = false;
		assert assertionsOn = true;
		System.out.println("PASS");
	}
}
