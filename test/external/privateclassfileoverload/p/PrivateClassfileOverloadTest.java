package p;

/**
 * Gate is read from a class file: a private, more specific overload is no
 * candidate for a call from another class, through "this.field" as well.
 * (Which overload came first used to decide it, so there are several pairs.)
 */
public class PrivateClassfileOverloadTest {
	static class Recording extends Gate.MockRequest {}

	private final Gate gate = new Gate();
	private final Recording request = new Recording();

	String run() {
		StringBuilder sb = new StringBuilder();
		sb.append(this.gate.one(this.request)).append(',');
		sb.append(this.gate.two(this.request)).append(',');
		sb.append(this.gate.three(this.request)).append(',');
		sb.append(this.gate.four(this.request)).append(',');
		sb.append(this.gate.five(this.request)).append(',');
		sb.append(this.gate.six(this.request)).append(',');
		sb.append(this.gate.seven(this.request)).append(',');
		sb.append(this.gate.eight(this.request)).append(',');
		return sb.toString();
	}

	public static void main(String[] args) {
		PrivateClassfileOverloadTest test = new PrivateClassfileOverloadTest();
		String expected = "public,public,public,public,public,public,public,public,";
		if (!expected.equals(test.run())) {
			throw new AssertionError("expected " + expected + " but got " + test.run());
		}
		if (!"privateprivate".equals(test.gate.inside(test.request))) {
			throw new AssertionError("the class itself calls its own private overloads");
		}
		System.out.println("PASS");
	}
}
