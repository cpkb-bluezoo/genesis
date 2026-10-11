public class OddMainMethodsAreJustMethodsTest {
	static class Launcher {
		static void main(String[] args) {
			ran++;
		}

		static void main() {
			ran++;
		}

		int main(String[] args, int extra) {
			return extra;
		}
	}

	static class Returning {
		static int main(String[] args) {
			return args.length;
		}
	}

	static int ran;

	public static void main(String[] args) {
		Launcher.main(new String[0]);
		Launcher.main();
		if (ran != 2) {
			throw new AssertionError("ran " + ran);
		}
		if (Returning.main(new String[3]) != 3) {
			throw new AssertionError("returning main");
		}
		System.out.println("PASS");
	}
}
