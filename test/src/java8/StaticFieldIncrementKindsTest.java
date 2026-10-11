public class StaticFieldIncrementKindsTest {
	static long total = 10;
	static double ratio = 1;
	static byte small = 127;
	static char letter = 'a';
	static short shorty = 1;
	static float fl = 1;
	static int plain = 5;

	public static void main(String[] args) {
		long x = total++;
		double y = ++ratio;
		byte z = ++small;
		char w = ++letter;
		short v = shorty--;
		float u = fl++;
		int p = plain-- + --plain;
		String got = x + " " + total + " " + y + " " + z + " " + w + " " + v + " " + shorty
			+ " " + u + " " + fl + " " + p + " " + plain;
		String expected = "10 11 2.0 -128 b 1 0 1.0 2.0 8 3";
		if (!expected.equals(got)) {
			throw new AssertionError("expected " + expected + " but got " + got);
		}
		System.out.println("PASS");
	}
}
