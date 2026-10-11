public class StaticFieldIncrementFromNestedClassTest {
	static class Base {
		static int inherited;
	}

	static class Nested extends Base {
		static int bumpInt() { return ++count; }
		static int bumpIntPost() { return count++; }
		static long bumpLong() { return total++; }
		static double bumpDouble() { return --ratio; }
		static byte bumpByte() { return ++small; }
		static char bumpChar() { return ++letter; }
		static int bumpInherited() { return inherited++ + ++inherited; }
		static void bumpStatement() { count++; --count; count++; }

		class Inner {
			int viaInner() { return count++ + (int) total--; }
		}
	}

	static int count;
	static long total = 10;
	static double ratio = 1.5;
	static byte small = 127;
	static char letter = 'a';

	public static void main(String[] args) {
		check(Nested.bumpInt(), 1);
		check(Nested.bumpIntPost(), 1);
		check(count, 2);
		check(Nested.bumpLong(), 10);
		check(total, 11);
		check(Nested.bumpDouble() == 0.5, true);
		check(Nested.bumpByte(), -128);
		check(Nested.bumpChar(), 'b');
		check(Nested.bumpInherited(), 2);
		check(Base.inherited, 2);
		Nested.bumpStatement();
		check(count, 3);
		check(new Nested().new Inner().viaInner(), 3 + 11);
		check(count, 4);
		check(total, 10);
		System.out.println("PASS");
	}

	static void check(long actual, long expected) {
		if (actual != expected) {
			throw new AssertionError("expected " + expected + " but got " + actual);
		}
	}

	static void check(boolean actual, boolean expected) {
		if (actual != expected) {
			throw new AssertionError("expected " + expected + " but got " + actual);
		}
	}
}
