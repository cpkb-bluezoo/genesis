import java.util.function.Predicate;

public class BooleanBitwiseOperatorsTest {
	static int calls;

	static boolean yes() { calls++; return true; }
	static boolean no() { calls++; return false; }

	interface Matcher { boolean matches(Predicate<String> active); }

	public static void main(String[] args) {
		boolean t = true;
		boolean f = false;
		check(t | f, true);
		check(f | f, false);
		check(t & f, false);
		check(t & t, true);
		check(t ^ f, true);
		check(t ^ t, false);

		// both operands are always evaluated, unlike || and &&
		calls = 0;
		check(yes() | no(), true);
		check(no() & yes(), false);
		check(yes() ^ yes(), false);
		if (calls != 6) {
			throw new AssertionError("expected 6 calls, got " + calls);
		}

		Boolean boxed = Boolean.TRUE;
		check(boxed | f, true);
		check(f | boxed, true);
		check(boxed & boxed, true);

		boolean acc = false;
		acc |= t;
		check(acc, true);
		acc &= f;
		check(acc, false);
		acc ^= t;
		check(acc, true);

		Predicate<String> p = (s) -> s.length() > 1;
		check(p.test("a") | p.test("bb"), true);
		Matcher m = (q) -> q.test("a") | q.test("bb");
		check(m.matches(p), true);
		System.out.println("PASS");
	}

	static void check(boolean actual, boolean expected) {
		if (actual != expected) {
			throw new AssertionError("expected " + expected + " but got " + actual);
		}
	}
}
