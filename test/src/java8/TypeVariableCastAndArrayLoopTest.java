import java.util.ArrayList;
import java.util.List;

public class TypeVariableCastAndArrayLoopTest {
	@SuppressWarnings("unchecked")
	static <E extends Throwable> void rethrow(Throwable e) throws E {
		throw (E) e;
	}

	@SuppressWarnings("unchecked")
	static <T extends Comparable<T>> T cast(Object o) {
		return (T) o;
	}

	@SuppressWarnings("unchecked")
	static <T> List<T> list(Object o) {
		return (List<T>) o;
	}

	@SuppressWarnings("unchecked")
	static <T> T[] array(Object o) {
		return (T[]) o;
	}

	static <E extends Number> int sum(E[] values, boolean add) {
		int total = 0;
		for (E value : values) {
			if (add) {
				total += value.intValue();
			}
		}
		return total;
	}

	static <T> int count(T[] source, int from, int to) {
		int n = 0;
		for (T each : source) {
			if (each != null) {
				n++;
			}
		}
		for (int i = from; i < to; i++) {
			n += source[i] == null ? 0 : 1;
		}
		return n;
	}

	public static void main(String[] args) {
		String caught = "none";
		try {
			TypeVariableCastAndArrayLoopTest.<RuntimeException>rethrow(new IllegalStateException("x"));
		}
		catch (IllegalStateException ex) {
			caught = ex.getMessage();
		}
		String text = cast("s");
		int size = TypeVariableCastAndArrayLoopTest.<String>list(new ArrayList<String>()).size();
		int length = TypeVariableCastAndArrayLoopTest.<Object>array(new Object[2]).length;
		int total = sum(new Integer[] { 1, 2, 3 }, true);
		int counted = count(new String[] { "a", null, "c" }, 0, 3);
		if (!"x".equals(caught) || !"s".equals(text) || size != 0 || length != 2 || total != 6 || counted != 4) {
			throw new AssertionError(caught + text + size + length + total + counted);
		}
		System.out.println("PASS");
	}
}
