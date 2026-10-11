import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class MethodRefInsideTernaryOnStackTest {
	private List<String> items;

	private String[] array;

	static String up(String value) {
		return value.toUpperCase();
	}

	String bound(String value) {
		return value + "!";
	}

	void assign(boolean empty, String... values) {
		this.items = empty ? new ArrayList<String>() : Arrays.stream(values).map(MethodRefInsideTernaryOnStackTest::up).toList();
	}

	void assignBound(boolean empty, String... values) {
		this.items = empty ? new ArrayList<String>() : Arrays.stream(values).map(this::bound).toList();
	}

	void assignArray(boolean empty, String... values) {
		this.array = empty ? new String[0] : Arrays.stream(values).toArray(String[]::new);
	}

	static String join(String prefix, List<String> list) {
		return prefix + list;
	}

	public static void main(String[] args) {
		MethodRefInsideTernaryOnStackTest test = new MethodRefInsideTernaryOnStackTest();
		test.assign(false, "a", "b");
		String first = test.items.toString();
		test.assignBound(false, "c");
		String second = test.items.toString();
		test.assignArray(false, "d", "e");
		String third = Arrays.toString(test.array);
		String joined = join("x", args.length == 99 ? new ArrayList<String>()
				: Arrays.stream(new String[] { "f" }).map(MethodRefInsideTernaryOnStackTest::up).toList());
		if (!"[A, B]".equals(first) || !"[c!]".equals(second) || !"[d, e]".equals(third) || !"x[F]".equals(joined)) {
			throw new AssertionError(first + second + third + joined);
		}
		System.out.println("PASS");
	}
}
