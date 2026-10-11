package p;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

/**
 * flatMap(Items::stream) over a List of Items: an unbound instance method reference whose
 * result mentions a member type declared in another file.
 */
public class FlatMapUnboundRefTest {
	static Items of(Items.Entry... entries) {
		return () -> Stream.of(entries);
	}

	public static void main(String[] args) {
		List<Items> parts = new ArrayList<Items>();
		parts.add(of(new Items.Entry("a", 1), new Items.Entry("b", 2)));
		parts.add(of(new Items.Entry("b", 3), new Items.Entry("c", 4)));
		long count = new Composite(parts).stream().count();
		if (count != 3) {
			throw new AssertionError("expected 3 distinct, got " + count);
		}
		System.out.println("PASS");
	}
}
