/*
 * Regression test: "Outer.this.changes::add" (a bound method reference whose receiver is a field of an
 * enclosing instance) read the field from java.lang.Object: NoSuchFieldError.
 */
import java.util.*;
import java.util.function.*;
public class QualifiedThisFieldMethodRefTest {
	private final List<String> changes = new ArrayList<>();
	class Inner {
		Consumer<String> adder() { return QualifiedThisFieldMethodRefTest.this.changes::add; }
	}
	public static void main(String[] a) { QualifiedThisFieldMethodRefTest q = new QualifiedThisFieldMethodRefTest(); q.new Inner().adder().accept("x"); if (!"[x]".equals(q.changes.toString())) {
			throw new RuntimeException("FAIL: " + q.changes);
		}
		System.out.println("QualifiedThisFieldMethodRefTest passed!");
	}
}
