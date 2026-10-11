/*
 * Regression test: "Info::new" for an inner (non-static) member class referenced the constructor without the
 * enclosing instance it takes: NoSuchMethodError when the method reference ran.
 */
import java.util.*;
import java.util.function.*;
public class InnerClassConstructorRefTest {
	final String tag = "t";
	public final class Info { final Object c; private Info(Object c) { this.c = c; } String tag() { return tag + c; } }
	public final class Plain { String tag() { return tag; } }
	List<Info> all(Object[] cs) { return Arrays.stream(cs).map(Info::new).toList(); }
	Supplier<Plain> make() { return Plain::new; }
	public static void main(String[] a) { InnerClassConstructorRefTest ic = new InnerClassConstructorRefTest(); String actual = ic.all(new Object[] {"x", "y"}).get(1).tag() + ic.make().get().tag();
		if (!"tyt".equals(actual)) {
			throw new RuntimeException("FAIL: " + actual);
		}
		System.out.println("InnerClassConstructorRefTest passed!"); }
}
