/* A nested enum of a class compiled in the same batch is an Enum: the overload taking Enum<?> is
 * the one for it (not the one taking an int). */
package p;
import java.util.*;
import java.util.function.*;
public class EnumStubOverloadTest {
	interface DriverOption {}
	static class Options {
		StringBuilder sb = new StringBuilder();
		private Options add(DriverOption o, String v) { sb.append("S" + v); return this; }
		private Options add(DriverOption o, int v) { sb.append("I" + v); return this; }
		private Options add(DriverOption o, Enum<?> v) { sb.append("E" + v.name()); return this; }
		private Options add(DriverOption o, List<String> v) { sb.append("L" + v); return this; }
	}
	static class Mapper<T> { T v; Mapper(T v) { this.v = v; } void to(Consumer<T> c) { c.accept(v); } }
	static <T> Mapper<T> from(Supplier<T> s) { return new Mapper<>(s.get()); }
	private final Settings properties = new Settings();
	void run() {
		Options options = new Options();
		DriverOption d = new DriverOption() {};
		from(this.properties::getCompression).to((compression) -> options.add(d, compression));
		if (!"ELZ4".equals(options.sb.toString())) { throw new AssertionError(options.sb); }
		System.out.println("PASS");
	}
	public static void main(String[] a) { new EnumStubOverloadTest().run(); }
}
