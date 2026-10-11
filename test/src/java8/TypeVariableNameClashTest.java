/*
 * Regression test: "as(adapter).as(Number::intValue)" inside a method declaring its own <R>: the receiver's
 * argument R (not the called method's R) was overwritten by the Integer inferred for the called method's, so the
 * method reference was linked as (Integer)Integer: ClassCastException when a Long went through it.
 */
import java.util.function.*;
public class TypeVariableNameClashTest {
	interface Adapter<T, R> { R adapt(T t); }
	static final class Source<T> {
		final Supplier<T> s;
		Source(Supplier<T> s) { this.s = s; }
		<R extends Number> Source<Integer> asInt(Adapter<? super T, ? extends R> adapter) { return as(adapter).as(Number::intValue); }
		<R> Source<R> as(Adapter<? super T, ? extends R> adapter) { return new Source<R>(() -> adapter.adapt(s.get())); }
		T get() { return s.get(); }
	}
	public static void main(String[] a) { Source<String> src = new Source<>(() -> "5"); int actual = src.asInt(Long::valueOf).get() + 1;
		if (actual != 6) {
			throw new RuntimeException("FAIL: " + actual);
		}
		System.out.println("TypeVariableNameClashTest passed!"); }
}
