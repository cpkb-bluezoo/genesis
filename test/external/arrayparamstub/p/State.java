package p;
import java.util.function.Predicate;
public enum State {
	PRESENT, ABSENT;
	static <T> State search(Iterable<T> source, Predicate<T> predicate) { return ABSENT; }
	static <T> State search(T[] source, int from, int to, Predicate<T> predicate) {
		for (int i = from; i < to; i++) { if (predicate.test(source[i])) { return PRESENT; } }
		return ABSENT;
	}
}
