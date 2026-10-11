import java.util.function.IntSupplier;
import java.util.function.LongSupplier;
import java.util.function.ToDoubleFunction;
import java.util.function.ToLongFunction;

public class LambdaResultConversionAndEnumValueOfTest {
	enum Status {
		OK(200), MISSING(404);

		final int code;

		Status(int code) {
			this.code = code;
		}

		static Status valueOf(int code) {
			for (Status status : values()) {
				if (status.code == code) {
					return status;
				}
			}
			throw new IllegalArgumentException("" + code);
		}
	}

	static class Stats {
		long deletes() {
			return 7L;
		}

		int pending() {
			return 3;
		}
	}

	static <T> double gauge(T value, ToDoubleFunction<T> function) {
		return function.applyAsDouble(value);
	}

	public static void main(String[] args) {
		Integer boxed = 404;
		int primitive = 200;
		ToDoubleFunction<Stats> longToDouble = (stats) -> stats.deletes();
		ToDoubleFunction<Stats> intToDouble = (stats) -> stats.pending();
		ToLongFunction<Stats> intToLong = (stats) -> stats.pending();
		LongSupplier boxedToLong = () -> Integer.valueOf(5);
		IntSupplier boxedToInt = () -> Integer.valueOf(6);
		Stats stats = new Stats();
		double total = gauge(stats, (s) -> s.deletes()) + longToDouble.applyAsDouble(stats)
				+ intToDouble.applyAsDouble(stats) + intToLong.applyAsLong(stats) + boxedToLong.getAsLong()
				+ boxedToInt.getAsInt();
		String statuses = Status.valueOf(boxed) + " " + Status.valueOf(primitive) + " " + Status.valueOf("OK");
		if (total != 7 + 7 + 3 + 3 + 5 + 6 || !"MISSING OK OK".equals(statuses)) {
			throw new AssertionError(total + statuses);
		}
		System.out.println("PASS");
	}
}
