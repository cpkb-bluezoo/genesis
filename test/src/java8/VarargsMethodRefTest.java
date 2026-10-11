import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

public class VarargsMethodRefTest {
	static class Builder {
		final int count;

		Builder(String... names) {
			this.count = names.length;
		}
	}

	static int count(String... names) {
		return names.length;
	}

	int total(int... values) {
		int sum = 0;
		for (int v : values) {
			sum += v;
		}
		return sum;
	}

	static <T> T get(Supplier<T> supplier) {
		return supplier.get();
	}

	public static void main(String[] args) {
		Builder none = get(Builder::new);
		Function<String, Builder> one = Builder::new;
		BiFunction<String, String, Builder> two = Builder::new;
		Supplier<Integer> zero = VarargsMethodRefTest::count;
		Function<String, Integer> single = VarargsMethodRefTest::count;
		BiFunction<String, String, Integer> pair = VarargsMethodRefTest::count;
		VarargsMethodRefTest self = new VarargsMethodRefTest();
		Supplier<Integer> bound = self::total;
		Function<Integer, Integer> boundOne = self::total;
		Function<VarargsMethodRefTest, Integer> unboundZero = VarargsMethodRefTest::total;
		BiFunction<VarargsMethodRefTest, Integer, Integer> unboundOne = VarargsMethodRefTest::total;
		if (unboundZero.apply(self) != 0 || unboundOne.apply(self, 7) != 7) {
			throw new AssertionError("unbound");
		}
		if (none.count != 0 || one.apply("a").count != 1 || two.apply("a", "b").count != 2) {
			throw new AssertionError("constructor");
		}
		if (zero.get() != 0 || single.apply("a") != 1 || pair.apply("a", "b") != 2) {
			throw new AssertionError("static");
		}
		if (bound.get() != 0 || boundOne.apply(5) != 5) {
			throw new AssertionError("bound");
		}
		System.out.println("PASS");
	}
}
