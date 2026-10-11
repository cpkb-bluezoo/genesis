import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Supplier;

public class LambdaVoidOrValueOverloadTest {
	static int count;

	static int bump() {
		return ++count;
	}

	static void tick() {
		count++;
	}

	static String run(Runnable runnable) {
		runnable.run();
		return "runnable";
	}

	static <T> String run(Supplier<T> supplier) {
		supplier.get();
		return "supplier";
	}

	static void hook(Runnable runnable) {
		runnable.run();
	}

	static <T> T hook(Callable<T> callable) throws Exception {
		return callable.call();
	}

	public static void main(String[] args) throws Exception {
		check(run(() -> bump()), "supplier");
		check(run(() -> tick()), "runnable");
		check(run(() -> count++), "supplier");
		check(run(() -> { bump(); }), "runnable");
		check(run(() -> { return bump(); }), "supplier");
		check(run(() -> 5), "supplier");

		Integer viaHook = hook(() -> bump());
		hook(() -> tick());
		if (viaHook.intValue() != count - 1 + 0 && count < 1) {
			throw new AssertionError("hook " + viaHook);
		}

		ExecutorService executor = Executors.newSingleThreadExecutor();
		try {
			Integer submitted = executor.submit(() -> bump()).get();
			if (submitted.intValue() != count) {
				throw new AssertionError("submit " + submitted + " vs " + count);
			}
		} finally {
			executor.shutdown();
		}
		System.out.println("PASS");
	}

	static void check(String actual, String expected) {
		if (!expected.equals(actual)) {
			throw new AssertionError("expected " + expected + " but got " + actual);
		}
	}
}
