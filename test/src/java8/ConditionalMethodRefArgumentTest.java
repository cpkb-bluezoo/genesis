import java.util.function.BiConsumer;
import java.util.function.Supplier;

public class ConditionalMethodRefArgumentTest {
	final StringBuilder log = new StringBuilder();

	void first(String name, Integer count) {
		log.append("first:").append(name).append(count).append(';');
	}

	void second(String name, Integer count) {
		log.append("second:").append(name).append(count).append(';');
	}

	String left() {
		return "left";
	}

	String right() {
		return "right";
	}

	static void apply(BiConsumer<String, Integer> consumer) {
		consumer.accept("a", 1);
	}

	static <T> T apply(T bean, Supplier<T> supplier) {
		return supplier.get();
	}

	void run(boolean flag) {
		apply(flag ? this::first : this::second);
		apply((flag ? this::first : this::second));
		String chosen = apply("x", flag ? this::left : this::right);
		log.append(chosen).append(';');
		BiConsumer<String, Integer> local = flag ? this::first : this::second;
		local.accept("l", 2);
	}

	public static void main(String[] args) {
		ConditionalMethodRefArgumentTest yes = new ConditionalMethodRefArgumentTest();
		yes.run(true);
		ConditionalMethodRefArgumentTest no = new ConditionalMethodRefArgumentTest();
		no.run(false);
		if (!"first:a1;first:a1;left;first:l2;".equals(yes.log.toString())
				|| !"second:a1;second:a1;right;second:l2;".equals(no.log.toString())) {
			throw new AssertionError(yes.log + " | " + no.log);
		}
		System.out.println("PASS");
	}
}
