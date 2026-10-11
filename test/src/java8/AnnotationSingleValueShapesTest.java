import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

public class AnnotationSingleValueShapesTest {
	@Retention(RetentionPolicy.RUNTIME)
	@interface Order {
		int value() default 0;
	}

	@Retention(RetentionPolicy.RUNTIME)
	@interface Big {
		long value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@interface Letter {
		char value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@interface Ratio {
		double value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@interface Label {
		String value();
	}

	static final int BASE = 40;

	@Order(-1)
	@Big(2L)
	@Letter('c')
	@Ratio(-1.5)
	@Label("a" + "b")
	static class Target {
	}

	@Order(BASE + 2)
	static class Computed {
	}

	@Order((3 * 4))
	static class Parenthesized {
	}

	public static void main(String[] args) {
		Order order = Target.class.getAnnotation(Order.class);
		if (order.value() != -1 || Target.class.getAnnotation(Big.class).value() != 2L
				|| Target.class.getAnnotation(Letter.class).value() != 'c'
				|| Target.class.getAnnotation(Ratio.class).value() != -1.5
				|| !"ab".equals(Target.class.getAnnotation(Label.class).value())
				|| Computed.class.getAnnotation(Order.class).value() != 42
				|| Parenthesized.class.getAnnotation(Order.class).value() != 12) {
			throw new AssertionError("wrong");
		}
		System.out.println("PASS");
	}
}
