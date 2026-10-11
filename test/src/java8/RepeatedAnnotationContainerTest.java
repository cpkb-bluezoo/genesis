import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Method;

public class RepeatedAnnotationContainerTest {
	@Retention(RetentionPolicy.RUNTIME)
	@Repeatable(Tags.class)
	@Target({ ElementType.TYPE, ElementType.METHOD, ElementType.FIELD, ElementType.PARAMETER })
	@interface Tag {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE, ElementType.METHOD, ElementType.FIELD, ElementType.PARAMETER })
	@interface Tags {
		Tag[] value();
	}

	@Tag("one")
	@Tag("two")
	@Tag("three")
	static class Repeated {
		@Tag("f1")
		@Tag("f2")
		String field;

		@Tag("m1")
		@Tag("m2")
		void method(@Tag("p1") @Tag("p2") String parameter) {
		}

		@Tag("only")
		void single() {
		}
	}

	static String values(Tag[] tags) {
		StringBuilder out = new StringBuilder();
		for (Tag tag : tags) {
			out.append(tag.value()).append(',');
		}
		return out.toString();
	}

	public static void main(String[] args) throws Exception {
		String type = values(Repeated.class.getAnnotationsByType(Tag.class));
		String field = values(Repeated.class.getDeclaredField("field").getAnnotationsByType(Tag.class));
		Method method = Repeated.class.getDeclaredMethod("method", String.class);
		String methodTags = values(method.getAnnotationsByType(Tag.class));
		String parameter = values(method.getParameters()[0].getAnnotationsByType(Tag.class));
		String single = values(Repeated.class.getDeclaredMethod("single").getAnnotationsByType(Tag.class));
		if (!"one,two,three,".equals(type) || !"f1,f2,".equals(field) || !"m1,m2,".equals(methodTags)
				|| !"p1,p2,".equals(parameter) || !"only,".equals(single)
				|| Repeated.class.getAnnotation(Tags.class) == null) {
			throw new AssertionError(type + field + methodTags + parameter + single);
		}
		System.out.println("PASS");
	}
}
