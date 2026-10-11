import java.lang.annotation.ElementType;
import java.lang.annotation.Target;

public class TypeParameterAnnotationTest {
	@Target({ ElementType.TYPE_USE, ElementType.TYPE_PARAMETER })
	@interface Nullable {
	}

	interface Accessor<S, @Nullable P extends @Nullable Object> {

		@Nullable P get(S source);

	}

	static class Box<@Nullable T, @Nullable U extends Number> {
		T first;
		U second;
	}

	static <@Nullable T extends CharSequence> int length(T text) {
		return text.length();
	}

	public static void main(String[] args) {
		Accessor<String, Integer> accessor = (source) -> source.length();
		Box<String, Integer> box = new Box<String, Integer>();
		box.first = "x";
		box.second = 3;
		if (accessor.get("four") != 4 || length("abc") != 3 || box.second != 3) {
			throw new AssertionError("wrong");
		}
		System.out.println("PASS");
	}
}
