import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public class ConstructorSingletonListLambdaTest {
	interface Function {
		String apply(StringBuilder data);
	}

	static class Sanitizer {
		private final List<Function> functions;

		Sanitizer(Collection<? extends Function> functions) {
			this.functions = new java.util.ArrayList<Function>(functions);
		}

		String run(StringBuilder data) {
			String last = "";
			for (Function function : this.functions) {
				last = function.apply(data);
			}
			return last;
		}
	}

	public static void main(String[] args) {
		Sanitizer single = new Sanitizer(Collections.singletonList((data) -> data.append("!").toString()));
		Sanitizer several = new Sanitizer(Arrays.asList((data) -> data.append("a").toString(),
				(data) -> data.append("b").toString()));
		String one = single.run(new StringBuilder("x"));
		String two = several.run(new StringBuilder("y"));
		if (!"x!".equals(one) || !"yab".equals(two)) {
			throw new AssertionError(one + two);
		}
		System.out.println("PASS");
	}
}
