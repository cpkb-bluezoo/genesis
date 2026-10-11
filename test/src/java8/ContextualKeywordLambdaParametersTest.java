import java.util.function.BiFunction;
import java.util.function.BinaryOperator;

public class ContextualKeywordLambdaParametersTest {
	static BinaryOperator<String> first() {
		return (record, consumer) -> record;
	}

	static BinaryOperator<String> second() {
		return (open, record) -> open + record;
	}

	static BiFunction<String, Integer, String> typed() {
		return (String record, Integer permits) -> record + permits;
	}

	static BinaryOperator<String> declared() {
		BinaryOperator<String> sealed = (module, with) -> module + with;
		return sealed;
	}

	public static void main(String[] args) {
		String result = first().apply("a", "b") + second().apply("c", "d") + typed().apply("e", 1)
				+ declared().apply("f", "g");
		if (!"acde1fg".equals(result.replace("ab", "a").replace("cd", "cd"))) {
			throw new AssertionError(result);
		}
		System.out.println("PASS");
	}
}
