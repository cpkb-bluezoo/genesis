import java.util.function.Function;
import java.util.function.Supplier;

public class ParenthesizedLambdaReturnTest {
	interface Customizer {
		void customize(StringBuilder builder);
	}

	Customizer parenthesizedReturn() {
		return ((builder) -> builder.append("p"));
	}

	Customizer plainReturn() {
		return (builder) -> builder.append("q");
	}

	Function<String, Integer> doubleParenthesized() {
		return (((text) -> text.length()));
	}

	Supplier<String> methodRef() {
		return (String::new);
	}

	public static void main(String[] args) {
		ParenthesizedLambdaReturnTest test = new ParenthesizedLambdaReturnTest();
		StringBuilder sb = new StringBuilder();
		test.parenthesizedReturn().customize(sb);
		test.plainReturn().customize(sb);
		Customizer local = ((builder) -> builder.append("l"));
		local.customize(sb);
		if (!"pql".equals(sb.toString()) || test.doubleParenthesized().apply("four") != 4
				|| !test.methodRef().get().isEmpty()) {
			throw new AssertionError(sb);
		}
		System.out.println("PASS");
	}
}
