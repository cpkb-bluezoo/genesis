import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

public class LambdaParameterOfEnclosingTypeVariableTest {
	static <T extends Number> double total(List<T> values) {
		Function<T, Double> asDouble = t -> t.doubleValue();
		Function<T, Integer> asInt = t -> t.intValue();
		Function<T, Number> same = t -> t;
		Function<T, String> text = t -> t.toString();
		double sum = 0;
		for (T value : values) {
			sum += asDouble.apply(value) + asInt.apply(value) + same.apply(value).longValue() + text.apply(value).length();
		}
		return sum;
	}

	public static void main(String[] args) {
		List<Integer> values = new ArrayList<Integer>();
		values.add(10);
		values.add(5);
		double total = total(values);
		if (total != 10 + 10 + 10 + 2 + 5 + 5 + 5 + 1) {
			throw new AssertionError(total);
		}
		System.out.println("PASS");
	}
}
