package p;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * A lambda passed to a generic method of a class declared in another file of
 * the same compilation: its parameter is typed from the receiver's type
 * arguments, however the two files are ordered.
 */
public class StubLambdaParamTest {

	public static void main(String[] args) {
		Holder<List<String>> holder = new Holder<List<String>>(Arrays.asList("a", "b"));
		List<String> result = new ArrayList<String>();
		holder.ifPresent((includes) -> { result.addAll(includes); });
		holder.each((again) -> { result.addAll(again); });
		if (result.size() != 4) {
			throw new AssertionError("expected 4 elements, got " + result);
		}
		System.out.println("PASS");
	}
}
