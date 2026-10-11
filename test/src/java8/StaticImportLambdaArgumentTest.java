import static java.util.Collections.sort;
import static java.util.concurrent.CompletableFuture.supplyAsync;

import java.util.ArrayList;
import java.util.List;

public class StaticImportLambdaArgumentTest {
	public static void main(String[] args) throws Exception {
		List<String> words = new ArrayList<String>();
		words.add("ccc");
		words.add("a");
		words.add("bb");
		sort(words, (left, right) -> left.length() - right.length());
		StringBuilder out = new StringBuilder();
		for (String word : words) {
			out.append(word.toUpperCase()).append(';');
		}
		int length = supplyAsync(() -> out.toString()).get().length();
		if (!"A;BB;CCC;".equals(out.toString()) || length != 9) {
			throw new AssertionError(out.toString());
		}
		System.out.println("PASS");
	}
}
