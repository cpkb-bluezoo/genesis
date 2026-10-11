import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class GenericCallArgumentFixedByParameterTest {
	interface Importer {
		Map<String, Integer> load(String context, List<String> locations, Object extra);

		Map<String, Integer> load(String context);
	}

	static class Stub<T> {
		T value;

		Stub<T> willReturn(T value) {
			this.value = value;
			return this;
		}
	}

	static <T> T anything() {
		return null;
	}

	static <T> Stub<T> given(T call) {
		return new Stub<T>();
	}

	static <T> T eq(T value) {
		return value;
	}

	public static void main(String[] args) {
		final Map<String, Integer> result = new HashMap<String, Integer>();
		result.put("a", 1);
		Importer importer = new Importer() {
			public Map<String, Integer> load(String context, List<String> locations, Object extra) {
				return result;
			}

			public Map<String, Integer> load(String context) {
				return null;
			}
		};
		Map<String, Integer> direct = importer.load("x", anything(), null);
		Stub<Map<String, Integer>> stub = given(importer.load("x", anything(), anything()));
		stub.willReturn(result);
		given(importer.load(eq("x"), anything(), null)).willReturn(result);
		List<String> locations = new ArrayList<String>();
		given(importer.load(eq("x"), eq(locations), anything())).willReturn(result);
		if (direct != result || stub.value != result) {
			throw new AssertionError("wrong result");
		}
		System.out.println("PASS");
	}
}
