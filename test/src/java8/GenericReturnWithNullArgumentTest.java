import java.util.HashMap;
import java.util.Map;

public class GenericReturnWithNullArgumentTest {
	private <T> T lookup(Map<String, T> values, String id, T fallback) {
		T value = values.get(id);
		return (value != null) ? value : fallback;
	}

	static double sum(double... values) {
		double total = 0;
		for (double value : values) {
			total += value;
		}
		return total;
	}

	static int length(String[][] grid) {
		return grid.length;
	}

	public static void main(String[] args) {
		Map<String, double[]> numbers = new HashMap<String, double[]>();
		numbers.put("a", new double[] { 0.5, 0.25 });
		Map<String, String[][]> grids = new HashMap<String, String[][]>();
		grids.put("g", new String[][] { { "x" }, { "y" } });
		GenericReturnWithNullArgumentTest test = new GenericReturnWithNullArgumentTest();
		double total = sum(test.lookup(numbers, "a", null));
		int rows = length(test.lookup(grids, "g", null));
		double missing = sum(test.lookup(numbers, "none", new double[] { 1 }));
		if (total != 0.75 || rows != 2 || missing != 1) {
			throw new AssertionError(total + " " + rows + " " + missing);
		}
		System.out.println("PASS");
	}
}
