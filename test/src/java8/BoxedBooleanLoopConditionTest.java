import java.util.function.Function;
import java.util.function.Supplier;

public class BoxedBooleanLoopConditionTest {
	public static void main(String[] args) {
		final int[] left = { 3 };
		Function<Integer, Boolean> more = (n) -> left[0] > 0;
		int whileRuns = 0;
		while (more.apply(left[0])) {
			left[0]--;
			whileRuns++;
		}
		Supplier<Boolean> again = () -> left[0] < 2;
		int doRuns = 0;
		do {
			left[0]++;
			doRuns++;
		} while (again.get());
		int forRuns = 0;
		for (int i = 0; Boolean.valueOf(i < 4); i++) {
			forRuns++;
		}
		Boolean flag = Boolean.TRUE;
		int ternary = flag ? 1 : 0;
		if (whileRuns != 3 || doRuns != 2 || forRuns != 4 || ternary != 1) {
			throw new AssertionError(whileRuns + " " + doRuns + " " + forRuns);
		}
		System.out.println("PASS");
	}
}
