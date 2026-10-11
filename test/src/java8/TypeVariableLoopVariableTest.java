import java.util.Arrays;
import java.util.Collection;
import java.util.List;

public class TypeVariableLoopVariableTest {
	static class Holder<C, N extends Number & Comparable<N>> {
		private final Collection<? extends C> items;
		private final List<N> numbers;

		Holder(Collection<? extends C> items, List<N> numbers) {
			this.items = items;
			this.numbers = numbers;
		}

		String joined() {
			StringBuilder sb = new StringBuilder();
			for (C item : this.items) {
				sb.append(item).append(',');
			}
			return sb.toString();
		}

		double total() {
			double total = 0;
			for (N number : this.numbers) {
				total += number.doubleValue();
			}
			return total;
		}
	}

	static <E> int count(Iterable<E> iterable) {
		int n = 0;
		for (E element : iterable) {
			n++;
		}
		return n;
	}

	public static void main(String[] args) {
		Holder<String, Integer> holder = new Holder<String, Integer>(Arrays.asList("a", "b"), Arrays.asList(1, 2, 3));
		if (!"a,b,".equals(holder.joined()) || holder.total() != 6 || count(Arrays.asList(1, 2)) != 2) {
			throw new AssertionError("wrong");
		}
		System.out.println("PASS");
	}
}
