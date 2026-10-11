import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Spliterator;
import java.util.stream.StreamSupport;

public class ImplementedGenericInterfaceMethodTest {
	interface Init {
		void onStartup(StringBuilder context);
	}

	interface Source {
		void onStartup(StringBuilder context);
	}

	static final class Sources implements Iterable<Source> {
		final List<Source> list = new ArrayList<Source>();

		public Iterator<Source> iterator() {
			return list.iterator();
		}
	}

	static class Names extends ArrayList<String> {
	}

	static Init[] inits(Sources sources) {
		return StreamSupport.stream(sources.spliterator(), false)
			.map((source) -> (Init) source::onStartup)
			.toArray(Init[]::new);
	}

	public static void main(String[] args) {
		Sources sources = new Sources();
		sources.list.add((context) -> context.append("a"));
		sources.list.add((context) -> context.append("b"));
		StringBuilder sb = new StringBuilder();
		for (Init init : inits(sources)) {
			init.onStartup(sb);
		}
		Spliterator<Source> spliterator = sources.spliterator();
		Names names = new Names();
		names.add("x");
		String first = names.stream().findFirst().get();
		if (!"ab".equals(sb.toString()) || spliterator == null || !"x".equals(first)) {
			throw new AssertionError(sb);
		}
		System.out.println("PASS");
	}
}
