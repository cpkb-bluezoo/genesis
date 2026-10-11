package p;

import java.util.Iterator;
import java.util.stream.Stream;

public interface Items extends Iterable<Items.Entry> {
	@Override
	default Iterator<Entry> iterator() {
		return stream().iterator();
	}

	Stream<Entry> stream();

	record Entry(String name, int size) {
	}
}
