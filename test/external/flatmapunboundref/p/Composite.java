package p;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

public class Composite implements Items {
	private final List<Items> parts;

	public Composite(List<Items> parts) {
		this.parts = parts;
	}

	@Override
	public Stream<Entry> stream() {
		Set<String> seen = new HashSet<>();
		return this.parts.stream()
			.flatMap(Items::stream)
			.filter((element) -> seen.add(element.name()));
	}
}
