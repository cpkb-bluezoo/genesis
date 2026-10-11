package p;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class Convs implements Iterable<Conv<?>> {
	private final List<Conv<?>> list = new ArrayList<Conv<?>>();

	public void add(Conv<?> conv) {
		this.list.add(conv);
	}

	@Override
	public Iterator<Conv<?>> iterator() {
		return this.list.iterator();
	}
}
