package p;

import java.util.Collection;

public interface Exposable<O extends Operation> {
	String getId();

	Collection<O> getOperations();
}
