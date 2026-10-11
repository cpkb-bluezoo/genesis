package p;

import java.util.Collection;

public interface Endpoints<E extends Exposable<?>> {
	Collection<E> getEndpoints();
}
