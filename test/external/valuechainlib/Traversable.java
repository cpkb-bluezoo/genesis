package valuechainlib;

import java.util.Iterator;

public interface Traversable<T> extends Iterable<T> {
    default Option<T> headOption() {
        Iterator<T> it = iterator();
        return it.hasNext() ? Option.of(it.next()) : Option.<T>none();
    }
}
