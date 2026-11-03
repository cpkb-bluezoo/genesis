package valuechainlib;

import java.util.Arrays;
import java.util.Iterator;

public final class Seq<T> implements Traversable<T> {
    private final T[] items;

    @SafeVarargs
    private Seq(T... items) {
        this.items = items;
    }

    @SafeVarargs
    public static <T> Seq<T> of(T... items) {
        return new Seq<T>(items);
    }

    public Iterator<T> iterator() {
        return Arrays.asList(items).iterator();
    }
}
