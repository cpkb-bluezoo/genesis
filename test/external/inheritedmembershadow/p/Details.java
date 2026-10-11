package p;

import java.util.stream.Stream;

public interface Details {

    Stream<Entry> stream();

    record Entry(String name, Object value) {
    }

}
