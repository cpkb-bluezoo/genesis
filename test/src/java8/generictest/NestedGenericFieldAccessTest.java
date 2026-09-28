package generictest;

import java.util.ArrayList;
import java.util.List;

/*
 * Regression test: a chained field access through a generic method call
 * result - "holder.items.get(0).value" - used to fail to resolve the
 * field's type when the field's own type (Box<T>'s "T value") needed
 * substitution from a doubly-nested generic ("List<Box<byte[]>>"). The
 * same expression split across two statements (assigning the list.get()
 * result to a locally-declared "Box<byte[]>" variable first) worked fine,
 * which is what made this so easy to miss: the declared-variable path
 * parses its type independently from source text, while the chained
 * access path depends on substitution correctly propagating nested type
 * arguments through the method call's return type. See genesis history
 * for details (search "type_is_under_parameterized" in semantic.c).
 */
public class NestedGenericFieldAccessTest {

    private static class Holder {
        final List<Box<byte[]>> items = new ArrayList<>();
    }

    private static class Box<T> {
        final T value;
        Box(T value) {
            this.value = value;
        }
    }

    void run() {
        Holder h = new Holder();
        h.items.add(new Box<>(new byte[] { 1, 2, 3 }));
        byte[] parsed = h.items.get(0).value;
    }
}
