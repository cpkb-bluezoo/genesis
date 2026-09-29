package crossfilegenericmethodchain;

import java.util.ArrayList;
import java.util.List;

/*
 * Companion classes for CrossFileGenericMethodChainVerifyTest - see that
 * file for the bug this pair reproduces. Must be compiled together with
 * the test file in a single genesis invocation (as javac/genesis do for
 * a whole source tree) to trigger it: resolving find()'s own generic
 * parameter type from a DIFFERENT file than the one it's declared in is
 * exactly what exposes the bug.
 */
abstract class Item {
}

final class Widget extends Item {
    private final int value;

    Widget(int value) {
        this.value = value;
    }

    int getValue() {
        return value;
    }
}

public class Registry {

    private final List<Object> entries = new ArrayList<Object>();

    public void add(Object entry) {
        entries.add(entry);
    }

    /** Everything in the registry of the given type, in order - mirrors
     * gumdrop's ClientHarness.sent(Class&lt;T&gt;), whose parameter type
     * (Class&lt;T&gt;) is itself parameterized by the method's own type
     * variable T, not the class's. */
    public <T extends Item> List<T> find(Class<T> type) {
        List<T> result = new ArrayList<T>();
        for (Object entry : entries) {
            if (type.isInstance(entry)) {
                result.add(type.cast(entry));
            }
        }
        return result;
    }
}
