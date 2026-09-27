package java8;

import java.util.List;

/** Host type for cross-file anonymous {@code Callback} tests. */
public class NestedCallbackHost {

    public interface Callback<T> {
        void completed(T result);
        void failed(Throwable error);
    }

    public static void run(Callback<List<String>> callback) {
        callback.completed(java.util.Collections.<String>emptyList());
    }
}
