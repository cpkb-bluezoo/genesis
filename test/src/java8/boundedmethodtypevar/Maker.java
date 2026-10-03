package boundedmethodtypevar;

import java.lang.annotation.Annotation;
import java.util.Map;

public class Maker {
    interface Named {
        String name();
    }

    /* The return type "A" erases to its BOUND, Annotation. */
    static <A extends Annotation> A annotation(Class<A> type, Map<String, Object> values) {
        return null;
    }

    static <N extends Comparable<N>> N pick(N a, N b) {
        return a.compareTo(b) >= 0 ? a : b;
    }
}
