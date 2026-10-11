import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.Objects.requireNonNull;
import static java.util.Optional.ofNullable;

import java.util.List;
import java.util.Optional;

/* Regression test: a generic method named through a static import is typed like
 * the same call written with its class ("Runnable r = mock(Runnable.class)" with
 * Mockito.mock imported): its type variable is inferred from the arguments and
 * the target, not left as T. */
public class StaticImportGenericMethodTest {
    static int count;

    public static void main(String[] args) {
        Runnable r = requireNonNull((Runnable) () -> count++);
        r.run();
        List<String> none = emptyList();
        List<String> one = singletonList("a");
        String first = one.get(0);
        Optional<String> o = ofNullable(first);
        int length = requireNonNull(first).length();
        if (count != 1 || !none.isEmpty() || !"a".equals(first) || !o.isPresent()
                || length != 1) {
            throw new RuntimeException("static import generics");
        }
        System.out.println("StaticImportGenericMethodTest passed!");
    }
}
