import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Predicate;

/*
 * Regression test: a method returning the self type of a fluent assertion API ("SELF extends
 * Base<SELF, E, EA>") on a receiver whose SELF is a wildcard gave Base<?, ?, ?>, losing E and
 * EA. The element of "check(list).filter(..).single()" was then an Object and a lambda
 * parameter calling a String method on it failed verification (AssertJ's
 * "assertThat(set).filteredOn(..).singleElement().satisfies(..)").
 */
public class WildcardSelfTypeKeepsOtherArgumentsTest {

    static class Base<SELF extends Base<SELF, E, EA>, E, EA extends Obj<E>> {
        final List<E> items;

        Base(List<E> items) {
            this.items = items;
        }

        @SuppressWarnings("unchecked")
        SELF filter(Predicate<? super E> predicate) {
            List<E> kept = new ArrayList<E>();
            for (E item : items) {
                if (predicate.test(item)) {
                    kept.add(item);
                }
            }
            return (SELF) new Base<SELF, E, EA>(kept);
        }

        @SuppressWarnings("unchecked")
        EA single() {
            return (EA) new Obj<E>(items.get(0));
        }
    }

    static class Obj<E> {
        final E value;

        Obj(E value) {
            this.value = value;
        }

        Obj<E> satisfies(Consumer<? super E> requirements) {
            requirements.accept(value);
            return this;
        }
    }

    static <E> Base<?, E, Obj<E>> check(List<E> items) {
        return new Base<>(items);
    }

    static int seen;

    public static void main(String[] args) {
        List<String> names = new ArrayList<String>();
        names.add("a");
        names.add("keep");
        check(names).filter((name) -> name.length() > 1).single().satisfies((name) -> seen = name.length());
        if (seen != 4) {
            throw new RuntimeException("FAIL: " + seen);
        }
        System.out.println("PASS");
    }
}
