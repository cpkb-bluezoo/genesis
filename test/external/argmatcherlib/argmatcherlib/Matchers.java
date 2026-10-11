package argmatcherlib;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

public final class Matchers {
    private static final List<Object> SEEN = new ArrayList<Object>();

    private Matchers() {
    }

    /* "<T> T assertArg(Consumer<T>)" shaped: the lambda's parameter is typed by what the call is
     * passed for */
    public static <T> T arg(Consumer<T> check) {
        SEEN.add(check);
        return null;
    }

    @SuppressWarnings("unchecked")
    public static <T> void feed(T value) {
        ((Consumer<T>) SEEN.get(SEEN.size() - 1)).accept(value);
    }

    public static <A> Holder<A> that(A actual) {
        return new Holder<A>(actual);
    }

    /* the overloads that a call made with an untyped argument must not be drawn to */
    public static <E> ListHolder<E> that(List<? extends E> actual) {
        return new ListHolder<E>(actual);
    }

    public static <A extends Comparable<? super A>> ComparableHolder<A> that(A actual) {
        return new ComparableHolder<A>();
    }

    public static <A extends Throwable> ThrowableHolder<A> that(A actual) {
        return new ThrowableHolder<A>();
    }

    public static final class ListHolder<E> {
        ListHolder(List<? extends E> actual) {
        }
    }

    public static final class ComparableHolder<A> {
    }

    public static final class ThrowableHolder<A> {
    }

    public static final class Holder<A> {
        private final A actual;

        Holder(A actual) {
            this.actual = actual;
        }

        public <S> Holder<A> satisfying(Class<S> type, Consumer<S> requirements) {
            requirements.accept(type.cast(this.actual));
            return this;
        }
    }
}
