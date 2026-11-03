package valuechainlib;

import java.util.Optional;

/*
 * Compiled with javac (see run-tests.sh): a generic interface whose default
 * method returns a type built from its own type variable, as a functional
 * library's base type does.
 */
public interface Value<T> {
    boolean isEmpty();

    T get();

    default Optional<T> toJavaOptional() {
        return isEmpty() ? Optional.<T>empty() : Optional.ofNullable(get());
    }
}
