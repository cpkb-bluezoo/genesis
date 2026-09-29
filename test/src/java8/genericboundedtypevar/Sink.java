package genericboundedtypevar;

/**
 * Regression fixture: an interface with a BOUNDED type variable
 * (T extends Enum<T>), erasing to Enum rather than Object.
 * See GenericBoundedTypeVarErasureVerifyTest.
 */
public interface Sink<T extends Enum<T>> {
    boolean token(T type, int n);
}
