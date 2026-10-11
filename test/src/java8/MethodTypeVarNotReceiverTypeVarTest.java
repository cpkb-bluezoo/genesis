/* Regression test: a generic method's own type variable is not the receiver
 * class's, whatever the two are called - in "Base<T> { <E extends Throwable> E
 * findCause(Throwable, Class<E>) }" called on a "Base<IllegalStateException>",
 * E was taken for IllegalStateException (Spring Boot's FailureAnalyzers). */
abstract class FailureBase<T extends Throwable> {
    protected <E extends Throwable> E findCause(Throwable failure, Class<E> type) {
        for (Throwable t = failure; t != null; t = t.getCause()) {
            if (type.isInstance(t)) {
                return type.cast(t);
            }
        }
        return null;
    }
}

public class MethodTypeVarNotReceiverTypeVarTest extends FailureBase<IllegalStateException> {
    String run(Throwable failure) {
        IllegalArgumentException found = findCause(failure, IllegalArgumentException.class);
        return found == null ? "none" : found.getMessage();
    }

    public static void main(String[] args) {
        MethodTypeVarNotReceiverTypeVarTest t = new MethodTypeVarNotReceiverTypeVarTest();
        Throwable failure = new IllegalStateException("outer", new IllegalArgumentException("inner"));
        if (!"inner".equals(t.run(failure)) || !"none".equals(t.run(new RuntimeException()))) {
            throw new RuntimeException("findCause");
        }
        System.out.println("MethodTypeVarNotReceiverTypeVarTest passed!");
    }
}
