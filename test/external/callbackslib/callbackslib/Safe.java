package callbackslib;

import java.util.Collection;
import java.util.function.Consumer;

/* Class-file side of a LambdaSafe-like helper: a generic factory method returning
 * a generic nested class whose own method takes a Consumer of the class's type
 * variable. */
public final class Safe {
    private Safe() {
    }

    public static <C, A> Callbacks<C, A> callbacks(Class<C> type, Collection<? extends C> instances, A argument) {
        return new Callbacks<C, A>(instances);
    }

    public static final class Callbacks<C, A> extends Base<C, A, Callbacks<C, A>> {
        Callbacks(Collection<? extends C> instances) {
            super(instances);
        }

        public void invoke(Consumer<C> invoker) {
            for (C instance : this.instances) {
                invoker.accept(instance);
            }
        }
    }

    abstract static class Base<C, A, SELF extends Base<C, A, SELF>> {
        final Collection<? extends C> instances;

        Base(Collection<? extends C> instances) {
            this.instances = instances;
        }
    }
}
