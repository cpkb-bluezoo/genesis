package p;
import java.util.function.BiFunction;
public abstract class Registry<C, E> {
    final BiFunction<String, C, E> factory;
    protected Registry(BiFunction<String, C, E> factory) { this.factory = factory; }
    public E make(String name, C contributor) { return factory.apply(name, contributor); }
}
