import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;

/*
 * A method reference whose receiver is a field the class inherits ("extra::add"), passed to
 * a stream's forEach: the name is a variable (JLS 6.4.2), not a type, so the reference is a
 * bound one and add(E) fits the Consumer the method takes.
 */
public class InheritedFieldMethodRefStreamTest {
    static class Base {
        protected List<String> extra = new ArrayList<>();
    }

    static class Demo extends Base {
        List<String> own = new ArrayList<>();

        void viaStream(List<String> items) {
            items.stream().forEach(extra::add);
        }

        void viaMappedStream(List<String> items) {
            items.stream().map(String::trim).forEach(extra::add);
        }

        void ownField(List<String> items) {
            items.stream().forEach(own::add);
        }

        void direct(List<String> items) {
            items.forEach(extra::add);
        }

        void asConsumer() {
            Consumer<String> c = extra::add;
            c.accept("e");
        }
    }

    public static void main(String[] args) {
        Demo demo = new Demo();
        demo.viaStream(Arrays.asList("a", "b"));
        demo.viaMappedStream(Arrays.asList(" c "));
        demo.ownField(Arrays.asList("o"));
        demo.direct(Arrays.asList("d"));
        demo.asConsumer();
        if (!demo.extra.equals(Arrays.asList("a", "b", "c", "d", "e")) || !demo.own.equals(Arrays.asList("o"))) {
            throw new AssertionError(demo.extra + " " + demo.own);
        }
        System.out.println("InheritedFieldMethodRefStreamTest passed!");
    }
}
