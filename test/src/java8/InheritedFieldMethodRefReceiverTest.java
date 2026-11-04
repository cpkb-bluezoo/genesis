import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

/*
 * A method reference whose receiver is a field the class inherits
 * ("session::associate") is a bound reference on that field, not a type name.
 */
public class InheritedFieldMethodRefReceiverTest {
    static class Item {
        final String name;

        Item(String name) {
            this.name = name;
        }
    }

    static class Session {
        final List<String> seen = new ArrayList<>();

        void associate(Item item) {
            seen.add(item.name);
        }

        String label() {
            return "s" + seen.size();
        }
    }

    static class Base {
        protected Session session = new Session();
    }

    static class Child extends Base {
        String go(List<Item> items) {
            items.forEach(session::associate);
            Supplier<String> label = session::label;
            return label.get() + String.join(",", session.seen);
        }
    }

    public static void main(String[] args) {
        String got = new Child().go(List.of(new Item("a"), new Item("b")));
        if (!got.equals("s2a,b")) {
            throw new AssertionError(got);
        }
        System.out.println("InheritedFieldMethodRefReceiverTest passed!");
    }
}
