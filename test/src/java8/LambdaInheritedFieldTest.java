import java.util.function.IntSupplier;
import java.util.function.Supplier;

/*
 * A lambda that reads or assigns a field the class inherits from a superclass captures 'this'
 * (the field is read through it), exactly as for a field the class declares itself.
 */
public class LambdaInheritedFieldTest {
    static class Base {
        protected String session = "abc";
        protected int count = 1;
    }

    static class Mid extends Base {
    }

    static class Child extends Mid {
        IntSupplier length() {
            return () -> session.length();
        }

        Supplier<String> field() {
            return () -> session;
        }

        Runnable assign(String value) {
            return () -> session = value;
        }

        Runnable update() {
            return () -> {
                count++;
                count += 4;
                --count;
                session += "!";
            };
        }

        Supplier<Supplier<String>> nested() {
            return () -> () -> session + count;
        }

        Supplier<String> viaAnonymous() {
            return () -> new Supplier<String>() {
                public String get() {
                    return session;
                }
            }.get();
        }

        class Inner {
            Supplier<String> read() {
                return () -> session;
            }
        }
    }

    public static void main(String[] args) {
        Child c = new Child();
        if (c.length().getAsInt() != 3 || !c.field().get().equals("abc")) {
            throw new AssertionError("read");
        }
        c.assign("xy").run();
        if (!c.session.equals("xy") || c.length().getAsInt() != 2) {
            throw new AssertionError("assign " + c.session);
        }
        c.update().run();
        if (c.count != 5 || !c.session.equals("xy!")) {
            throw new AssertionError("update " + c.count + " " + c.session);
        }
        if (!c.nested().get().get().equals("xy!5") || !c.viaAnonymous().get().equals("xy!")) {
            throw new AssertionError("nested");
        }
        if (!c.new Inner().read().get().equals("xy!")) {
            throw new AssertionError("inner");
        }
        System.out.println("LambdaInheritedFieldTest passed!");
    }
}
