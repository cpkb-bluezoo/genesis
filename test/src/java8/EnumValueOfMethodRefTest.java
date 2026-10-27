import java.util.Optional;
import java.util.function.Function;

public class EnumValueOfMethodRefTest {
    enum Color { RED, GREEN }

    // The result of a generic call that is not used has no target type to bind the reference to
    static void discarded(Optional<String> name, String route) {
        name.map(Color::valueOf);
        Optional.ofNullable(route).map(String::toUpperCase).map(Color::valueOf);
    }

    // The valueOf(String) of an enum is implicit, yet a method reference may name it (JLS 8.9.3).
    public static void main(String[] args) {
        Function<String, Color> f = Color::valueOf;
        if (f.apply("GREEN") != Color.GREEN) throw new AssertionError();

        Optional<String> name = Optional.of("RED");
        if (name.map(Color::valueOf).get() != Color.RED) throw new AssertionError();

        Color c = Optional.ofNullable("green").map(String::toUpperCase).map(Color::valueOf).get();
        if (c != Color.GREEN) throw new AssertionError();

        discarded(name, "RED");

        java.util.function.Supplier<Color[]> all = Color::values;
        if (all.get().length != 2) throw new AssertionError();

        Function<Color, String> n = Color::name;
        if (!"RED".equals(n.apply(Color.RED))) throw new AssertionError();
        System.out.println("ok");
    }
}
