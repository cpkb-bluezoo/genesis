/*
 * Switches that cover every value of their selector without a default: sealed hierarchies,
 * enum constants, and record patterns that cover their components between them.
 */
public class SwitchExhaustiveTest {

    sealed interface Shape permits Circle, Square, Group {}
    record Circle(int r) implements Shape {}
    record Square(int side) implements Shape {}
    sealed interface Group extends Shape permits Pair, Trio {}
    record Pair(Shape a, Shape b) implements Group {}
    record Trio(Shape a, Shape b, Shape c) implements Group {}

    /* no permits clause: the classes of this file that extend it */
    sealed interface Token {}
    record Word(String text) implements Token {}
    record Number(int value) implements Token {}
    enum Symbol implements Token { PLUS, MINUS }

    /* a sealed class that is not abstract is a value itself */
    static sealed class Animal permits Dog, Cat {}
    static final class Dog extends Animal {}
    static non-sealed class Cat extends Animal {}

    sealed interface Flag permits On, Off {}
    record On() implements Flag {}
    record Off() implements Flag {}
    record Both(Flag first, Flag second) {}

    enum Colour { RED, GREEN, BLUE }

    static void check(boolean ok, String what) {
        if (!ok) {
            System.out.println("FAILED: " + what);
            System.exit(1);
        }
    }

    static int area(Shape s) {
        return switch (s) {
            case Circle c -> 3 * c.r() * c.r();
            case Square q -> q.side() * q.side();
            case Group g -> 0;
        };
    }

    static int leaves(Shape s) {
        return switch (s) {
            case Circle c -> 1;
            case Square q -> 1;
            case Pair(var a, var b) -> leaves(a) + leaves(b);
            case Trio(var a, var b, var c) -> leaves(a) + leaves(b) + leaves(c);
        };
    }

    static String token(Token t) {
        return switch (t) {
            case Word w -> "word";
            case Number n -> "number";
            case Symbol sy -> "symbol";
        };
    }

    static String animal(Animal a) {
        return switch (a) {
            case Dog d -> "dog";
            case Cat c -> "cat";
            case Animal x -> "animal";
        };
    }

    static String colour(Colour c) {
        return switch (c) {
            case RED -> "r";
            case GREEN -> "g";
            case BLUE -> "b";
        };
    }

    static String both(Both b) {
        return switch (b) {
            case Both(On x, On y) -> "on on";
            case Both(On x, Off y) -> "on off";
            case Both(Off x, var y) -> "off";
        };
    }

    static String guarded(Object o) {
        return switch (o) {
            case String s when s.isEmpty() -> "empty";
            case String s -> "string";
            case Object x -> "object";
        };
    }

    static String withNull(Flag f) {
        return switch (f) {
            case On o -> "on";
            case null, default -> "other";
        };
    }

    static String statement(Shape s) {
        String result = "";
        switch (s) {
            case Circle c -> result = "circle";
            case Square q -> result = "square";
            case Group g -> result = "group";
        }
        return result;
    }

    static int plain(int i) {
        return switch (i) {
            case 1 -> 10;
            case 2 -> 20;
            default -> 0;
        };
    }

    public static void main(String[] args) {
        check(area(new Circle(2)) == 12, "sealed interface");
        check(leaves(new Pair(new Circle(1), new Trio(new Square(1), new Circle(1), new Square(2)))) == 4,
              "records under a sealed interface");
        check("word".equals(token(new Word("x"))) && "symbol".equals(token(Symbol.PLUS)), "no permits clause");
        check("dog".equals(animal(new Dog())) && "animal".equals(animal(new Animal())), "sealed class itself");
        check("g".equals(colour(Colour.GREEN)), "every enum constant");
        check("on off".equals(both(new Both(new On(), new Off()))), "record pattern rows");
        check("off".equals(both(new Both(new Off(), new On()))), "record pattern, covering component");
        check("empty".equals(guarded("")) && "object".equals(guarded(1)), "guards with a total case");
        check("other".equals(withNull(new Off())), "case null, default");
        check("square".equals(statement(new Square(1))), "statement over patterns");
        check(plain(2) == 20, "plain switch with default");
    }
}
