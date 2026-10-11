package p;

public class Types {
    public record Person(String name, int age) {
    }

    public enum Kind {
        ONE, TWO
    }

    public interface Named {
        String name();
    }
}
