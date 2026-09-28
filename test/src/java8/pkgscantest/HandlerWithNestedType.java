package pkgscantest;

public interface HandlerWithNestedType {
    enum Type { A, B }
    Type getType();
}
