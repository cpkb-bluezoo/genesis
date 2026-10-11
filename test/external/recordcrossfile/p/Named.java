package p;

public record Named(String name, int size) {
    public Named {
        if (size < 0) {
            throw new IllegalArgumentException("size");
        }
    }

    @Override
    public String name() {
        return name.toUpperCase();
    }
}
