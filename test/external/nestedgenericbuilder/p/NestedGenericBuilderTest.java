package p;

public class NestedGenericBuilderTest {
    static class Impl implements Cust.Builder<String> {
        boolean nested;

        public Cust.Builder<String> nested(boolean nested) {
            this.nested = nested;
            return this;
        }

        public Cust<String> build() {
            return (s) -> (nested ? "nested " : "") + s;
        }
    }

    static Object chain(Cust.Builder<?> builder) {
        return builder.nested().build();
    }

    public static void main(String[] args) {
        Cust<String> c = new Impl().nested().build();
        if (!"nested x".equals(c.describe("x")) || chain(new Impl()) == null) {
            throw new RuntimeException("nested generic builder");
        }
    }
}
