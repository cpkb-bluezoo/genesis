package selftypelib;

/* "<T extends B> T baseUrl(..)": the result is a variable bounded by another variable of the
 * class, which is itself bounded by the interface. */
public interface Fluent<B extends Fluent<B>> {
    <T extends B> T named(String name);

    String build();

    static Fluent<?> create() {
        return new Impl();
    }

    final class Impl implements Fluent<Impl> {
        private String name = "";

        @SuppressWarnings("unchecked")
        public <T extends Impl> T named(String value) {
            this.name = value;
            return (T) this;
        }

        public String build() {
            return "built " + this.name;
        }
    }
}
