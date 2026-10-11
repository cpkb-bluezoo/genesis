package iplib;

public class Configurer<H> {
    public class Registry {
        public String name() {
            return "registry";
        }
    }

    public static class Http {
        public Http authorize(java.util.function.Consumer<Configurer<Http>.Registry> customizer) {
            customizer.accept(new Configurer<Http>().new Registry());
            return this;
        }
    }
}
