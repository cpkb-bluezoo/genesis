package nestedsiblinglib;

public interface Responses {
    interface Headers<B extends Headers<B>> {
        B header(String name);

        String build();
    }

    interface Body extends Headers<Body> {
        Body contentType(String type);
    }

    static Body status(int code) {
        return new Body() {
            private final StringBuilder out = new StringBuilder("status " + code);

            public Body header(String name) {
                this.out.append(" header ").append(name);
                return this;
            }

            public Body contentType(String type) {
                this.out.append(" type ").append(type);
                return this;
            }

            public String build() {
                return this.out.toString();
            }
        };
    }
}
