package ifacememberlib;

public interface Store {
    /** Declared in an interface: implicitly public and static (JLS 9.5). */
    class Result {
        public final int code;
        public final String name;

        public Result(int code, String name) {
            this.code = code;
            this.name = name;
        }
    }

    Result fetch(String key);
}
