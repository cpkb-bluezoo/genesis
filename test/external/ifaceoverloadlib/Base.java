package ifaceoverloadlib;

public interface Base {
    interface Callback<T> {
        void done(T value);
    }

    default String ask(String name, Callback<String> callback) {
        return "base2";
    }
}
