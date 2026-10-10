package ifaceoverloadlib;

public interface Api extends Base {
    default String ask(String name) {
        return "api1";
    }

    @Override
    default String ask(String name, Callback<String> callback) {
        return "api2";
    }

    default String ask(String name, Callback<String> callback, int flags) {
        return "api3";
    }

    default String ask(int a, int b) {
        return "apiII";
    }
}
