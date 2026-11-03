package varboundrefslib;

/*
 * Compiled with javac (see run-tests.sh): a class from a dependency whose
 * factory method returns an instance used through a "var" local.
 */
public class Factory {
    public final String name;
    public final StringBuilder seen = new StringBuilder();

    public Factory(String name) {
        this.name = name;
    }

    public static Factory create(String name) {
        return new Factory(name);
    }

    public void add(String key, String value) {
        seen.append(name).append(' ').append(key).append('=').append(value);
    }
}
