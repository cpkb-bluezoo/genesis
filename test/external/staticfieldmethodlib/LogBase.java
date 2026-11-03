package staticfieldmethodlib;

/*
 * Compiled with javac (see run-tests.sh): a base class declaring a public
 * static field and an instance method with the same name.
 */
public class LogBase {
    public static final Note log = new Note();

    public void log() {
    }
}
