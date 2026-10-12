/**
 * An empty synchronized body leaves the handler's protected range empty, which the class file
 * format rejects ("Illegal exception table range"); the range has to cover the monitor release.
 */
public class SynchronizedEmptyBodyTest {

    static int counter;

    static void plain(Object lock) {
        synchronized (lock) {
        }
    }

    public static void main(String[] args) throws Exception {
        Object o = new Object();
        Thread t = new Thread(() -> {
            synchronized (o) {
                // Intentionally empty
            }
            counter++;
        });
        t.start();
        t.join();
        plain(o);
        synchronized (o) {
            synchronized (o) {
            }
        }
        try {
        } finally {
            counter++;
        }
        if (counter != 2) {
            throw new AssertionError("counter " + counter);
        }
        System.out.println("PASS");
    }
}
