import static java.util.concurrent.TimeUnit.SECONDS;

public class StaticImportEnumMethodCallTest {
    static long readLong(String key, Long fallback) {
        return fallback;
    }

    public static void main(String[] args) {
        long direct = SECONDS.toMillis(30);
        String name = SECONDS.name();
        long boxed = readLong("interval", SECONDS.toMillis(30));
        if (direct != 30000L || boxed != 30000L || !"SECONDS".equals(name)) {
            throw new AssertionError(direct + " " + boxed + " " + name);
        }
        System.out.println("StaticImportEnumMethodCallTest passed!");
    }
}
