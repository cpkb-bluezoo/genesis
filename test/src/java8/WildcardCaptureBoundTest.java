/* Regression test: a member of "Holder<?>" has the type the parameter is
 * declared to extend ("Holder<T extends Runnable>.get()" is a Runnable), not "?" -
 * Spring Boot's ServletRegistrationBean<?>.getServlet(). */
public class WildcardCaptureBoundTest {
    static class Holder<T extends Runnable> {
        final T value;

        Holder(T value) {
            this.value = value;
        }

        T get() {
            return value;
        }
    }

    static int run(Holder<?> holder) {
        Runnable r = holder.get();
        r.run();
        holder.get().run();
        return 2;
    }

    static int bounded(Holder<? extends Runnable> holder) {
        Runnable r = holder.get();
        r.run();
        return 1;
    }

    static int calls;

    public static void main(String[] args) {
        Runnable counter = () -> calls++;
        Holder<Runnable> h = new Holder<>(counter);
        if (run(h) != 2 || bounded(h) != 1 || calls != 3) {
            throw new RuntimeException("calls " + calls);
        }
        System.out.println("WildcardCaptureBoundTest passed!");
    }
}
