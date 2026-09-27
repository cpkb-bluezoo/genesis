import java.util.concurrent.RejectedExecutionException;

/** Regression: catch (final Exception e) (StorageExecutor.java). */
public class CatchFinalTest {
    static void catchFinal() {
        try {
            throw new RejectedExecutionException("saturated");
        } catch (final RejectedExecutionException rejected) {
            System.out.println(rejected.getMessage());
        }
    }

    public static void main(String[] args) {
        catchFinal();
        System.out.println("CatchFinalTest OK");
    }
}
