import ep.ExecutorProvider;
import java.util.concurrent.Executor;

public class NestedLambdaOuter {
    static class InnerConfig {
        ExecutorProvider bean() {
            return () -> new Executor() {
                public void execute(Runnable command) {
                }
            };
        }
    }

    public static void main(String[] args) {
        ExecutorProvider p = new InnerConfig().bean();
        if (p.getExecutor() == null) {
            throw new RuntimeException("null executor");
        }
        System.out.println("PASS");
    }
}
