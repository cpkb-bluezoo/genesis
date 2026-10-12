package ep;

import java.util.concurrent.Executor;

@FunctionalInterface
public interface ExecutorProvider {
    Executor getExecutor();
}
