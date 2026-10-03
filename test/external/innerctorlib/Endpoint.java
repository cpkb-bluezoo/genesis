package innerctorlib;

import java.util.concurrent.CountDownLatch;

public class Endpoint {
    public class Handler {
        public final CountDownLatch latch;

        public Handler(CountDownLatch latch) {
            this.latch = latch;
        }
    }

    public class Wide {
        public final long id;
        public final String label;

        public Wide(long id, String label) {
            this.id = id;
            this.label = label;
        }
    }
}
