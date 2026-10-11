import java.io.Closeable;

/*
 * Regression test: a return, break or continue inside the body of a try-with-resources
 * statement left it without closing the resources, and the dead code after the jump had no
 * stack map frame (VerifyError "Expecting a stack map frame").
 */
public class TryWithResourcesEarlyExitTest {

    static final StringBuilder LOG = new StringBuilder();

    static class Res implements Closeable {
        final String name;

        Res(String name) {
            this.name = name;
        }

        public void close() {
            LOG.append("close:").append(name).append(' ');
        }

        int status() {
            return 200;
        }
    }

    static boolean returnsInside() {
        try (Res r = new Res("a")) {
            return r.status() == 200;
        }
    }

    static int twoResources() {
        try (Res r = new Res("b1"); Res s = new Res("b2")) {
            if (r.status() > 0) {
                return 1;
            }
        }
        return 2;
    }

    static boolean insideTryCatch() {
        try {
            Res first = new Res("c0");
            try (Res r = new Res("c")) {
                return r.status() == 200;
            }
        } catch (Exception e) {
            return false;
        }
    }

    static void loop() {
        for (int i = 0; i < 2; i++) {
            try (Res r = new Res("d" + i)) {
                if (i == 0) {
                    continue;
                }
                break;
            }
        }
    }

    public static void main(String[] args) {
        returnsInside();
        twoResources();
        insideTryCatch();
        loop();
        String expected = "close:a close:b2 close:b1 close:c close:d0 close:d1 ";
        if (!expected.equals(LOG.toString())) {
            throw new RuntimeException("FAIL: " + LOG);
        }
        System.out.println("TryWithResourcesEarlyExitTest passed!");
    }
}
