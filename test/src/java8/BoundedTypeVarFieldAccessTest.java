/* Regression test: a field read or written through a receiver whose type is a
 * type variable with a class bound ("T extends Handler"; handler.response =
 * response). The field lookup did not look through the variable's bound and
 * the assignment failed with "cannot convert Resp to <unknown>". Mirrors
 * gumdrop's CollectingRequestHandler.bind. */
public class BoundedTypeVarFieldAccessTest {
    static class Resp {
        final String id;
        Resp(String id) { this.id = id; }
    }

    static class Handler {
        public Resp response;
        public int hits;
        public long total;
        public Handler() { }
    }

    static class Special extends Handler { }

    static <T extends Handler> T bind(T handler, Resp response) {
        handler.response = response;
        handler.hits++;
        handler.hits += 2;
        handler.total = handler.hits * 10L;
        return handler;
    }

    static <T extends Handler> String idOf(T handler) {
        return handler.response.id;
    }

    public static void main(String[] args) {
        Special s = bind(new Special(), new Resp("r1"));
        if (!"r1".equals(idOf(s))) {
            throw new RuntimeException(idOf(s));
        }
        if (s.hits != 3 || s.total != 30L) {
            throw new RuntimeException(s.hits + " " + s.total);
        }
        System.out.println("BoundedTypeVarFieldAccessTest passed!");
    }
}
