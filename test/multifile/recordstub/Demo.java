class Box {
    final String text;
    Box(String message, Throwable cause) {
        this.text = message + ":" + (cause == null ? "none" : cause.getMessage());
    }
}

public class Demo {
    static Box go(Failure failure) {
        return switch (failure) {
            case Failure.Transport(var message, var cause) -> new Box(message, cause);
            case Failure.Holder<?>(var value, var more) -> new Box("holder " + value.intValue() + " " + more.length, null);
            case Failure.Timeout(long millis, int[] attempts, var hosts) ->
                new Box("timeout " + millis + " " + attempts.length + " " + hosts, null);
        };
    }

    static boolean isTransport(Object o) {
        return o instanceof Failure.Transport(String message, Throwable cause) && message.equals("m");
    }

    public static void main(String[] args) {
        String a = go(new Failure.Transport("m", new RuntimeException("c"))).text;
        String b = go(new Failure.Timeout(5L, new int[2], java.util.List.of("h"))).text;
        String c = go(new Failure.Holder<Integer>(7, new Integer[3])).text;
        if (!c.equals("holder 7 3:none") || !a.equals("m:c") || !b.equals("timeout 5 2 [h]:none") ||
            !isTransport(new Failure.Transport("m", null)) ||
            isTransport(new Failure.Transport("x", null))) {
            throw new AssertionError(a + " / " + b);
        }
        System.out.println("RecordStubComponents passed!");
    }
}
