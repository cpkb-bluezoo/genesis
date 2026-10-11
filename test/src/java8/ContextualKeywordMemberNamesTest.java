import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

/* Regression test: contextual keywords (to, with, record, open, exports, opens,
 * uses, provides, requires, permits, when, sealed...) are ordinary names for
 * methods, fields and variables, and so the target of a method reference:
 * "this::to" and "this::record" failed to parse (Spring Boot's WritableJson.to
 * and BufferingApplicationStartup.record). */
public class ContextualKeywordMemberNamesTest {
    final StringBuilder log = new StringBuilder();
    int open = 1;
    int exports = 2;

    void to(Appendable out) {
        try {
            out.append("to");
        } catch (java.io.IOException e) {
            throw new RuntimeException(e);
        }
    }

    void record(String s) {
        log.append("record:").append(s).append(';');
    }

    String with(String s) {
        return "with:" + s;
    }

    static String requires(String s) {
        return "requires:" + s;
    }

    static String uses() {
        return "uses";
    }

    interface Sink {
        void to(Appendable out) throws java.io.IOException;
    }

    static String drain(Sink sink) throws java.io.IOException {
        StringBuilder sb = new StringBuilder();
        sink.to(sb);
        return sb.toString();
    }

    static int count(Consumer<Appendable> c) {
        return c == null ? 0 : 1;
    }

    int viaReturn() {
        return count(this::to);
    }

    String run() throws java.io.IOException {
        Consumer<Appendable> sink = this::to;
        sink.accept(new StringBuilder());
        if (viaReturn() != 1) {
            throw new RuntimeException("return");
        }
        Consumer<String> recorder = this::record;
        recorder.accept("a");
        Function<String, String> w = this::with;
        Function<String, String> r = ContextualKeywordMemberNamesTest::requires;
        Supplier<String> u = ContextualKeywordMemberNamesTest::uses;
        String viaCall = this.with("b") + requires("c") + uses();
        int permits = open + exports;
        int when = permits * 2;
        return drain(this::to) + log + w.apply("x") + r.apply("y") + u.get() + viaCall + permits + when;
    }

    public static void main(String[] args) throws java.io.IOException {
        String got = new ContextualKeywordMemberNamesTest().run();
        String want = "torecord:a;with:xrequires:yuseswith:brequires:cuses36";
        if (!want.equals(got)) {
            throw new RuntimeException(got);
        }
        System.out.println("ContextualKeywordMemberNamesTest passed!");
    }
}
