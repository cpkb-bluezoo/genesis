import java.util.function.Function;

/* The shadowing that is legal must keep compiling when a local may not hide another local of its
 * method (JLS 6.4): sibling scopes reuse names, a field is hidden freely, and a class declared in a
 * method starts a scope of its own. */
public class LegalLocalShadowingTest {
    interface SAM {
        void m(Object o);
    }

    int value = 1;
    static SAM field1 = field1 -> { };

    int siblings(int n) {
        int total = 0;
        { int x = 1; total += x; }
        { int x = 2; total += x; }
        for (int i = 0; i < n; i++) { total += i; }
        for (int i = 0; i < n; i++) { total += i; }
        switch (n) {
            case 0: { int y = 1; total += y; break; }
            default: { int y = 2; total += y; }
        }
        try { throw new RuntimeException(); } catch (RuntimeException e) { total += 1; }
        try { throw new RuntimeException(); } catch (RuntimeException e) { total += 1; }
        Function<Integer, Integer> f = v -> v + 1;
        Function<Integer, Integer> g = v -> v + 2;
        return total + f.apply(0) + g.apply(0);
    }

    int hidesField(int value) {
        Function<Integer, Integer> f = field1 -> field1 * 2;
        int fromLambda = f.apply(value);
        return fromLambda;
    }

    int localClass() {
        int local = 1;
        class Inner {
            int run(int local) {
                int other = local * 2;
                return other;
            }
        }
        Object anonymous = new Object() {
            int again(int local) {
                return local + 1;
            }
        };
        return new Inner().run(5) + local;
    }

    int afterBlock() {
        { int late = 1; }
        int late = 2;
        return late;
    }

    public static void main(String[] args) {
        LegalLocalShadowingTest t = new LegalLocalShadowingTest();
        int total = t.siblings(2) + t.hidesField(3) + t.localClass() + t.afterBlock();
        if (total != 31) {
            System.out.println("FAILED: " + total);
            System.exit(1);
        }
        System.out.println("LegalLocalShadowingTest passed!");
    }
}
