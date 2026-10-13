import java.util.function.Supplier;

/* Programs the definite assignment analysis (JLS 16) must accept: a variable assigned on every path
 * that reaches a use, however the paths are made - conditions, loops, switches, try statements,
 * short-circuit operators, labelled breaks. Run, so that the values come out right too. */
public class DefiniteAssignmentValidTest {
    static boolean flag(int n) {
        return n > 0;
    }

    static int whileTrue(int n) {
        int x;
        while (true) {
            if (n > 3) {
                x = n;
                break;
            }
            n++;
        }
        return x;
    }

    static int forever(int n) {
        int x;
        for (;;) {
            x = n * 2;
            if (x > 0) {
                break;
            }
            n++;
        }
        return x;
    }

    static int shortCircuitAnd(int n) {
        int x;
        if (flag(n) && (x = n) > 0) {
            return x;
        }
        return 0;
    }

    static int negated(int n) {
        int x;
        if (!(flag(n) || (x = n) > 0)) {
            return x;
        }
        return 0;
    }

    static int breakThroughFinally(int n) {
        int x;
        while (true) {
            try {
                if (n > 0) {
                    x = n;
                    break;
                }
            } finally {
                n++;
            }
        }
        return x;
    }

    static double everyKind(int n) {
        long l;
        double d;
        float f;
        boolean b;
        char c;
        String s;
        int[] arr;
        Object o;
        while (true) {
            if (n > 0) {
                l = n;
                d = n * 1.5;
                f = n * 2f;
                b = n > 1;
                c = 'a';
                s = "s" + n;
                arr = new int[] {n};
                o = s;
                break;
            }
            n++;
        }
        return l + d + f + (b ? 1 : 0) + c + s.length() + arr[0] + o.toString().length();
    }

    static int nestedLoopsAssign(int n) {
        int found;
        int i = 0;
        outer:
        for (;;) {
            for (int j = 0; j < 10; j++) {
                if (i * j > n) {
                    found = i * j;
                    break outer;
                }
            }
            i++;
        }
        return found;
    }

    static int labelled(int n) {
        int x;
        outer:
        {
            if (n > 0) {
                x = 1;
                break outer;
            }
            x = 2;
        }
        return x;
    }

    static int ifElse(int n) {
        int x;
        if (n > 0) {
            x = 1;
        } else {
            x = 2;
        }
        return x;
    }

    static int ifThenReturn(int n) {
        int x;
        if (n > 0) {
            x = 1;
        } else {
            return -1;
        }
        return x;
    }

    static int ifThrow(int n) {
        int x;
        if (n > 0) {
            x = 5;
        } else {
            throw new IllegalArgumentException();
        }
        return x;
    }

    static int oldSwitchWithDefault(int n) {
        int x;
        switch (n) {
            case 0:
                x = 10;
                break;
            case 1:
            case 2:
                x = 20;
                break;
            default:
                x = 30;
        }
        return x;
    }

    static int fallThrough(int n) {
        int x;
        switch (n) {
            case 0:
                x = 1;
            case 1:
                x = 2;
                break;
            default:
                x = 3;
        }
        return x;
    }

    static int arrowSwitch(int n) {
        int x;
        switch (n) {
            case 0 -> x = 1;
            case 1 -> { x = 2; }
            default -> throw new IllegalStateException();
        }
        return x;
    }

    static int switchExpression(int n) {
        int x;
        int y = switch (n) {
            case 0 -> { x = 4; yield 1; }
            default -> { x = 5; yield 2; }
        };
        return x + y;
    }

    static int switchGroupLocal(int n) {
        switch (n) {
            case 0:
                int y = 1;
                return y;
            default:
                y = 3;
                return y;
        }
    }

    static int tryCatch(String s) {
        int x;
        try {
            x = Integer.parseInt(s);
        } catch (NumberFormatException e) {
            x = -1;
        }
        return x;
    }

    static int tryCatchThrows(String s) {
        int x;
        try {
            x = Integer.parseInt(s);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(e);
        }
        return x;
    }

    static int tryFinally(String s) {
        int x;
        try {
            s.length();
        } finally {
            x = 7;
        }
        return x;
    }

    static int tryWithResourcesLike(String s) {
        int x;
        try (java.io.StringReader r = new java.io.StringReader(s)) {
            x = r.read();
        } catch (java.io.IOException e) {
            x = -2;
        }
        return x;
    }

    static int doWhile(int n) {
        int x;
        do {
            x = n++;
        } while (x < 3);
        return x;
    }

    static int forLoopInit() {
        int total = 0;
        int i;
        for (i = 0; i < 3; i++) {
            total += i;
        }
        return total + i;
    }

    static int continueLoop(int n) {
        int total = 0;
        for (int i = 0; i < n; i++) {
            int x;
            if (i % 2 == 0) {
                continue;
            }
            x = i;
            total += x;
        }
        return total;
    }

    static int shortCircuitOr(int n) {
        int x;
        if (!flag(n) || (x = n) < 0) {
            return 0;
        }
        return 1;
    }

    static int ternary(boolean c) {
        int y;
        boolean ok = c ? (y = 1) > 0 : (y = 2) > 0;
        return ok ? y : -y;
    }

    static int labelledLoops(int n) {
        int x = 0;
        int found;
        search:
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (i * j > 2) {
                    found = i * j;
                    x = found;
                    break search;
                }
            }
        }
        return x;
    }

    static int synchronizedBlock(Object lock) {
        int x;
        synchronized (lock) {
            x = 3;
        }
        return x;
    }

    static int lambdaCapture(int n) {
        int x;
        x = n + 1;
        Supplier<Integer> s = () -> x * 2;
        return s.get();
    }

    static int lambdaLocal() {
        Supplier<Integer> s = () -> {
            int y;
            if (flag(1)) {
                y = 1;
            } else {
                y = 2;
            }
            return y;
        };
        return s.get();
    }

    static int anonymousClass(int n) {
        int x;
        x = n;
        Supplier<Integer> s = new Supplier<Integer>() {
            public Integer get() {
                int z;
                z = x + 1;
                return z;
            }
        };
        return s.get();
    }

    static int capturedNames() {
        int x;
        int y;
        y = 2;
        Supplier<Integer> s = new Supplier<Integer>() {
            int x = 10;

            public Integer get() {
                int fromParam = both(1);
                return x + fromParam + y;
            }

            int both(int y) {
                return y;
            }
        };
        x = 5;
        return s.get() + x;
    }

    static int localClassAfterAssign() {
        int x;
        x = 4;
        class Local {
            int get() {
                return x * 2;
            }
        }
        return new Local().get();
    }

    static int finalOnce(int n) {
        final int x;
        if (n > 0) {
            x = 1;
        } else {
            x = 2;
        }
        return x;
    }

    static int finalInLoopBody(int n) {
        int total = 0;
        for (int i = 0; i < n; i++) {
            final int k;
            k = i * 2;
            total += k;
        }
        return total;
    }

    static int finalSwitch(int i) {
        final int x;
        switch (i) {
            case 0:
                x = 1;
                break;
            case 1:
                x = 2;
                break;
            default:
                x = 3;
        }
        return x;
    }

    static int finalTryThrows(String s) {
        final int x;
        try {
            x = Integer.parseInt(s);
        } catch (RuntimeException e) {
            throw new IllegalStateException();
        }
        return x;
    }

    static int finalWithInitializer() {
        final int k = 7;
        final int j;
        j = k + 1;
        return j;
    }

    static int finalDoWhileFalse() {
        final int x;
        do {
            x = 3;
        } while (false);
        return x;
    }

    static int finalConditional(boolean a, boolean b) {
        final int x;
        if (a) {
            x = 1;
        } else if (b) {
            x = 2;
        } else {
            x = 3;
        }
        return x;
    }

    static class Both {
        final int a;
        final String b;
        static final int S;

        static {
            S = 3;
        }

        Both(boolean c) {
            if (c) {
                a = 1;
            } else {
                a = 2;
            }
            b = "x";
        }

        Both() {
            this(true);
        }
    }

    static class ViaInitializer {
        final int v;

        {
            v = 4;
        }

        ViaInitializer() {
        }

        ViaInitializer(int n) {
        }
    }

    static class ThisDot {
        final int t;

        ThisDot(int n) {
            this.t = n;
        }
    }

    static class ThrowPath {
        final int p;

        ThrowPath(int n) {
            if (n < 0) {
                throw new IllegalArgumentException();
            }
            p = n;
        }
    }

    static class SwitchAssign {
        final int s;

        SwitchAssign(int n) {
            switch (n) {
                case 0:
                    s = 1;
                    break;
                default:
                    s = 2;
            }
        }
    }

    static class EarlyReturn {
        final int r;

        EarlyReturn(boolean c) {
            if (c) {
                r = 1;
                return;
            }
            r = 2;
        }
    }

    static class ParamAssign {
        final int w;

        ParamAssign(int w) {
            w = w + 1;
            this.w = w;
        }
    }

    enum Level {
        LOW(1), HIGH(2);

        final int rank;

        Level(int rank) {
            this.rank = rank;
        }
    }

    static int blankFinalFields() {
        return new Both(false).a + new Both().a + Both.S + new ViaInitializer().v + new ViaInitializer(1).v
            + new ThisDot(5).t + new ThrowPath(6).p + new SwitchAssign(0).s + new EarlyReturn(true).r
            + new ParamAssign(7).w + Level.HIGH.rank;
    }

    static class ReadsAfterAssign {
        final int a;
        final int b;
        final int c;
        final Supplier<Integer> later;

        ReadsAfterAssign(int seed, boolean flag) {
            a = seed;
            int local = a + 1;
            b = local * 2;
            if (flag) {
                this.c = this.a + b;
            } else {
                this.c = -1;
            }
            later = () -> a + b + c;
        }
    }

    static class ShadowedByLocals {
        final int x;
        final int y;

        ShadowedByLocals(int x) {
            int y = x + 1;
            y = y * 2;
            x = x + 10;
            this.x = x;
            this.y = y;
        }
    }

    static class InitializerThenConstructor {
        final int base;
        int doubled = 5;

        {
            base = 3;
            doubled = base * 2;
        }

        InitializerThenConstructor() {
        }
    }

    record Pair(int first, int second) {
        Pair(int first, int second) {
            if (first > second) {
                int t = first;
                first = second;
                second = t;
            }
            this.first = first;
            this.second = second;
        }
    }

    record Wrapped(String text) {
        Wrapped {
            if (text == null) {
                text = "";
            }
        }
    }

    static int fieldReads() {
        ReadsAfterAssign r = new ReadsAfterAssign(2, true);
        ShadowedByLocals sh = new ShadowedByLocals(1);
        InitializerThenConstructor ic = new InitializerThenConstructor();
        Object holder = new Object() {
            final int fixed;

            {
                fixed = 6;
            }

            @Override
            public String toString() {
                return Integer.toString(fixed);
            }
        };
        return r.later.get() + sh.x + sh.y + ic.doubled + new Pair(9, 4).first() + new Wrapped(null).text().length()
            + holder.toString().length();
    }

    enum Shape {
        SQUARE {
            final int sides;

            {
                sides = 4;
            }

            @Override
            int sides() {
                return sides;
            }
        },
        LINE {
            @Override
            int sides() {
                return 1;
            }
        };

        abstract int sides();
    }

    static int blankFinal(int n) {
        final int k;
        if (n > 0) {
            k = 1;
        } else {
            k = 2;
        }
        return k;
    }

    static int assignedInCondition(int n) {
        int x;
        while ((x = n--) > 5) {
            n -= 2;
        }
        return x;
    }

    static int nestedTry(String s) {
        int x;
        try {
            try {
                x = Integer.parseInt(s);
            } finally {
                s = null;
            }
        } catch (NumberFormatException e) {
            x = 0;
        }
        return x;
    }

    static int assertDoesNotAssign() {
        int x = 1;
        assert x > 0;
        return x;
    }

    static int chainedAssign() {
        int a, b, c;
        a = b = c = 4;
        return a + b + c;
    }

    static int declaredInGroupAssignedLater(int n) {
        switch (n) {
            case 0:
                int v;
                v = 1;
                return v;
            case 1:
                v = 2;
                return v;
            default:
                return 0;
        }
    }

    static int compoundAfterAssign() {
        int x;
        x = 1;
        x += 2;
        x++;
        return x;
    }

    public static void main(String[] args) {
        int sum = Shape.SQUARE.sides() + Shape.LINE.sides() + labelled(1) + labelled(0) + whileTrue(0) + forever(1) + shortCircuitAnd(2) + negated(-1) + breakThroughFinally(1)
            + (int) everyKind(2) + nestedLoopsAssign(3) + ifElse(1) + ifElse(0) + ifThenReturn(1) + ifThrow(1) + oldSwitchWithDefault(0)
            + oldSwitchWithDefault(2) + oldSwitchWithDefault(9) + fallThrough(0) + arrowSwitch(1)
            + switchExpression(0) + switchGroupLocal(0) + switchGroupLocal(5) + tryCatch("12")
            + tryCatch("x") + tryCatchThrows("3") + tryFinally("a") + tryWithResourcesLike("a")
            + doWhile(0) + forLoopInit() + continueLoop(5)
            + shortCircuitOr(0) + ternary(true) + labelledLoops(4)
            + synchronizedBlock(new Object()) + lambdaCapture(1) + lambdaLocal() + anonymousClass(1)
            + capturedNames() + localClassAfterAssign() + finalOnce(1) + finalInLoopBody(3) + finalSwitch(1) + finalTryThrows("4") + finalWithInitializer() + finalDoWhileFalse() + finalConditional(false, true) + blankFinalFields() + fieldReads() + blankFinal(0) + assignedInCondition(8) + nestedTry("5")
            + assertDoesNotAssign() + chainedAssign() + declaredInGroupAssignedLater(1) + compoundAfterAssign();
        if (sum != 522) {
            System.out.println("FAILED: " + sum);
            System.exit(1);
        }
        System.out.println("DefiniteAssignmentValidTest passed!");
    }
}
