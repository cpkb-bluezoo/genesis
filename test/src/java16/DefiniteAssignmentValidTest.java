import java.util.function.Supplier;

/* Programs the definite assignment analysis (JLS 16) must accept: a variable assigned on every path
 * that reaches a use, however the paths are made - conditions, loops, switches, try statements,
 * short-circuit operators, labelled breaks. Run, so that the values come out right too. */
public class DefiniteAssignmentValidTest {
    static boolean flag(int n) {
        return n > 0;
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
        int sum = ifElse(1) + ifElse(0) + ifThenReturn(1) + ifThrow(1) + oldSwitchWithDefault(0)
            + oldSwitchWithDefault(2) + oldSwitchWithDefault(9) + fallThrough(0) + arrowSwitch(1)
            + switchExpression(0) + switchGroupLocal(0) + switchGroupLocal(5) + tryCatch("12")
            + tryCatch("x") + tryCatchThrows("3") + tryFinally("a") + tryWithResourcesLike("a")
            + doWhile(0) + forLoopInit() + continueLoop(5)
            + shortCircuitOr(0) + ternary(true) + labelledLoops(4)
            + synchronizedBlock(new Object()) + lambdaCapture(1) + lambdaLocal() + anonymousClass(1)
            + blankFinal(0) + assignedInCondition(8) + nestedTry("5")
            + assertDoesNotAssign() + chainedAssign() + declaredInGroupAssignedLater(1) + compoundAfterAssign();
        if (sum != 258) {
            System.out.println("FAILED: " + sum);
            System.exit(1);
        }
        System.out.println("DefiniteAssignmentValidTest passed!");
    }
}
