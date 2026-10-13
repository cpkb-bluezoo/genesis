/* Regression tests: "break label" leaves any labelled statement, not only a loop or a switch (JLS
 * 14.15): a labelled block, labelled blocks nested in each other, and one in a loop. genesis
 * reported "'break' outside switch or loop". A "break label" out of an inner loop also went to the
 * end of the inner loop, as the label stayed pending for the loops nested in the labelled one. */
public class LabelledBlockBreakTest {
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

    static int nested(int n) {
        int total = 0;
        a:
        {
            b:
            {
                if (n == 0) {
                    break a;
                }
                total += 1;
                if (n == 1) {
                    break b;
                }
                total += 10;
            }
            total += 100;
        }
        return total;
    }

    static int inLoop(int n) {
        int total = 0;
        for (int i = 0; i < n; i++) {
            skip:
            {
                if (i % 2 == 0) {
                    break skip;
                }
                total += i;
            }
            total += 1000;
        }
        return total;
    }

    static int outOfInnerLoop(int n) {
        int found = 0;
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

    static int continueOuter(int n) {
        int count = 0;
        outer:
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (j > i) {
                    continue outer;
                }
                count++;
            }
        }
        return count;
    }

    private static void check(int expected, int actual, String what) {
        if (expected != actual) {
            System.out.println("FAILED " + what + ": expected " + expected + ", got " + actual);
            System.exit(1);
        }
    }

    public static void main(String[] args) {
        check(1, labelled(1), "labelled(1)");
        check(2, labelled(0), "labelled(0)");
        check(0, nested(0), "nested(0)");
        check(101, nested(1), "nested(1)");
        check(111, nested(2), "nested(2)");
        check(4004, inLoop(4), "inLoop");
        check(4, outOfInnerLoop(3), "outOfInnerLoop");
        check(10, continueOuter(4), "continueOuter");
        System.out.println("LabelledBlockBreakTest passed!");
    }
}
