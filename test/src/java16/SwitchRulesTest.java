import java.util.ArrayList;
import java.util.List;

/* Switch rules ("case X ->") over constants: as expressions (typed as the join
 * of their arms, with the operand stack carried across them) and as statements
 * (several labels per rule, blocks that return or break, strings, chars). */
public class SwitchRulesTest {
    enum Level { LOW, MID, HIGH }

    static final char SLASH = '/';
    static final String KEY = "key";

    static String text(String s) {
        return "<" + switch (s) {
            case "a", "b" -> "ab";
            case KEY -> "key!";
            default -> s.toUpperCase();
        } + ">";
    }

    static int chars(char c) {
        return switch (c) {
            case 'x' -> 1;
            case SLASH -> 2;
            case 'a', 'b', 'c' -> 3;
            default -> -1;
        };
    }

    static long widened(int n) {
        return switch (n) {
            case 0 -> 1;
            case 1 -> 2L;
            default -> n;
        };
    }

    static double mixed(Level l) {
        return switch (l) {
            case LOW -> 1;
            case MID -> 2.5f;
            case HIGH -> 3.25;
        };
    }

    static Object joined(int n) {
        return switch (n) {
            case 0 -> "zero";
            case 1 -> 1;
            case 2 -> null;
            default -> new ArrayList<String>();
        };
    }

    static int boxed(Integer n) {
        return switch (n) {
            case 1 -> 10;
            case 2 -> 20;
            default -> 0;
        };
    }

    static String blocks(Level l, int k) {
        return switch (l) {
            case LOW -> {
                String s = "lo" + k;
                yield s;
            }
            case MID -> {
                if (k > 0) {
                    yield "mid+";
                }
                int t = k * 2;
                yield "mid" + t;
            }
            case HIGH -> throw new IllegalStateException("high");
        };
    }

    static int stackCarried(Level l) {
        int[] a = new int[1];
        a[0] = 5 + switch (l) {
            case LOW -> 1;
            case MID -> 2;
            case HIGH -> 3;
        } * 2;
        return a[0] + take(7, switch (l) {
            case LOW -> 1L;
            default -> 2L;
        });
    }

    static int take(int a, long b) {
        return (int) (a + b);
    }

    static String nested(Level l, int n) {
        return switch (l) {
            case LOW -> switch (n) {
                case 0 -> "low0";
                default -> "low" + n;
            };
            default -> "other";
        };
    }

    static String defaultInTheMiddle(int n) {
        return switch (n) {
            case 1 -> "one";
            default -> "many";
            case 2 -> "two";
        };
    }

    static boolean inCondition(Level l) {
        if (switch (l) { case LOW -> 1; default -> 2; } == 1) {
            return true;
        }
        return false;
    }

    static int loop() {
        int sum = 0;
        for (int i = 0; i < 6; i++) {
            sum += switch (i % 3) {
                case 0 -> 1;
                case 1 -> 10;
                default -> 100;
            };
        }
        return sum;
    }

    static String statements(Level l, String s) {
        StringBuilder out = new StringBuilder();
        switch (l) {
            case LOW, MID -> out.append("lm");
            case HIGH -> {
                out.append("h");
                if (s.isEmpty()) {
                    break;
                }
                out.append(s);
            }
        }
        switch (s) {
            case "x" -> out.append("X");
            case "y", "z" -> out.append("YZ");
            default -> out.append("?");
        }
        return out.toString();
    }

    static int returning(int n) {
        switch (n) {
            case 0 -> {
                return 100;
            }
            case 1 -> {
                if (n > 0) {
                    return 101;
                } else {
                    return 102;
                }
            }
            default -> throw new IllegalArgumentException("n=" + n);
        }
    }

    static int oldStyleStillWorks(int n) {
        switch (n) {
            case 1:
            case 2:
                return 12;
            default:
                return 0;
        }
    }

    private static void check(boolean ok, String what) {
        if (!ok) {
            throw new AssertionError(what);
        }
    }

    public static void main(String[] args) {
        check(text("a").equals("<ab>") && text("key").equals("<key!>") && text("q").equals("<Q>"), "text");
        check(chars('x') == 1 && chars('/') == 2 && chars('b') == 3 && chars('z') == -1, "chars");
        check(widened(0) == 1L && widened(1) == 2L && widened(9) == 9L, "widened");
        check(mixed(Level.LOW) == 1.0 && mixed(Level.MID) == 2.5 && mixed(Level.HIGH) == 3.25, "mixed");
        check("zero".equals(joined(0)) && Integer.valueOf(1).equals(joined(1)) && joined(2) == null
                && joined(3) instanceof ArrayList, "joined");
        check(boxed(1) == 10 && boxed(2) == 20 && boxed(3) == 0, "boxed");
        check(blocks(Level.LOW, 3).equals("lo3") && blocks(Level.MID, 1).equals("mid+")
                && blocks(Level.MID, -2).equals("mid-4"), "blocks");
        try {
            blocks(Level.HIGH, 0);
            check(false, "throw arm");
        } catch (IllegalStateException expected) {
            check("high".equals(expected.getMessage()), "throw arm message");
        }
        check(stackCarried(Level.LOW) == 7 + 1 + 7 && stackCarried(Level.HIGH) == 11 + 9, "stackCarried");
        check(nested(Level.LOW, 0).equals("low0") && nested(Level.LOW, 4).equals("low4")
                && nested(Level.HIGH, 0).equals("other"), "nested");
        check(defaultInTheMiddle(1).equals("one") && defaultInTheMiddle(2).equals("two")
                && defaultInTheMiddle(3).equals("many"), "defaultInTheMiddle");
        check(inCondition(Level.LOW) && !inCondition(Level.HIGH), "inCondition");
        check(loop() == 222, "loop");
        check(statements(Level.LOW, "x").equals("lmX") && statements(Level.HIGH, "").equals("h?")
                && statements(Level.HIGH, "z").equals("hzYZ"), "statements");
        check(returning(0) == 100 && returning(1) == 101, "returning");
        try {
            returning(5);
            check(false, "returning throw");
        } catch (IllegalArgumentException expected) {
            check("n=5".equals(expected.getMessage()), "returning message");
        }
        check(oldStyleStillWorks(1) == 12 && oldStyleStillWorks(2) == 12 && oldStyleStillWorks(3) == 0, "old style");
        List<String> none = new ArrayList<>();
        check(none.isEmpty(), "sanity");
        System.out.println("SwitchRulesTest passed!");
    }
}
