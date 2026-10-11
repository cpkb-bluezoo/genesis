/*
 * Regression test: "continue" inside a switch inside a loop jumped to itself (the switch's
 * own context was taken for the loop's), and a switch every path of which returns, continues or
 * throws was still followed by the loop's back-edge, unreachable and without a stack map frame.
 */
public class ContinueInsideSwitchTest {

    static int pos;

    static int next() {
        return pos++ < 3 ? ',' : ']';
    }

    static String read() {
        StringBuilder result = new StringBuilder();
        boolean trailing = false;
        while (true) {
            switch (next()) {
                case -1:
                    throw new RuntimeException("unterminated");
                case ']':
                    if (trailing) {
                        result.append("T");
                    }
                    return result.toString();
                case ',', ';':
                    result.append("n");
                    trailing = true;
                    continue;
                default:
                    pos--;
            }
            result.append("v");
            switch (next()) {
                case ']':
                    return result.toString();
                case ',', ';':
                    trailing = true;
                    continue;
                default:
                    throw new RuntimeException("unexpected");
            }
        }
    }

    static int forLoop() {
        int sum = 0;
        for (int i = 0; i < 6; i++) {
            switch (i % 3) {
                case 0:
                    continue;
                case 1:
                    sum += 1;
                    break;
                default:
                    sum += 10;
            }
            sum += 100;
        }
        return sum;
    }

    public static void main(String[] args) {
        String actual = read() + " " + forLoop();
        if (!"nnnT 422".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("ContinueInsideSwitchTest passed!");
    }
}
