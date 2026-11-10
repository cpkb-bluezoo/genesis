/**
 * A lambda may implement an interface method with more than 16 parameters.
 */
public class LambdaManyParametersTest {
    interface Many {
        int apply(int a, int b, int c, int d, int e, int f, int g, int h, int i,
                  int j, int k, int l, int m, int n, int o, int p, int q, int r);
    }

    interface Typed {
        long apply(long a, int b, int c, int d, int e, int f, int g, int h, int i,
                   int j, int k, int l, int m, int n, int o, int p, double q);
    }

    public static void main(String[] args) {
        int base = 100;
        Many many = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) ->
            base + a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q + r * 2;
        int got = many.apply(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18);
        if (got != 289) {
            throw new AssertionError("got " + got);
        }
        Typed typed = (long a, int b, int c, int d, int e, int f, int g, int h, int i,
                       int j, int k, int l, int m, int n, int o, int p, double q) ->
            a + b + p + (long) q;
        if (typed.apply(10L, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 5.0) != 20L) {
            throw new AssertionError("typed");
        }
        System.out.println("LambdaManyParametersTest passed!");
    }
}
