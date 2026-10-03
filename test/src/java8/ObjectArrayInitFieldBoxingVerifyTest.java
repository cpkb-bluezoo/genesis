/* Regression test: "new Object[] { localLong, intField }" - a primitive
 * instance FIELD (read via an implicit this) used as an element of an
 * Object[] array initializer must be boxed, same as a primitive local.
 * genesis stored the raw int, failing verification ("Type integer ... is
 * not assignable to Object"). Mirrors gumdrop's AmqpClientRecovery
 * (LOGGER.log(Level.INFO, msg, new Object[] { delay, attempt })). */
public class ObjectArrayInitFieldBoxingVerifyTest {
    private int attempt = 3;
    private long total = 9L;
    private boolean flag = true;
    private char letter = 'q';

    Object[] fields(long delay) {
        return new Object[] { delay, attempt, total, flag, letter };
    }

    public static void main(String[] args) {
        Object[] a = new ObjectArrayInitFieldBoxingVerifyTest().fields(7L);
        if (a.length != 5 || !a[0].equals(7L) || !a[1].equals(3) || !a[2].equals(9L)
                || !a[3].equals(true) || !a[4].equals('q')) {
            throw new RuntimeException("wrong elements: " + java.util.Arrays.toString(a));
        }
        System.out.println("ObjectArrayInitFieldBoxingVerifyTest passed!");
    }
}
