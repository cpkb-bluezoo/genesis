/* Regression test (compiled with genericsuperclient/Mid.java): a
 * receiver-less call, inside an inner class, to a method inherited by the
 * ENCLOSING class from a class-file superclass that sits two levels up a
 * generic chain ("Leaf extends Mid<Leaf>", "Mid<E extends Mid<E>>
 * extends Base<E>"). The enclosing-class walk stopped at a hollow stub of Base
 * and the call typed as <unknown> ("cannot convert <unknown> to long"). Also
 * covers the class's own type variable in its extends clause and a call that
 * returns that variable. Mirrors gumdrop's OtlpExporter.ExportThread. */
public class InnerCallThroughGenericSuperTest extends Mid<InnerCallThroughGenericSuperTest> {
    final class Worker extends Thread {
        long pass(long now) {
            long wait = getInterval() - (now - 1);
            return wait + getSize();
        }
    }

    public static void main(String[] args) {
        InnerCallThroughGenericSuperTest t = new InnerCallThroughGenericSuperTest().name("q").interval(9);
        long got = t.new Worker().pass(3);
        if (got != 9) {
            throw new RuntimeException("got " + got);
        }
        if (!"q".equals(t.name)) {
            throw new RuntimeException(t.name);
        }
        System.out.println("InnerCallThroughGenericSuperTest passed!");
    }
}
