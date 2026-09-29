package inheritedcrossfilefield;

/* Regression test: a field (`session`) declared on a superclass (`Base`,
 * in Base.java) and inherited by a subclass (`Mid`, in Mid.java, adding
 * nothing relevant to the field), accessed through a THIRD file
 * (this file) whose own field is typed as the SUBCLASS (`Mid`) - i.e. a
 * doubly-cross-file, inherited field access: `holder.session.getCredit()`
 * where `holder`'s static type (`Mid`) doesn't declare `session` itself,
 * only inherits it from `Base`. See IncomingDeliveryImpl/ReceiverImpl/
 * LinkImpl in gumdrop's amqp1 client for the real-world shape this
 * mirrors (Amqp1ReceiverTest). */
public class InheritedCrossFileFieldVerifyTest {
    final Mid holder;

    InheritedCrossFileFieldVerifyTest(Mid holder) {
        this.holder = holder;
    }

    int credit() {
        return holder.session.getCredit();
    }

    public static void main(String[] args) {
        Session s = new Session();
        Mid m = new Mid(s);
        InheritedCrossFileFieldVerifyTest t = new InheritedCrossFileFieldVerifyTest(m);
        int c = t.credit();
        if (c != 42) {
            throw new RuntimeException("expected 42, got " + c);
        }
        System.out.println("PASS");
    }
}
