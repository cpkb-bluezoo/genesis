/**
 * Bug: an enum constant declared with a class body ("P(2) { ... }") was
 * silently dropped. Real javac compiles such a constant as an anonymous
 * subclass of the enum (e.g. "W$1"), instantiated via
 * "new W$1(name, ordinal, args...)" which forwards to the enum's own
 * (JLS 8.9.2 implicitly-private) constructor via super(). Genesis never
 * created that subclass at all: the constant's own override method was
 * registered into the ENUM's shared member scope under the same key as
 * the enum's own method of the same name/arity, so hashtable_insert's
 * replace-on-collision semantics silently overwrote the override with no
 * error of any kind, and no W$1 class was ever written - P.twice() ran
 * the enum's own base method instead of its own override. Confirmed
 * against real javac to produce an identical anonymous-subclass shape
 * (super-forwarding constructor, own override method, correct nest-mate
 * access to the enum's private constructor and inherited field).
 */
public class EnumConstantClassBodyVerifyTest {
    enum W {
        P(2) {
            @Override
            int twice() {
                return n * 2;
            }
        },
        Q(3);

        final int n;

        W(int n) {
            this.n = n;
        }

        int twice() {
            return -1;
        }
    }

    public static void main(String[] args) {
        int p = W.P.twice();
        int q = W.Q.twice();
        if (p != 4) {
            throw new RuntimeException("expected P.twice()==4, got " + p);
        }
        if (q != -1) {
            throw new RuntimeException("expected Q.twice()==-1 (base method, no override), got " + q);
        }
        if (W.P.n != 2 || W.Q.n != 3) {
            throw new RuntimeException("enum constant's own inherited field not set correctly: P.n=" +
                W.P.n + " Q.n=" + W.Q.n);
        }
        if (!W.P.name().equals("P") || W.P.ordinal() != 0 || !W.Q.name().equals("Q") || W.Q.ordinal() != 1) {
            throw new RuntimeException("enum constant name/ordinal wrong: P=" + W.P.name() + "/" +
                W.P.ordinal() + " Q=" + W.Q.name() + "/" + W.Q.ordinal());
        }
    }
}
