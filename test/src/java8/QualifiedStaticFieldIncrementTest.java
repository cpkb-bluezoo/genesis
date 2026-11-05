/*
 * ++ and -- on a static field named through its class ("Holder.COUNT++"), through a subclass
 * of the class that declares it, or through the enclosing class's own name: the old value of
 * a postfix form and the new value of a prefix form are the value of the expression.
 */
public class QualifiedStaticFieldIncrementTest {
    static int own = 10;

    static class Holder {
        static int count = 3;
        static long total = 100L;
        static double ratio = 1.5;
        static byte small = 127;
        static char letter = 'a';
    }

    static class Child extends Holder {
    }

    static class Outer {
        static class Inner {
            static int depth = 0;
        }
    }

    public static void main(String[] args) {
        Holder.count++;
        ++Holder.count;
        int after = Holder.count++;
        int before = --Holder.count;
        if (Holder.count != 5 || after != 5 || before != 5) {
            throw new AssertionError(Holder.count + " " + after + " " + before);
        }

        long oldTotal = Holder.total++;
        long newTotal = ++Holder.total;
        double oldRatio = Holder.ratio--;
        if (oldTotal != 100L || newTotal != 102L || Holder.total != 102L || oldRatio != 1.5 || Holder.ratio != 0.5) {
            throw new AssertionError(oldTotal + " " + newTotal + " " + Holder.ratio);
        }

        Child.count++;
        int viaChild = Child.count--;
        if (Holder.count != 5 || viaChild != 6) {
            throw new AssertionError(Holder.count + " " + viaChild);
        }

        QualifiedStaticFieldIncrementTest.own++;
        int ownOld = QualifiedStaticFieldIncrementTest.own--;
        if (own != 10 || ownOld != 11) {
            throw new AssertionError(own + " " + ownOld);
        }

        Outer.Inner.depth++;
        Outer.Inner.depth++;
        if (Outer.Inner.depth != 2) {
            throw new AssertionError(Outer.Inner.depth);
        }

        Holder.small++;
        Holder.letter++;
        if (Holder.small != -128 || Holder.letter != 'b') {
            throw new AssertionError(Holder.small + " " + Holder.letter);
        }
        System.out.println("QualifiedStaticFieldIncrementTest passed!");
    }
}
