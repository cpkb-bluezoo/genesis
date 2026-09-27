/*
 * A record's toString() (JEP 395) prints its simple name, not its binary
 * name: a record nested in another class prints "P[x=1]", not "Outer$P[x=1]".
 */
public class NestedRecordToStringTest {

    record Point(int x, int y) {
    }

    static class Holder {
        record Inner(String s) {
        }
    }

    public static void main(String[] args) {
        String s1 = new Point(1, 2).toString();
        if (!"Point[x=1, y=2]".equals(s1)) {
            System.out.println("FAILED: nested record toString(): " + s1);
            System.exit(1);
        }
        
        String s2 = new Holder.Inner("hi").toString();
        if (!"Inner[s=hi]".equals(s2)) {
            System.out.println("FAILED: doubly-nested record toString(): " + s2);
            System.exit(1);
        }
        
        System.out.println("NestedRecordToStringTest passed!");
    }
}
