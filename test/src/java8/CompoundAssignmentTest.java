/*
 * Compound assignment (JLS 15.26.2): the operator's operand types are
 * promoted using binary numeric promotion, and the result is narrowed back
 * to the left-hand side's declared type, for every primitive type, for
 * String, and for a field/array element target.
 */
public class CompoundAssignmentTest {

    static long sfield = 1;
    long ifield = 1;

    static void check(boolean ok, String what) {
        if (!ok) {
            System.out.println("FAILED: " + what);
            System.exit(1);
        }
    }

    public static void main(String[] args) {
        long a = 1;
        a += 2L;
        check(a == 3L, "long += long");

        double d = 1;
        d *= 1.5;
        check(d == 1.5, "double *= double");

        int i = 2;
        i += 3;
        i *= 2;
        check(i == 10, "int += and *= (already worked)");

        long x = 10;
        x -= 3;
        check(x == 7L, "long -= int literal");

        long y = 100;
        y /= 3;
        check(y == 33L, "long /= int literal");

        long z = 100;
        z %= 7;
        check(z == 2L, "long %= int literal");

        String s = "a";
        s += "b";
        check("ab".equals(s), "String += String");
        s += 1;
        check("ab1".equals(s), "String += int");
        s += 2L;
        check("ab12".equals(s), "String += long");

        byte by = 10;
        by += 5;
        check(by == 15, "byte += int (narrowing back to byte)");

        char ch = 'a';
        ch += 1;
        check(ch == 'b', "char += int (narrowing back to char)");

        sfield += 2;
        check(sfield == 3L, "static long field += int");

        CompoundAssignmentTest t = new CompoundAssignmentTest();
        t.ifield += 2;
        check(t.ifield == 3L, "instance long field += int");

        long[] arr = new long[1];
        arr[0] = 5;
        arr[0] += 2;
        check(arr[0] == 7L, "long array element += int");

        int bits = 0xF0;
        bits &= 0xFF00;
        check(bits == 0, "int &= (promotion doesn't change opcode)");

        long shifted = 1L;
        shifted <<= 3;
        check(shifted == 8L, "long <<= int");

        int halved = 16;
        halved >>= 2;
        check(halved == 4, "int >>= int");

        int uns = -1;
        uns >>>= 28;
        check(uns == 15, "int >>>= int");

        System.out.println("CompoundAssignmentTest passed!");
    }
}
