package p;

/* A record (and enum, interface) declared inside a class of the same batch is implicitly
 * static: creating one from an inner class passes no enclosing instance. */
public class MemberRecordUseTest {
    class Inner {
        Types.Person make() {
            return new Types.Person("inner", 1);
        }
    }

    public static void main(String[] args) {
        Types.Person person = new MemberRecordUseTest().new Inner().make();
        if (!"inner".equals(person.name()) || Types.Kind.valueOf("TWO") != Types.Kind.TWO) {
            throw new AssertionError(person);
        }
        System.out.println("PASS");
    }
}
