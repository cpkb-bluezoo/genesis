import java.util.function.Consumer;

/*
 * Regression test: "Outer<X>.Inner" as a parameter or return type, or as a type argument, was a
 * parse error ("Expected type declaration") - only as a field type did it get through.
 */
public class QualifiedParameterizedMemberTypeTest<H> {

    public class Registry {
        String name() {
            return "registry";
        }
    }

    static String seen;

    void accept(Consumer<QualifiedParameterizedMemberTypeTest<String>.Registry> consumer) {
        consumer.accept(new QualifiedParameterizedMemberTypeTest<String>().new Registry());
    }

    QualifiedParameterizedMemberTypeTest<String>.Registry make() {
        return new QualifiedParameterizedMemberTypeTest<String>().new Registry();
    }

    String describe(QualifiedParameterizedMemberTypeTest<String>.Registry registry) {
        return registry.name();
    }

    public static void main(String[] args) {
        QualifiedParameterizedMemberTypeTest<Object> test = new QualifiedParameterizedMemberTypeTest<>();
        test.accept(r -> seen = test.describe(r));
        if (!"registry".equals(seen) || !"registry".equals(test.make().name())) {
            throw new RuntimeException("FAIL: " + seen);
        }
        System.out.println("QualifiedParameterizedMemberTypeTest passed!");
    }
}
