/* Regression test: a private overload of ANOTHER top-level class is not a
 * candidate for a call from here, however much more specific it is than the
 * accessible one (Spring Boot's TypeUtils.getJavaDoc(Element) and the private
 * getJavaDoc(RecordComponentElement)). */
class DocUtils {
    String doc(Object o) {
        return "object";
    }

    private String doc(String s) {
        return "string";
    }

    String own(String s) {
        return doc(s);
    }
}

public class PrivateOverloadOfOtherClassTest {
    public static void main(String[] args) {
        DocUtils u = new DocUtils();
        if (!"object".equals(u.doc("x")) || !"string".equals(u.own("y"))) {
            throw new RuntimeException(u.doc("x") + u.own("y"));
        }
        System.out.println("PrivateOverloadOfOtherClassTest passed!");
    }
}
