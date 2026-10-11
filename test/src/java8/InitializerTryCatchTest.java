/*
 * Regression test: a try/catch in a static initializer block, or in an instance initializer block
 * of a class with the default constructor (or an anonymous class), lost its exception table: the
 * handler was in the code and nothing led to it, so the exception escaped (Spring's
 * WebMvcTypeExcludeFilter loading optional classes in a static block:
 * ExceptionInInitializerError).
 */
public class InitializerTryCatchTest {

    static String staticResult;

    static {
        try {
            staticResult = String.valueOf(Class.forName("does.not.Exist"));
        }
        catch (Exception ex) {
            staticResult = "static caught";
        }
    }

    String instanceResult;

    {
        try {
            instanceResult = String.valueOf(Class.forName("does.not.Exist"));
        }
        catch (ClassNotFoundException ex) {
            instanceResult = "instance caught";
        }
    }

    public static void main(String[] args) {
        String anonymous = new Object() {
            String value;

            {
                try {
                    value = String.valueOf(Class.forName("does.not.Exist"));
                }
                catch (ClassNotFoundException ex) {
                    value = "anonymous caught";
                }
            }

            @Override
            public String toString() {
                return value;
            }
        }.toString();
        String actual = staticResult + "|" + new InitializerTryCatchTest().instanceResult + "|" + anonymous;
        if (!"static caught|instance caught|anonymous caught".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("PASS");
    }
}
