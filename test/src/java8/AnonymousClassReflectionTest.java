/* Regression test: reflection must see an anonymous or local class as one.
 * Class.isAnonymousClass() and getSimpleName() come from the class's own
 * InnerClasses entry (an anonymous class has neither a declaring class nor a
 * name) together with an EnclosingMethod attribute; genesis wrote a named,
 * static entry and no EnclosingMethod, so Spring's BeanDefinitionLoader loaded
 * anonymous classes as components and derived "1" as a bean name. */
public class AnonymousClassReflectionTest {
    static abstract class Base {
    }

    private final Base fieldAnon = new Base() { };
    private static final Base staticFieldAnon = new Base() { };

    private static void check(boolean ok, String what) {
        if (!ok) {
            System.out.println("FAILED: " + what);
            System.exit(1);
        }
    }

    private static void checkAnonymous(Class<?> c, String what) {
        check(c.isAnonymousClass(), what + " isAnonymousClass");
        check(c.getSimpleName().isEmpty(), what + " getSimpleName is empty, was '" + c.getSimpleName() + "'");
        check(!c.isMemberClass(), what + " is not a member class");
        check(c.getDeclaringClass() == null, what + " has no declaring class");
        check(c.getEnclosingClass() == AnonymousClassReflectionTest.class, what + " enclosing class");
    }

    private Object instanceMethod() {
        return new Base() { };
    }

    public static void main(String[] args) throws Exception {
        Base local = new Base() { };
        checkAnonymous(local.getClass(), "method-local anonymous class");
        check(local.getClass().getEnclosingMethod() != null &&
              local.getClass().getEnclosingMethod().getName().equals("main"),
              "enclosing method of a method-local anonymous class");
        checkAnonymous(new AnonymousClassReflectionTest().fieldAnon.getClass(), "field anonymous class");
        checkAnonymous(staticFieldAnon.getClass(), "static field anonymous class");
        checkAnonymous(new AnonymousClassReflectionTest().instanceMethod().getClass(), "instance-method anonymous class");

        class Local extends Base {
        }
        Class<?> lc = new Local().getClass();
        check(lc.isLocalClass(), "local class isLocalClass");
        check(!lc.isAnonymousClass(), "local class is not anonymous");
        check(lc.getSimpleName().equals("Local"), "local class simple name");
        check(!lc.isMemberClass(), "local class is not a member class");

        check(Base.class.isMemberClass(), "member class isMemberClass");
        check(Base.class.getSimpleName().equals("Base"), "member class simple name");
        System.out.println("AnonymousClassReflectionTest passed!");
    }
}
