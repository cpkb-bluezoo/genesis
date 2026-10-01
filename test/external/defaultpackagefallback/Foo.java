/*
 * Deliberately an UNRELATED class, in the default package, that just
 * happens to share a simple name ("Foo") with a class referenced (but
 * never declared) under a different package below - see
 * UsesNonExistentQualifiedClass.java's own comment for the bug this
 * exposes.
 */
public class Foo {
    public static void unrelatedMethod() {
        System.out.println("unrelated Foo in the default package");
    }
}
