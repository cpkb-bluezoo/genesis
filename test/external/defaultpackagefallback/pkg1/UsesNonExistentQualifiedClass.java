package pkg1;

/*
 * Bug: a reference to a qualified class that does NOT exist anywhere
 * ("pkg2.Foo" - there is no pkg2 directory at all under this file's own
 * -sourcepath) compiled and LINKED successfully instead of failing,
 * because load_class_from_source_impl() (semantic.c), when
 * "<sourcepath>/pkg2/Foo.java" didn't exist, fell back to trying just the
 * SIMPLE name ("<sourcepath>/Foo.java") "for default package" - silently
 * picking up the sibling Foo.java in this same test directory (an
 * entirely unrelated, default-package class) and treating IT as if it
 * were "pkg2.Foo". The call below compiled down to a real
 * "invokestatic pkg2/Foo.unrelatedMethod:()V" - a bytecode reference to a
 * class that genuinely does not exist on any classpath, which would
 * throw NoClassDefFoundError at RUNTIME instead of being rejected at
 * COMPILE time, where the real problem actually is. Real javac correctly
 * rejects this at compile time with "error: package pkg2 does not
 * exist". The fallback was dead weight for a genuinely default-package
 * name to begin with (strrchr(name, '.') is NULL there, so "simple_name"
 * already equals "name" - the exact same path already tried and already
 * failed) and only ever had an effect - always a wrong one - for a
 * QUALIFIED name whose real package directory doesn't exist on the
 * sourcepath. Fixed by removing the fallback.
 */
public class UsesNonExistentQualifiedClass {
    public static void main(String[] args) {
        pkg2.Foo.unrelatedMethod();
    }
}
