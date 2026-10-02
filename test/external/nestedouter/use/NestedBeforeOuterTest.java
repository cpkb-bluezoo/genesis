package nestedouter.use;

import nestedouter.lib.SimpleFileObject;

/**
 * A nested type, loaded from a class file, that the compiler has already
 * met on its own (in KindUser, compiled just before this file in the same
 * batch) by the time it loads the outer type: it must still be found as a
 * member of the outer type, by its simple name, through a class that
 * inherits it.
 */
public class NestedBeforeOuterTest {

    static class Source extends SimpleFileObject {
        Source() {
            /* Kind is a member type inherited from FileObject */
            super(Kind.SOURCE);
        }

        String extension() {
            return Kind.CLASS.extension;
        }
    }

    public static void main(String[] args) {
        Source source = new Source();
        if (source.getKind() != nestedouter.lib.FileObject.Kind.SOURCE ||
            !".class".equals(source.extension()) ||
            KindUser.favourite() != nestedouter.lib.FileObject.Kind.CLASS) {
            throw new AssertionError(source.getKind() + " " + source.extension());
        }
        System.out.println("NestedBeforeOuterTest passed");
    }
}
