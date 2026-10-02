package nestedouter.lib;

/* An interface with a nested enum - the shape of javax.tools.JavaFileObject
 * and its nested Kind. */
public interface FileObject {

    enum Kind {
        SOURCE(".java"),
        CLASS(".class");

        public final String extension;

        Kind(String extension) {
            this.extension = extension;
        }
    }

    Kind getKind();
}
