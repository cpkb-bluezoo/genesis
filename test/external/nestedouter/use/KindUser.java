package nestedouter.use;

/* Names the nested type only, by its qualified name: the compiler loads
 * FileObject$Kind without having loaded FileObject. */
public class KindUser {

    public static nestedouter.lib.FileObject.Kind favourite() {
        return nestedouter.lib.FileObject.Kind.CLASS;
    }
}
