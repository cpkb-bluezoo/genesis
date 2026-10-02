package nestedouter.lib;

public class SimpleFileObject implements FileObject {

    protected final Kind kind;

    protected SimpleFileObject(Kind kind) {
        this.kind = kind;
    }

    public Kind getKind() {
        return kind;
    }
}
