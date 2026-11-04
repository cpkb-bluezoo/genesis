/**
 * A builder interface that inherits build() from two parents: MidBuilder
 * returns Mid, and Maker<LeafBuilder, Leaf> returns Leaf, a subclass of Mid.
 * The call is the more specific of the two (JLS 8.4.8.4), so its result can
 * be passed where a Leaf is required. The library is compiled by javac and
 * read as class files.
 */
public class InheritedBuildTest {
    static class Impl implements Store {
        public Attrs read(Leaf request) { return new Attrs(); }
        public Bytes fetch(Leaf request) { return new Bytes(); }
    }

    static Attrs load(Store store, String bucket, String path) {
        Attrs attrs = store.read(Reqs.leaf().bucket(bucket).key(path).build());
        Bytes blob = store.fetch(Reqs.leaf().bucket(bucket).key(path).build());
        return blob == null ? null : attrs;
    }

    public static void main(String[] args) {
        // Reqs.leaf() is null in the library, so load is not called: the call
        // sites need only type-check, and the class must verify.
        Attrs unused = args.length > 0 ? load(new Impl(), "b", "k") : null;
        System.out.println("InheritedBuildTest passed!");
    }
}
