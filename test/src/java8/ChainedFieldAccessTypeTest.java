/*
 * Regression test: a chained field access (a.b.c, where b and c are both
 * plain instance fields) used to produce a class file that failed JVM
 * bytecode verification whenever the outer field (c) wasn't itself a
 * plain object reference type:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type 'java.lang.Object' ... is not assignable to integer
 *
 * codegen_field_access() (src/codegen_expr.c) has a dedicated branch for a
 * chained access whose receiver is itself an AST_FIELD_ACCESS (as opposed
 * to a bare identifier). It correctly resolves the receiver's own class
 * (so the emitted getfield's *owner* is right), but then hardcoded the
 * FIELD's own descriptor to "Ljava/lang/Object;" unconditionally, never
 * looking at the field's real declared type - so the generated
 * "getfield ... .directory:Ljava/lang/Object;" (should have been "Z" for
 * a boolean field) and "getfield ...:Ljava/lang/Object;" (should have
 * been the real Map type) left the wrong value on the stack wherever the
 * field's actual type mattered: an `ifeq`, an `ireturn`, or any use as
 * something other than a bare Object reference.
 * See genesis history for details (search "The field's own descriptor" in
 * codegen_expr.c).
 */
public class ChainedFieldAccessTypeTest {
    static class Inner {
        final boolean flag;
        Inner(boolean flag) { this.flag = flag; }
    }
    static class Node {
        final boolean directory;
        final java.util.TreeMap<String, Node> children;
        final Inner inner;
        Node(boolean directory, java.util.TreeMap<String, Node> children, Inner inner) {
            this.directory = directory;
            this.children = children;
            this.inner = inner;
        }
    }
    static class Location {
        final Node node;
        Location(Node node) { this.node = node; }
    }

    /* Two-level chain, boolean field, used directly as a return value
     * (matches the minimal repro that first isolated this bug). */
    static boolean isDirectory(Location dst) {
        return dst.node.directory;
    }

    /* Two-level chain, boolean AND a method call on another two-level
     * chain, combined in a short-circuit && inside an if-condition -
     * matches the exact shape of the real gumdrop bug
     * (MemoryFileSystemProvider.prepareTarget). */
    static boolean isEmptyDirectory(Location dst) {
        return dst.node.directory && dst.node.children.isEmpty();
    }

    /* Three-level chain, to confirm the fix isn't limited to exactly two
     * levels of nesting. */
    static boolean isFlagged(Location dst) {
        return dst.node.inner.flag;
    }

    public static void main(String[] args) {
        java.util.TreeMap<String, Node> emptyKids = new java.util.TreeMap<String, Node>();
        Node dirNode = new Node(true, emptyKids, new Inner(true));
        Location loc = new Location(dirNode);

        if (!isDirectory(loc)) {
            throw new RuntimeException("isDirectory failed");
        }
        if (!isEmptyDirectory(loc)) {
            throw new RuntimeException("isEmptyDirectory failed");
        }
        if (!isFlagged(loc)) {
            throw new RuntimeException("isFlagged failed");
        }

        java.util.TreeMap<String, Node> nonEmptyKids = new java.util.TreeMap<String, Node>();
        nonEmptyKids.put("x", dirNode);
        Node dirNode2 = new Node(true, nonEmptyKids, new Inner(false));
        Location loc2 = new Location(dirNode2);
        if (isEmptyDirectory(loc2)) {
            throw new RuntimeException("isEmptyDirectory should be false for non-empty children");
        }
        if (isFlagged(loc2)) {
            throw new RuntimeException("isFlagged should be false");
        }

        System.out.println("All tests passed!");
    }
}
