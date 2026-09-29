package inheritedmembertype.base;

/* Helper for InheritedMemberTypeVerifyTest: declares the member types
 * (Handler, Options) that the subclasses in the OTHER package inherit
 * and refer to by simple name only. Mirrors gumdrop's ByteStreamLexer,
 * which declares `public interface Handler<T>` as a nested member. */
public abstract class Base<T> {

    public interface Handler<T> {
        boolean token(T type, int n);
    }

    public static class Options {
        public final int limit;

        public Options(int limit) {
            this.limit = limit;
        }
    }

    protected final Handler<T> handler;
    protected final int max;

    protected Base(Handler<T> handler, int max) {
        this.handler = handler;
        this.max = max;
    }
}
