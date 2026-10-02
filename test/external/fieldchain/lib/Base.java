package fieldchain.lib;

/* A superclass whose fields are themselves objects with fields. */
public class Base {
    protected final Kind kind = Kind.SOURCE;
    public final Holder holder = new Holder();
    public static final Holder SHARED = new Holder();
}
