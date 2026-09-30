/* Library half of ExternalVarargsFlagTest: ABSTRACT-class varargs methods,
 * which take a separate code path in genesis from interface methods.
 *
 * Both use a class element type on purpose: overriding a cross-file
 * method whose varargs element type is a primitive or an array is a
 * separate genesis problem that would stop this library compiling. */
public abstract class VarargsBase implements VarargsSession {

    public abstract String join(int n, String... parts);

    public abstract int size(Object... items);
}
