package hiddenlib;
abstract class Hidden<C> {
    protected final String create(String s) { return "c" + s; }
    protected static String st(String s) { return "t" + s; }
    public String open() { return "o"; }
}
