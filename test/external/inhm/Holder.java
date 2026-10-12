package p;

/** Return type is the inherited member type Sub.Mode, not a class Sub$Mode. */
public class Holder {
    private Sub.Mode mode = Sub.Mode.SIMPLE;

    public Sub.Mode getMode() {
        return mode;
    }
}
