package p;

/**
 * Analyzed before Holder.java. Holder.getMode()'s return type is written
 * Sub.Mode, which is Base.Mode. The call and the constructor must both
 * name Base$Mode.
 */
public class Use {
    public static Sub make(Holder h) {
        return new Sub(h.getMode());
    }

    public static void main(String[] args) {
        Sub s = make(new Holder());
        if (s.mode != Base.Mode.SIMPLE) {
            throw new AssertionError(String.valueOf(s.mode));
        }
        System.out.println("PASS");
    }
}
