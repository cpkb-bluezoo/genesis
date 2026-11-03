/*
 * A case of a switch with pattern labels that runs on into the next one is
 * rejected: that fall-through is not supported.
 */
public class PatternSwitchFallThrough {
    static void f(Object o) {
        switch (o) {
            case String s:
                System.out.println(s);
            default:
                System.out.println("other");
        }
    }
}
