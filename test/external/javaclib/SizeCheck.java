/* Library for MostSpecificOverloadClassFileTest (built by javac). */
public class SizeCheck {
    public String hasSameSizeAs(Object o) {
        return "Object";
    }

    public String hasSameSizeAs(Iterable<?> o) {
        return "Iterable";
    }
}
