/* A local that is not final is no constant: no narrowing, with or without boxing. */
public class NarrowingFromNonFinalLocal {
    public static void main(String[] args) {
        int k = 1;
        Byte b = k;
    }
}
