package nestedouter.lib;

/* An interface whose nested interface extends it - the shape of
 * jakarta.servlet.ServletRegistration and its nested Dynamic. */
public interface Registration {

    String describe(String... parts);

    interface Dynamic extends Registration {
        void flag(boolean on);
    }
}
