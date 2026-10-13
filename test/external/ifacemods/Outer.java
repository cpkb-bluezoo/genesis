/* A nested interface's members, as the compiler registers them: methods public and abstract (unless
 * static, default or private), fields public static final, in every registration of the same
 * interface, whatever else is written on them. */
public class Outer {
    public interface Callback {
        void done(String s);

        int count();

        default int twice() {
            return 2;
        }

        static Callback none() {
            return null;
        }

        private int hidden() {
            return 1;
        }

        int LIMIT = 4;
    }

    public @interface Marker {
        int value();
    }
}
