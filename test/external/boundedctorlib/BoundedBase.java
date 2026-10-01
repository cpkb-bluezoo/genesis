package boundedctorlib;

/*
 * Deliberately compiled with REAL javac (see run-tests.sh) - mirrors
 * javax.tools.ForwardingJavaFileManager<M extends JavaFileManager>'s own
 * constructor shape exactly, and the bug this supports testing is
 * specifically about a REAL, javac-compiled classfile's bounded type
 * variable constructor parameter (genesis compiling this same class itself
 * does not reproduce it - genesis's own classfile writer/loader round-trips
 * enough information between the two that the gap this test is for never
 * opens up; only a classfile from an independent, real javac compilation
 * does, exactly like the actual javax.tools classes gumdrop depends on).
 */
public class BoundedBase<T extends CharSequence> {
    protected final T value;

    protected BoundedBase(T value) {
        this.value = value;
    }
}
