package spdep;

/* Regression test: classes reached only through -sourcepath, from a file
 * in the SAME package (so with no import naming them). This is the only
 * file given to the compiler; Handler, Session and Impl live under a
 * separate -sourcepath root.
 *
 * Two loader functions shared one "currently loading" guard table keyed
 * by class name. The outer one (load_external_class) marked the name and
 * then fell back to the source loader (load_class_from_source) for that
 * same name, which saw the marker, took it for a reentrant load and gave
 * up - so nothing was ever loaded from -sourcepath: "Cannot resolve
 * symbol: Impl", "cannot convert <unknown> to spdep.Session", and calls
 * compiled from a guessed descriptor.
 *
 * It must also RUN from the output directory alone: the dependencies
 * have to be compiled to class files too, as javac does, and all of
 * them, not just the last one loaded. */
public class SamePackageSourcepathTest {

    static int run(Session session, Handler h) {
        session.command(h, "CONFIG", "GET", "maxmemory");
        return session.plain("abc");
    }

    public static void main(String[] args) {
        Session s = Impl.open();
        Handler h = new Handler();
        int n = run(s, h);
        if (!"S:CONFIG:2".equals(h.got) || n != 3) {
            throw new RuntimeException("got " + h.got + " " + n);
        }
        s.command(h, "SET", new byte[] { 1 }, new byte[] { 2 });
        if (!"B:SET:2".equals(h.got)) {
            throw new RuntimeException("got " + h.got);
        }
        System.out.println("PASS");
    }
}
