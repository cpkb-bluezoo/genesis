package spuser;

import spdep.Handler;
import spdep.Session;

/* Regression test: classes reached only through -sourcepath, imported
 * from ANOTHER package, implemented by a nested class and extended by an
 * anonymous class here. See SamePackageSourcepathTest for the bug. The
 * symptoms in this shape were "cannot convert spuser.ImportSourcepathTest$Stub
 * to spdep.Session" and "cannot convert java.lang.String to <unknown>"
 * for the field assignment. */
public class ImportSourcepathTest {

    static final class Stub implements Session {
        @Override
        public void command(Handler handler, String command, String... args) {
            handler.got = "s" + args.length;
        }

        @Override
        public void command(Handler handler, String command, byte[]... args) {
            handler.got = "b" + args.length;
        }

        @Override
        public int plain(String s) {
            return 1;
        }
    }

    public static void main(String[] args) {
        Session session = new Stub();
        Handler h = new Handler() { };
        session.command(h, "X", "y");
        if (!"s1".equals(h.got) || session.plain("q") != 1) {
            throw new RuntimeException("got " + h.got);
        }
        System.out.println("PASS");
    }
}
