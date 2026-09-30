package spdep;

/* Library half of the -sourcepath tests (see Handler). */
public class Impl implements Session {

    public static Session open() {
        return new Impl();
    }

    @Override
    public void command(Handler handler, String command, String... args) {
        handler.got = "S:" + command + ":" + args.length;
    }

    @Override
    public void command(Handler handler, String command, byte[]... args) {
        handler.got = "B:" + command + ":" + args.length;
    }

    @Override
    public int plain(String s) {
        return s.length();
    }
}
