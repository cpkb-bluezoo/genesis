package spdep;

/* Library half of the -sourcepath tests (see Handler). */
public interface Session {

    void command(Handler handler, String command, String... args);

    void command(Handler handler, String command, byte[]... args);

    int plain(String s);
}
