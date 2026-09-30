/* Library half of ExternalVarargsFlagTest: an INTERFACE whose methods are
 * overloaded on the element type of a trailing varargs parameter. Mirrors
 * gumdrop's RedisSession.command(ArrayResultHandler, String, String...)
 * and command(ArrayResultHandler, String, byte[]...). */
public interface VarargsSession {

    String command(String handler, String command, String... args);

    String command(String handler, String command, byte[]... args);

    int count(long scale, int... values);
}
