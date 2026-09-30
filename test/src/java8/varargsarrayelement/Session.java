package varargsarrayelement;

/* Helper for VarargsArrayElementVerifyTest: two overloads differing only
 * in the element type of their varargs parameter, one of which is itself
 * an array. Mirrors gumdrop's RedisSession.command(...) pair. */
public interface Session {

    String command(String handler, String command, String... args);

    String command(String handler, String command, byte[]... args);

    int pieces(byte[]... parts);
}
