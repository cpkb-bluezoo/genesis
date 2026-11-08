/* Compiled by genesis: its class file must carry the Signature "Ljava/lang/Enum<LColor;>;" that
 * javac writes for every enum, or javac treats it as a raw Enum when it reads it back. */
public enum Color {
    RED,
    GREEN { @Override public String label() { return "g"; } };

    public String label() { return "c"; }

    public enum Nested { ONE }
}
