import narrowconstlib.Frames;

/* Regression test: an int CONSTANT of another class (loaded from a class
 * file, so known only through its ConstantValue attribute) assigned to a
 * narrower type that holds its value - byte, short, char (JLS 5.2) - as a
 * plain assignment, an array element store, or a variable initializer.
 * genesis reported "Incompatible types: cannot convert int to byte". Mirrors
 * gumdrop's H2ParserErrorPathTest: "header[3] = H2FrameHandler.TYPE_DATA;". */
public class NarrowingConstantFromClassfileTest {
    static byte field;

    public static void main(String[] args) {
        byte[] header = new byte[4];
        header[0] = Frames.TYPE_PING;
        header[1] = Frames.FLAG_ACK | Frames.TYPE_PING;
        byte b = Frames.TYPE_PING;
        field = Frames.FLAG_ACK;
        short s = Frames.BIG;
        char c = Frames.TYPE_PING;
        byte viaCast = (byte) Frames.BIG;
        char letter = Frames.LETTER;
        final byte local = Frames.FLAG_ACK;
        byte again = local;
        if (header[0] != 6 || header[1] != 7 || b != 6 || field != 1 || s != 300 || c != 6
                || viaCast != 44 || letter != 'q' || again != 1) {
            throw new RuntimeException("wrong values");
        }
        System.out.println("NarrowingConstantFromClassfileTest passed!");
    }
}
