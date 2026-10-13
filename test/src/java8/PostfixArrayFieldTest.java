public class PostfixArrayFieldTest {
    private static final String COLUMNS[] = new String[] {"a", "b"};
    private int grid[][] = new int[2][3], single[] = {7}, plain = 5;
    private static String NAMES[], ONE;

    public static void main(String[] args) {
        String[] copy = COLUMNS;
        if (copy.length != 2) throw new AssertionError("columns");
        PostfixArrayFieldTest t = new PostfixArrayFieldTest();
        int[][] g = t.grid;
        if (g.length != 2 || g[0].length != 3) throw new AssertionError("grid");
        int[] s = t.single;
        if (s[0] != 7 || t.plain != 5) throw new AssertionError("single/plain");
        NAMES = new String[] {"x"};
        ONE = "y";
        if (NAMES.length != 1 || !ONE.equals("y")) throw new AssertionError("names");
        System.out.println("ok");
    }
}
