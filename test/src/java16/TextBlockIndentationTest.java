/*
 * Regression test: text block translation (JLS 3.10.6): indentation counts characters (a tab is one),
 * the closing delimiter's own line is significant for it, trailing white space of every line is
 * stripped (the closing delimiter line's too, which is what leaves "...\n" and no stray tab), a
 * delimiter on the last content line adds no line terminator, and escapes are interpreted after
 * the stripping.
 */
public class TextBlockIndentationTest {

    static final String CLOSING_DEEPER = """
			first
				second
					""";

    static final String CLOSING_SHALLOWER = """
			first
				second
		""";

    static final String SAME_LINE = """
		one
		two""";

    static final String TRAILING = """
		trailing   \s
		blank:	
		
		end""";

    static final String ESCAPES = """
		a\tb\n"quoted" \""" tail \
		joined \101\60""";

    static final String SPACES = """
        spaces
          indented
        """;

    static void check(String actual, String expected) {
        if (!expected.equals(actual)) {
            throw new RuntimeException("FAIL: [" + actual.replace("\t", "<T>").replace("\n", "<N>") + "]");
        }
    }

    public static void main(String[] args) {
        check(CLOSING_DEEPER, "first\n\tsecond\n");
        check(CLOSING_SHALLOWER, "\tfirst\n\t\tsecond\n");
        check(SAME_LINE, "one\ntwo");
        check(TRAILING, "trailing    \nblank:\n\nend");
        check(ESCAPES, "a\tb\n\"quoted\" \"\"\" tail joined A0");
        check(SPACES, "spaces\n  indented\n");
        System.out.println("PASS");
    }
}
