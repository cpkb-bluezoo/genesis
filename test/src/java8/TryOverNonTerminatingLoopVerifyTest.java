import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;

/* Regression test: a method whose whole body is a try whose block cannot
 * complete normally (an endless "while (true)" that only returns), with a
 * catch that throws. A jump past the handler was emitted after the loop and
 * targeted the end of the code, where there is no instruction ("Expecting a
 * stackmap frame at branch target"). Mirrors gumdrop's
 * FileHandler.PropfindWalker.advance. */
public class TryOverNonTerminatingLoopVerifyTest {
    private final List<String> items = new ArrayList<>();
    private int pos;

    String advance() {
        try {
            if (pos == 0) {
                pos++;
                return "root";
            }
            while (true) {
                if (pos > items.size()) {
                    return null;
                }
                String s = items.get(pos - 1);
                pos++;
                if (s.startsWith(".")) {
                    continue;
                }
                return s;
            }
        } catch (IndexOutOfBoundsException e) {
            throw new NoSuchElementException(e.getMessage());
        }
    }

    int count(int limit) {
        try {
            for (int i = 0; ; i++) {
                if (i >= limit) {
                    return i;
                }
            }
        } finally {
            pos = -1;
        }
    }

    public static void main(String[] args) {
        TryOverNonTerminatingLoopVerifyTest t = new TryOverNonTerminatingLoopVerifyTest();
        t.items.add(".hidden");
        t.items.add("a");
        StringBuilder sb = new StringBuilder();
        String s;
        while ((s = t.advance()) != null) {
            sb.append(s).append(',');
        }
        if (!"root,a,".equals(sb.toString())) {
            throw new RuntimeException(sb.toString());
        }
        if (t.count(5) != 5 || t.pos != -1) {
            throw new RuntimeException("count");
        }
        System.out.println("TryOverNonTerminatingLoopVerifyTest passed!");
    }
}
