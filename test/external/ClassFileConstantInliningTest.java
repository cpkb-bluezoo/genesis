import static constlib.Constants.NAME;

import constlib.Constants;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Set;

/* The constants of a class read from a class file are compiled into their uses (JLS 4.12.4): this
 * is run WITHOUT that class on the class path - and its static initializer would throw. */
public class ClassFileConstantInliningTest {
    static final Set<String> NAMES = new LinkedHashSet<>(Arrays.asList(Constants.NAME, NAME + "x"));

    public static void main(String[] args) {
        String actual = NAMES + "" + Constants.COUNT + Constants.BIG + Constants.FLAG + Constants.LETTER
            + Constants.RATIO + Constants.SMALL + (Constants.COUNT + 1);
        if (!"[n, nx]57truec1.50.256".equals(actual)) {
            throw new AssertionError(actual);
        }
        System.out.println("PASS");
    }
}
