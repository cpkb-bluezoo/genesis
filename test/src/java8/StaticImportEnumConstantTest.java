import static staticimportenum.Kind.ALPHA;
import static staticimportenum.Kind.*;

import java.util.ArrayList;

public class StaticImportEnumConstantTest {
    static ArrayList<String> names(long id) {
        return new ArrayList<String>();
    }

    static boolean enabled(long id) {
        return id > 0;
    }

    public static void main(String[] args) {
        ArrayList<String> rows = names(ALPHA.getId());
        if (!enabled(ALPHA.getId()) || !rows.isEmpty() || BETA.getId() != 2L) {
            throw new AssertionError("static-imported enum constant");
        }
        System.out.println("StaticImportEnumConstantTest passed!");
    }
}
