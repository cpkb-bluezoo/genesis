import static nestedenum.Labels.Holder.LIMIT;
import static nestedenum.Labels.Nested.FAIL;
import static nestedenum.Labels.Nested.OK;

import nestedenum.Labels;

public class StaticImportNestedEnumTest {
    public static void main(String[] args) {
        if (OK == FAIL || OK != Labels.Nested.OK) {
            throw new AssertionError("enum constants");
        }
        if (FAIL != Labels.Nested.FAIL || LIMIT != 9) {
            throw new AssertionError("values");
        }
        System.out.println("ok");
    }
}
