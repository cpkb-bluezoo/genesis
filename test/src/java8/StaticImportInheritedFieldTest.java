import static staticimportinherited.Child.CODE;
import static staticimportinherited.Child.*;

public class StaticImportInheritedFieldTest {
    public static void main(String[] args) {
        if (CODE != 7 || !"base".equals(label)) {
            throw new AssertionError(CODE + " " + label);
        }
        System.out.println("StaticImportInheritedFieldTest passed!");
    }
}
