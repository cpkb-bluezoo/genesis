import static nestedenum.Labels.Definition.ValidValues;

public class StaticImportNestedTypeTest {
    public static void main(String[] args) {
        if (!"FILE_NAME".equals(ValidValues.FILE_NAME.getValue())) {
            throw new AssertionError("enum constant via imported type");
        }
        ValidValues v = ValidValues.valueOf("FILE_NAME");
        if (v != ValidValues.FILE_NAME) {
            throw new AssertionError("imported type as a declared type");
        }
        System.out.println("ok");
    }
}
