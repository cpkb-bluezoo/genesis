import static nestedenum.Labels.Definition.*;

public class StaticStarImportNestedTypeTest {
    public static void main(String[] args) {
        if (!"FILE_NAME".equals(ValidValues.FILE_NAME.getValue())) {
            throw new AssertionError("enum constant via star-imported type");
        }
        ValidValues v = ValidValues.valueOf("FILE_NAME");
        if (v != ValidValues.FILE_NAME) {
            throw new AssertionError("star-imported type as a declared type");
        }
        System.out.println("ok");
    }
}
