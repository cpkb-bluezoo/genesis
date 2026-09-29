import pkgclasslit.Foo;

public class PackagedClassLiteralSyncVerifyTest {
    public static void main(String[] args) {
        Foo.bump();
        Foo.bump();
        if (Foo.get() != 2) {
            throw new RuntimeException("expected 2, got " + Foo.get());
        }
        System.out.println("PackagedClassLiteralSyncVerifyTest passed!");
    }
}
