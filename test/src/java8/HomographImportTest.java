import homographimport.app.HomographImportApp;

public class HomographImportTest {
    public static void main(String[] args) {
        if (!HomographImportApp.create().isHolding()) throw new AssertionError();
        System.out.println("ok");
    }
}
