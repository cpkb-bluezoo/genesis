import module java.logging;

/* import module reads the exports of java.logging from its module-info in the image. */
public class JImageModuleImportTest {
    public static void main(String[] args) {
        Logger.getLogger("jimage").fine("not shown");
        System.out.println("module");
    }
}
