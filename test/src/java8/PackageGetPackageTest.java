public class PackageGetPackageTest {
    String version() {
        java.lang.Class<?> c = getClass();
        java.lang.Package pkg = c.getPackage();
        if (pkg != null && pkg.getImplementationVersion() != null) {
            return pkg.getImplementationVersion();
        }
        return "1.0";
    }

    public static void main(String[] args) {
        System.out.println("PackageGetPackageTest: " + new PackageGetPackageTest().version());
    }
}
