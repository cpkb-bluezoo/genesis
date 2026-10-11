package p;

public class VarargsReturnTest {
    private final String[] properties = {"no.such.property", "user.dir"};

    public String value() {
        return SP.get(this.properties);
    }

    public static void main(String[] args) {
        String v = new VarargsReturnTest().value();
        if (v == null || SP.get("no.such.property") != null) {
            throw new RuntimeException("varargs return");
        }
    }
}
