public class GetClassReturnTest {
    void implicitThis() {
        Class<?> c = getClass();
        if (c == null) {
            throw new RuntimeException("null class");
        }
    }

    void explicitObject() {
        Object o = "hi";
        Class<?> c = o.getClass();
        if (c == null) {
            throw new RuntimeException("null class");
        }
    }

    public static void main(String[] args) {
        GetClassReturnTest t = new GetClassReturnTest();
        t.implicitThis();
        t.explicitObject();
        System.out.println("GetClassReturnTest passed");
    }
}
