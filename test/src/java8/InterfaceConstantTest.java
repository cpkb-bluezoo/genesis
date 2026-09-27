interface ConstProvider {
    int ANSWER = 42;
    String LABEL = "ok";
}

class InterfaceConstantUser implements ConstProvider {
    int value() {
        return ANSWER;
    }

    String text() {
        return LABEL;
    }
}

public class InterfaceConstantTest {
    public static void main(String[] args) {
        InterfaceConstantUser u = new InterfaceConstantUser();
        if (u.value() != 42 || !"ok".equals(u.text())) {
            throw new AssertionError("interface constant lookup failed");
        }
        System.out.println("InterfaceConstantTest OK");
    }
}
