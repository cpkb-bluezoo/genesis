interface QualifiedSuperIface {
    default boolean base() {
        return true;
    }

    default boolean derived() {
        return false;
    }
}

public class QualifiedSuperTest implements QualifiedSuperIface {
    @Override
    public boolean derived() {
        return QualifiedSuperIface.super.base();
    }

    public static void main(String[] args) {
        QualifiedSuperTest t = new QualifiedSuperTest();
        if (!t.derived()) {
            throw new RuntimeException("FAILED");
        }
        System.out.println("QualifiedSuperTest passed!");
    }
}
