/** Implicit values() inside enum static method. */
public enum EnumValuesCallTest {
    A, B;

    static int count() {
        return values().length;
    }

    public static void main(String[] args) {
        System.out.println("EnumValuesCallTest OK " + count());
    }
}
