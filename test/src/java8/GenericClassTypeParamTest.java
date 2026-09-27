/** Class type parameter in method return and argument. */
public class GenericClassTypeParamTest<T> {
    T id(T value) {
        return value;
    }

    static long fromBox(GenericClassTypeParamTest<Long> box, Long n) {
        Long out = box.id(n);
        return out.longValue();
    }

    public static void main(String[] args) {
        GenericClassTypeParamTest<Long> box = new GenericClassTypeParamTest<>();
        System.out.println("GenericClassTypeParamTest OK " + fromBox(box, 7L));
    }
}
