import java.util.function.Function;

/* Regression test: an inner class names its outer class's type parameter in its
 * implements clause and member signatures ("class LenientToEnumConverter<E>
 * implements Converter<T, E>" and "E convert(T source)" in Spring Boot's
 * LenientObjectToEnumConverterFactory<T>). */
public class InnerClassOuterTypeParameterTest<T> {
    private class Conv<E extends Enum<E>> implements Function<T, E> {
        private final Class<E> type;

        Conv(Class<E> type) {
            this.type = type;
        }

        @Override
        public E apply(T source) {
            return Enum.valueOf(type, source.toString().trim().toUpperCase());
        }

        T same(T t) {
            return t;
        }
    }

    enum Color {
        RED, GREEN
    }

    <E extends Enum<E>> Function<T, E> converter(Class<E> type) {
        return new Conv<>(type);
    }

    public static void main(String[] args) {
        Function<String, Color> c = new InnerClassOuterTypeParameterTest<String>().converter(Color.class);
        if (c.apply(" green ") != Color.GREEN) {
            throw new RuntimeException("inner converter");
        }
        System.out.println("InnerClassOuterTypeParameterTest passed!");
    }
}
