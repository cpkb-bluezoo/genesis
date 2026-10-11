package p;

import java.util.function.Supplier;

public class DefaultCtorRefTest {
    static <T> T make(Class<T> type, Supplier<T> supplier) {
        return supplier.get();
    }

    public static void main(String[] args) {
        Conv c = make(Conv.class, Conv::new);
        Supplier<Conv> s = Conv::new;
        if (!"conv".equals(c.name()) || !"conv".equals(s.get().name())) {
            throw new RuntimeException("Conv::new");
        }
    }
}
