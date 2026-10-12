import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

/** Regression: ArrayList::new as Supplier (spring-boot ArrayBinder). */
public class SupplierArrayListNewVerifyTest {

    static final class Box {
        final List<?> list;

        Box(Supplier<List<?>> supplier) {
            this.list = supplier.get();
        }
    }

    public static void main(String[] args) {
        Box b = new Box(ArrayList::new);
        if (b.list == null || b.list.size() != 0) {
            throw new RuntimeException("expected empty list");
        }
        System.out.println("PASS");
    }
}
