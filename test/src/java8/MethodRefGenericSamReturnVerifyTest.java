/**
 * Regression: method reference on a generic SAM (return type variable bounded by
 * a concrete type) must not specialize the metafactory return to Object.
 */
public class MethodRefGenericSamReturnVerifyTest {

    interface Resp {
    }

    static final class MyResp implements Resp {
    }

    interface HandlerFunction<T extends Resp> {
        T handle(int x);
    }

    static class Service implements HandlerFunction<MyResp> {
        @Override
        public MyResp handle(int x) {
            return new MyResp();
        }
    }

    static void run(HandlerFunction<MyResp> handler) {
        if (handler.handle(0) == null) {
            throw new RuntimeException("null");
        }
    }

    public static void main(String[] args) {
        Service service = new Service();
        run(service::handle);
        System.out.println("PASS");
    }
}
