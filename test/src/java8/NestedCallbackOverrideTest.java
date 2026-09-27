import java.util.List;
import java.util.ArrayList;

class NestedCallbackOverrideTest {

    interface Callback<T> {
        void completed(T result);
        void failed(Throwable error);
    }

    static class Outer {
        interface Callback<T> {
            void completed(T result);
            void failed(Throwable error);
        }
    }

    void nestedAnonymous() {
        new Outer.Callback<String>() {
            @Override
            public void completed(String result) {
            }

            @Override
            public void failed(Throwable exc) {
            }
        };
    }

    void simpleAnonymous() {
        new Callback<List<String>>() {
            @Override
            public void completed(List<String> result) {
            }

            @Override
            public void failed(Throwable exc) {
            }
        };
    }

    public static void main(String[] args) {
        new NestedCallbackOverrideTest().nestedAnonymous();
        new NestedCallbackOverrideTest().simpleAnonymous();
        System.out.println("NestedCallbackOverrideTest OK");
    }
}
