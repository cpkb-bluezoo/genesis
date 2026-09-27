package java8;

import java.util.List;

/** Uses {@link NestedCallbackHost.Callback} from another compilation unit. */
public class NestedCallbackClient {

    void useCallback() {
        NestedCallbackHost.run(new NestedCallbackHost.Callback<List<String>>() {
            @Override
            public void completed(List<String> result) {
            }

            @Override
            public void failed(Throwable error) {
            }
        });
    }

    public static void main(String[] args) {
        new NestedCallbackClient().useCallback();
        System.out.println("NestedCallbackClient OK");
    }
}
