/*
 * An abstract or interface method's own declared parameter type node for
 * a varargs parameter ("String... keys") describes only the ELEMENT type
 * ("String") - its real parameter type has one more array dimension on
 * top. codegen_interface_method() and codegen_abstract_method()
 * (codegen.c) built the method's own compiled descriptor straight from
 * that element-type node, with no MOD_VARARGS check at all (unlike the
 * concrete-method parameter loop in codegen_method(), which already
 * detects this and switches to TYPE_ARRAY). The call site (correctly)
 * built an invokeinterface/invokevirtual reference expecting
 * "(...[Ljava/lang/String;)V", but the interface/abstract method's own
 * declared signature never matched it: NoSuchMethodError at runtime.
 *
 * Confirmed against gumdrop's own RedisSession.xread(int, long,
 * ArrayResultHandler, String... keysAndIds).
 */
public class InterfaceVarargsVerifyTest {
    interface Handler {
        void handle(String[] arr);
    }

    interface Session {
        void xread(int count, long blockMillis, Handler handler, String... keysAndIds);
    }

    static class SessionImpl implements Session {
        @Override
        public void xread(int count, long blockMillis, Handler handler, String... keysAndIds) {
            handler.handle(keysAndIds);
        }
    }

    public static void main(String[] args) {
        Session session = new SessionImpl();
        session.xread(5, -1, new Handler() {
            @Override
            public void handle(String[] arr) {
                if (arr.length != 2 || !arr[0].equals("stream1") || !arr[1].equals("0")) {
                    throw new RuntimeException("bad args: " + java.util.Arrays.toString(arr));
                }
            }
        }, "stream1", "0");
        System.out.println("InterfaceVarargsVerifyTest passed!");
    }
}
