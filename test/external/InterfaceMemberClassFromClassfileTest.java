import ifacememberlib.Store;

/* Regression test: a class declared inside an INTERFACE is implicitly public
 * and static (JLS 9.5), so its InnerClasses entry must say so. genesis
 * wrote flags 0 there; a later compile reading the interface from its class
 * file therefore believed the class was an inner (non-static) one and
 * passed an enclosing instance as the first constructor argument:
 * NoSuchMethodError "...$Result.<init>(Store, int, String)". The interface
 * is compiled FIRST and this file against its class file only (-cp), the
 * way gumdrop's test sources see its production classes. */
public class InterfaceMemberClassFromClassfileTest {
    public static void main(String[] args) {
        Store.Result r = new Store.Result(7, "seven");
        if (r.code != 7 || !"seven".equals(r.name)) {
            throw new RuntimeException("wrong result");
        }
        Store s = new Store() {
            @Override
            public Store.Result fetch(String key) {
                return new Store.Result(key.length(), key);
            }
        };
        if (s.fetch("abc").code != 3) {
            throw new RuntimeException("anonymous");
        }
        System.out.println("InterfaceMemberClassFromClassfileTest passed!");
    }
}
