import java.net.InetAddress;
import java.util.LinkedHashMap;
import java.util.Map;

/** Varargs @Override matching and LinkedHashMap.removeEldestEntry. */
public class VarargsOverrideTest {
    static class SubListener extends BaseListener {
        @Override
        public SubListener addresses(InetAddress... addrs) {
            return this;
        }
    }

    static class BaseListener {
        public BaseListener addresses(InetAddress... addrs) {
            return this;
        }
    }

    static Map<String, Integer> boundedMap() {
        return new LinkedHashMap<String, Integer>() {
            @Override
            protected boolean removeEldestEntry(Map.Entry<String, Integer> eldest) {
                return size() > 2;
            }
        };
    }

    public static void main(String[] args) throws Exception {
        new SubListener().addresses(InetAddress.getByName("127.0.0.1"));
        boundedMap().put("a", 1);
        System.out.println("VarargsOverrideTest OK");
    }
}
