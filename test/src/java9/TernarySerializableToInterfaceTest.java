import java.io.Serializable;

public class TernarySerializableToInterfaceTest {
    interface DataMap extends Serializable {
        String get(String key);
    }

    interface Client {
        DataMap fetch(String id);
    }

    static class Request implements DataMap {
        public String get(String key) {
            return key;
        }
    }

    static void update(Client client, String id, DataMap fallback) {
        DataMap dao = id != null ? client.fetch(id) : fallback;
        dao.get("x");
    }

    public static void main(String[] args) {
        Client client = id -> new Request();
        update(client, "1", new Request());
        update(client, null, new Request());
        System.out.println("TernarySerializableToInterfaceTest passed!");
    }
}
