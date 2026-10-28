import java.util.HashMap;
import java.util.Map;

public class MapForEachBoundMethodRefTest {
    static final class MapReduceLog {
        static MapReduceLog newInstance(String name) {
            return new MapReduceLog(name);
        }

        private final String name;

        MapReduceLog(String name) {
            this.name = name;
        }

        void add(String key, String value) {
            if (key == null || value == null) {
                throw new AssertionError();
            }
        }

        void log() {}
    }

    static void logStats(Map<String, String> data) {
        var logger = MapReduceLog.newInstance("EXPORT");
        data.forEach(logger::add);
        logger.log();
    }

    public static void main(String[] args) {
        Map<String, String> data = new HashMap<>();
        data.put("k", "v");
        logStats(data);
        System.out.println("MapForEachBoundMethodRefTest passed!");
    }
}
