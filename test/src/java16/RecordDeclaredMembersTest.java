import java.util.List;

/* Regression test for records: members the record declares itself replace the
 * generated ones (a second accessor, toString, equals or hashCode was
 * generated and the class failed to load: "Duplicate method"), a compact
 * canonical constructor (it did not parse), static members used through the
 * record's name, and the implicit canonical constructor next to another
 * constructor. */
public class RecordDeclaredMembersTest {
    record Range(int low, int high) {
        static final int MAX = 100;

        Range {
            if (low > high) {
                throw new IllegalArgumentException(low + ">" + high);
            }
            high = Math.min(high, MAX);
        }

        Range(int single) {
            this(single, single);
        }

        static Range of(int low, int high) {
            return new Range(low, high);
        }

        @Override
        public int low() {
            return low;
        }

        @Override
        public String toString() {
            return "[" + low + ".." + high + "]";
        }

        @Override
        public boolean equals(Object o) {
            if (!(o instanceof Range)) {
                return false;
            }
            Range r = (Range) o;
            return r.low == low && r.high == high;
        }

        @Override
        public int hashCode() {
            return 31 * low + high;
        }
    }

    record Tagged<T>(T value, List<String> tags) {
        Tagged(T value) {
            this(value, List.of());
        }
    }

    public static void main(String[] args) {
        Range r = Range.of(2, 500);
        if (!"[2..100]".equals(r.toString()) || r.low() != 2 || r.high() != 100 || Range.MAX != 100) {
            throw new RuntimeException(r.toString());
        }
        if (!r.equals(new Range(2, 100)) || r.hashCode() != 31 * 2 + 100 || !new Range(7).equals(Range.of(7, 7))) {
            throw new RuntimeException("equals/hashCode");
        }
        try {
            Range.of(5, 1);
            throw new RuntimeException("compact constructor check");
        } catch (IllegalArgumentException expected) {
            if (!"5>1".equals(expected.getMessage())) {
                throw new RuntimeException(expected.getMessage());
            }
        }
        Tagged<String> t = new Tagged<>("v");
        Tagged<Integer> u = new Tagged<>(1, List.of("a"));
        if (!"v".equals(t.value()) || !t.tags().isEmpty() || u.tags().size() != 1 || u.value() != 1) {
            throw new RuntimeException("generic record");
        }
        System.out.println("RecordDeclaredMembersTest passed!");
    }
}
