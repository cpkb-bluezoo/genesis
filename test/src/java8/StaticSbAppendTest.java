public class StaticSbAppendTest {
    enum DnsType { A, NS }

    static final class Record {
        DnsType getType() {
            return DnsType.A;
        }
    }

    static String build() {
        StringBuilder sb = new StringBuilder();
        sb.append("hello");
        sb.append(' ').append("world");
        return sb.toString();
    }

    static String formatRecord(Record rr) {
        StringBuilder sb = new StringBuilder();
        sb.append(" IN ").append(rr.getType().name());
        switch (rr.getType()) {
            case A:
                sb.append(' ').append("addr");
                break;
            case NS:
                sb.append(' ').append("ns");
                break;
            default:
                throw new IllegalArgumentException("bad: " + rr.getType());
        }
        return sb.toString();
    }

    public static void main(String[] args) {
        if (!"hello world".equals(build())) {
            throw new RuntimeException("FAILED: " + build());
        }
        if (!" IN A addr".equals(formatRecord(new Record()))) {
            throw new RuntimeException("FAILED format: " + formatRecord(new Record()));
        }
        System.out.println("StaticSbAppendTest passed!");
    }
}
