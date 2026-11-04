public class Info {
    private final String ip;

    public Info(String ip) {
        this.ip = ip;
    }

    public Option<String> ip() {
        return Option.some(ip);
    }
}
