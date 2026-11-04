public class Header {
    private final Payload payload;

    public Header(Payload payload) {
        this.payload = payload;
    }

    public Payload payload() {
        return payload;
    }
}
