public class Payload {
    private final Info info;

    public Payload(Info info) {
        this.info = info;
    }

    public Either<None, Seq<Info>> results() {
        return Either.right(Seq.of(info));
    }
}
