import java.util.Optional;

/*
 * The result of a method reference ("Payload::results", an Either of a Seq read
 * from a class file) is the receiver of the next stage's reference
 * ("Either::toJavaOptional", a method Either inherits from Value<R>): the
 * stages after it are typed with the Either's own R, not an unresolved T.
 */
public class MethodRefResultAsReceiverTest {
    static Optional<String> ip(Optional<Header> header) {
        return header.map(Header::payload)
                .map(Payload::results)
                .flatMap(Either::toJavaOptional)
                .flatMap(rows -> rows.headOption().toJavaOptional())
                .flatMap(row -> row.ip().toJavaOptional());
    }

    public static void main(String[] args) {
        String got = ip(Optional.of(new Header(new Payload(new Info("10.0.0.1"))))).get();
        if (!got.equals("10.0.0.1")) {
            throw new AssertionError(got);
        }
        if (ip(Optional.empty()).isPresent()) {
            throw new AssertionError("present");
        }
        System.out.println("MethodRefResultAsReceiverTest passed!");
    }
}
