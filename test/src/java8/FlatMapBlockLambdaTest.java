import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/*
 * A block lambda passed to flatMap returns a result parameterized by the method's own variable
 * ("Optional<? extends U>", "Stream<? extends R>"): what it returns says what U is, as it does
 * for an expression lambda, so the stage after it sees the right element type.
 */
public class FlatMapBlockLambdaTest {
    static String address(int port) {
        return "host:" + port;
    }

    public static void main(String[] args) {
        Optional<String> viaBlock = Optional.of("8080")
                .flatMap(text -> {
                    Integer port = Integer.valueOf(text);
                    return Optional.of(port);
                })
                .map(port -> address(port));
        if (!viaBlock.get().equals("host:8080")) {
            throw new AssertionError(viaBlock);
        }

        Optional<String> viaExpression = Optional.of("1")
                .flatMap(text -> Optional.of(Integer.valueOf(text)))
                .map(port -> address(port));
        if (!viaExpression.get().equals("host:1")) {
            throw new AssertionError(viaExpression);
        }

        List<String> viaStream = Stream.of("1,2", "3")
                .flatMap(text -> {
                    String[] parts = text.split(",");
                    return Arrays.stream(parts).map(Integer::valueOf);
                })
                .map(n -> address(n))
                .collect(Collectors.toList());
        if (!viaStream.equals(Arrays.asList("host:1", "host:2", "host:3"))) {
            throw new AssertionError(viaStream);
        }

        // the first return is an Optional.empty() whose own variable nothing fixes: the later
        // return says what U is
        Optional<String> emptyFirst = Optional.of("").flatMap(text -> {
                    if (text.isEmpty()) {
                        return Optional.empty();
                    }
                    return Optional.of(Integer.valueOf(text));
                })
                .map(port -> address(port));
        if (emptyFirst.isPresent()) {
            throw new AssertionError(emptyFirst);
        }
        Optional<String> emptyLast = Optional.of("7").flatMap(text -> {
                    if (!text.isEmpty()) {
                        return Optional.of(Integer.valueOf(text));
                    }
                    return Optional.empty();
                })
                .map(port -> address(port));
        if (!emptyLast.get().equals("host:7")) {
            throw new AssertionError(emptyLast);
        }
        System.out.println("FlatMapBlockLambdaTest passed!");
    }
}
