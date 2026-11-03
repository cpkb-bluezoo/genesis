import java.util.Optional;
import valuechainlib.Either;
import valuechainlib.Seq;

/*
 * Regression test: "Either::toJavaOptional", a method Either inherits from
 * Value<R>, produces an Optional of the Either's own R, so a second flatMap
 * in the chain sees a Seq<String>.
 */
public class InheritedMethodRefResultChainTest {
    static Optional<String> first(Either<Exception, Seq<String>> either) {
        return Optional.of(either)
                .flatMap(Either::toJavaOptional)
                .flatMap(seq -> seq.headOption().toJavaOptional());
    }

    public static void main(String[] args) {
        if (!"a".equals(first(Either.<Exception, Seq<String>>right(Seq.of("a", "b"))).orElse("none"))) {
            throw new AssertionError();
        }
        if (!"none".equals(first(Either.<Exception, Seq<String>>right(Seq.<String>of())).orElse("none"))) {
            throw new AssertionError();
        }
        System.out.println("InheritedMethodRefResultChainTest passed!");
    }
}
