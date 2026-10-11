import java.util.function.Supplier;

/* Regression test: a functional interface whose default methods share its abstract
 * method's name AND arity but not its parameter types (Spring Boot's
 * DeferredLogFactory: getLog(Supplier<Log>) abstract, getLog(Class) and
 * getLog(Log) default) still has its one abstract method - "Supplier::get" is a
 * DeferredLogFactory. */
interface Logger {
    String name();

    String level();
}

class Named implements Logger {
    private final String name;
    private final String level;

    Named(String name, String level) {
        this.name = name;
        this.level = level;
    }

    public String name() {
        return name;
    }

    public String level() {
        return level;
    }
}

interface LoggerFactory {
    Logger getLog(Supplier<Logger> destination);

    default Logger getLog(Class<?> destination) {
        return getLog(() -> new Named(destination.getSimpleName(), "info"));
    }

    default Logger getLog(Logger destination) {
        return getLog(() -> destination);
    }
}

public class FunctionalInterfaceSameArityDefaultsTest {
    public static void main(String[] args) {
        LoggerFactory viaRef = Supplier::get;
        LoggerFactory viaLambda = (supplier) -> supplier.get();
        Logger fixed = new Named("fixed", "warn");
        if (!"fixed".equals(viaRef.getLog(fixed).name()) || !"String".equals(viaLambda.getLog(String.class).name())
                || !"warn".equals(viaRef.getLog(() -> fixed).level())) {
            throw new RuntimeException("same arity defaults");
        }
        System.out.println("FunctionalInterfaceSameArityDefaultsTest passed!");
    }
}
