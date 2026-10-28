package protectedstaticfieldlib;

/*
 * Compiled with javac (see run-tests.sh): a base class from a dependency
 * with a "protected static final" field, as logging and statistics base
 * classes commonly have.
 */
public abstract class BaseStats {
    public interface Log {
        void error(String message);
    }

    protected static final Log log = message -> {
        throw new IllegalStateException(message);
    };

    protected final String statsType;

    protected BaseStats(String statsType) {
        this.statsType = statsType;
    }

    protected String getStatsType() {
        return statsType;
    }
}
