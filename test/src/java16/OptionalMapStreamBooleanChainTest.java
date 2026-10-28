import java.util.List;
import java.util.Optional;

/**
 * Outer {@code Optional.map} with a block lambda that returns a boolean expression
 * built from a stream pipeline ending in {@code orElse(boolean)}.
 */
public class OptionalMapStreamBooleanChainTest {
    static final class Link {
        private final boolean enabled;

        Link(boolean enabled) {
            this.enabled = enabled;
        }

        boolean isEnabled() {
            return enabled;
        }
    }

    static Optional<Link> linkFor(long id) {
        return Optional.of(new Link(id == 1L));
    }

    static boolean isEnabled(Optional<Holder> holderOpt, long notificationType) {
        return holderOpt
                .map(holder -> {
                    List<Long> disableable = holder.disableableIds();
                    return disableable.stream()
                            .filter(id -> id.equals(notificationType))
                            .map(id -> linkFor(id))
                            .filter(Optional::isPresent)
                            .findFirst()
                            .map(Optional::get)
                            .map(Link::isEnabled)
                            .orElse(true);
                })
                .orElse(true);
    }

    public static void main(String[] args) {
        Holder h = new Holder(List.of(1L, 2L));
        if (!isEnabled(Optional.of(h), 1L)) {
            throw new AssertionError();
        }
        System.out.println("OptionalMapStreamBooleanChainTest passed!");
    }

    record Holder(List<Long> disableableIds) {}
}
