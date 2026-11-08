import lombok.Getter;
import lombok.Setter;

/* Nothing here needs the members Lombok would generate, so the source compiles as written. */
@Getter
@Setter
class LombokMarkerOnly {
    private String name;
    private boolean enabled = true;
}
